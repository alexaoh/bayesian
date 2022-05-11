# Implementation of second model: Simple Bimodal Gaussian.

# perhaps interesting read: https://towardsdatascience.com/gaussian-mixture-models-explained-6986aaf5a95

library(rstan)
library(bayesplot)
library(tidyverse)
library(Hmisc)
library(ggplot2)

# Read the data that was cleaned in "erasmus.R".
data <- read.csv("cleaned.csv")
summary(data)
dim(data)

# Change the data types again. 
data <- data %>% transmute(duration = as.numeric(duration),
                           age = as.numeric(age),
                           gender = as.factor(gender),
                           nationality = as.factor(nationality),
                           sending.country = as.factor(sending.country),
                           receiving.country = as.factor(receiving.country),
                           activity = as.factor(activity),
                           participants = participants)

describe(data)

# We want to model the duration of the student exchange. 
dur <- data$duration 
any(is.na(dur)) # Checking to be sure that there are no NA here. 

# We simulate values from the posterior distribution using Stan. 
# Define model and call stan. 

points <- 50000
data_list <- list(
  n=points,
  y=sample(dur, size = points) # Tar bort noe data for å teste modellen, for det tar veldig lang tid med all dataen!
  # Vet ikke helt hva vi burde gjøre for å løse dette problemet!? Nå har jeg samplet fra dataen!
)

# Prøver å følge følgende guider for å få til Stan-koden:
# https://mc-stan.org/docs/2_29/stan-users-guide/summing-out-the-responsibility-parameter.html
# https://mc-stan.org/docs/2_29/stan-users-guide/vectorizing-mixtures.html
# https://mc-stan.org/users/documentation/case-studies/identifying_mixture_models.html
#### DEN SISTE ARTIKKELEN VIRKER Å VÆRE RELATIVT VIKTIG! SAMMENHENG MED reassignation problem (tror jeg) som er nevnt
# i den ene oppgaven som er lagt ut fra tidligere i Bayesian angående mixture models!

fit <- stan("stan_models/model_one.stan", iter = 1000, chains = 4,
            data = data_list, seed = 1)

# Convergence analysis.
print(fit)
traceplot(fit)

# Annen måte å plotte chainsene på. 
# https://mc-stan.org/users/documentation/case-studies/identifying_mixture_models.html
# Lånt fra lenken: kan være grei å bruke for å sjekke at alt er greit underveis også!
chains1 <- as.data.frame(rstan::extract(fit, permuted=FALSE)[,1,])
chains2 <- as.data.frame(rstan::extract(fit, permuted=FALSE)[,2,])
chains3 <- as.data.frame(rstan::extract(fit, permuted=FALSE)[,3,])
chains4 <- as.data.frame(rstan::extract(fit, permuted=FALSE)[,4,])

par(mar = c(4, 4, 1.5, 1))
plot(chains1$mu1, chains1$mu2, col="black", pch=16, cex=0.8,
     xlab="mu1", ylab="mu2", xlim = c(130, 140), ylim = c(270, 300), 
     main = "Chains for mu1 and mu2 Plotted in Two Dimensions")
points(chains2$mu1, chains2$mu2, col="red", pch=16, cex=0.8, alpha = 0.5)
points(chains3$mu1, chains3$mu2, col="yellow", pch=16, cex=0.8, alpha = 0.4)
points(chains4$mu1, chains4$mu2, col="blue", pch=16, cex=0.8, alpha = 0.3)
#lines(0.08*(1:100) - 4, 0.08*(1:100) - 4, col="grey", lw=2)
legend("topright", c("Chain 1", "Chain 2", "Chain 3", "Chain 4"),
       fill=c("black", "red",
              "yellow", "blue"), box.lty=0, inset=0.0005)

posterior <- as.data.frame(fit)
head(posterior)          
dim(posterior)

par(mfrow = c(2, 2))
acf(posterior$p)
acf(posterior$sigma1)
acf(posterioe$sigma2)
acf(posterior$mu1)
acf(posterior$mu2)
par(mfrow = c(1,1))

mcmc_areas(posterior %>% select(p)) + ggtitle("Posterior distribution of p")

plot_title <- ggtitle("Posterior distributions of mu", "with medians and 80% intervals")
mcmc_areas(posterior %>% select(c(mu1,mu2)), 
           pars = c("mu1", "mu2"), 
           prob = 0.8) + plot_title

plot_title <- ggtitle("Posterior distributions of sigma", "with medians and 80% intervals")
mcmc_areas(posterior %>% select(c(sigma1, sigma2)), 
           pars = c("sigma1", "sigma2"), 
           prob = 0.8) + plot_title

# Prøve å plotte posterior mixture model:
p.mean <- mean(posterior$p)
mu1.mean <- mean(posterior$mu1)
mu2.mean <- mean(posterior$mu2)
sigma1.mean <- mean(posterior$sigma1)
sigma2.mean <- mean(posterior$sigma2)

N <- 100000
components <- sample(1:2,prob=c(p.mean,1-p.mean),size=N,replace=TRUE)
mus <- c(mu1.mean,mu2.mean)
sds <- c(sigma1.mean,sigma2.mean) 

samples <- rnorm(N)*sds[components]+mus[components]

tibble(samples) %>% 
  ggplot(aes(samples)) +
  geom_density(aes(y = (..count..)/sum(..count..))) +
  ggtitle("Mix of Gaussian")
# Vi ser at den ikke er helt forferdelig!
