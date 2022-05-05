# Implementation of third model: Hierarchical Bimodal Gaussian.

library(rstan)
library(bayesplot)
library(tidyverse)
library(Hmisc)

# Read the data that was cleaned in "erasmus.R".
data <- read.csv("./cleaned.csv")
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

points <- 500
data_list <- list(
  n=points,
  y=sample(dur, size = points) # Tar bort noe data for å teste modellen, for det tar veldig lang tid med all dataen!
  # Vet ikke helt hva vi burde gjøre for å løse dette problemet!? Nå har jeg samplet fra dataen!
)

# Prøver å følge følgende guider for å få til Stan-koden:
# https://mc-stan.org/docs/2_29/stan-users-guide/summing-out-the-responsibility-parameter.html
# https://mc-stan.org/docs/2_29/stan-users-guide/vectorizing-mixtures.html
# https://mc-stan.org/users/documentation/case-studies/identifying_mixture_models.html

fit <- stan("stan_models/bimodal.stan", iter = 1000, chains = 4,
           data = data_list, seed = 1)
# Error evaluating the log probability at the inital value. 
# Den bruker sinnsykt lang tid på å kompilere! Mulig det er for komplisert og/eller at jeg ikke har valgt fornuftige fordelinger. 


# Convergence analysis.
print(fit) # Den konvergerte ikke hos meg!
traceplot(fit)

posterior <- as.data.frame(fit)
head(posterior)          
dim(posterior)

par(mfrow = c(3, 3))
acf(posterior$p)
acf(posterior$sigma1)
acf(posterior$sigma2)
acf(posterior$mu1)
acf(posterior$mu2)
acf(posterior$c)
acf(posterior$d)
acf(posterior$a)
acf(posterior$b)
par(mfrow = c(1,1))

mcmc_areas(posterior %>% select(p)) + ggtitle("Posterior distribution of p")

plot_title <- ggtitle("Posterior distributions of mu", "with medians and 80% intervals")
mcmc_areas(posterior %>% select(c(mu1,mu2)), 
           pars = c("mu1", "mu2"), 
           prob = 0.8) + plot_title

plot_title <- ggtitle("Posterior distributions of sigma", "with medians and 80% intervals")
mcmc_areas(posterior %>% select(c(sigma1,sigma2)), 
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
# Den konvergerte ikke! Endre litt på priorene muligens. 
