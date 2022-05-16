library(rstan)
library(bayesplot)
library(tidyverse)
library(Hmisc)
library(ggplot2)
library(xtable)

set.seed(1234)
setwd("/home/ajo/gitRepos/bayesian/project/models")
# Read the data that was cleaned in "erasmus.R".
data <- read.csv("../cleaned.csv")
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
train <- sample(dur, size = points)
data_list <- list(
  n=points,
  y=train # Sample `points` number of points from the dataset.
)

fit1 <- stan("../stan_models/model1.stan", iter = 1000, chains = 4,
            data = data_list, seed = 1, cores = 8)

# Save the fitted object in order to not run again every time. 
# Analysis can easily be done later by loading this object. 
save(fit1, train, file="model150k.RData") # Used for saving several objects. 
#saveRDS(fit1, file = "model1FIT50k.rds") # Used for saving one object. 

#fit1 <- readRDS("model1FIT50k.rds") # Load one object.
# Load several objects into scope.
# In this case we load "fit1" and "train".
load(file = "model150k.RData")

# Convergence analysis.
print(fit1)
traceplot(fit1)

# Lag en ok LaTeX tabell!
xtable(summary(fit1)$summary)


# Annen måte å plotte chainsene på. 
# https://mc-stan.org/users/documentation/case-studies/identifying_mixture_models.html
# Lånt fra lenken: kan være grei å bruke for å sjekke at alt er greit underveis også!
chains1 <- as.data.frame(rstan::extract(fit1, permuted=FALSE)[,1,])
chains2 <- as.data.frame(rstan::extract(fit1, permuted=FALSE)[,2,])
chains3 <- as.data.frame(rstan::extract(fit1, permuted=FALSE)[,3,])
chains4 <- as.data.frame(rstan::extract(fit1, permuted=FALSE)[,4,])

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

posterior <- as.data.frame(fit1)
head(posterior)          
dim(posterior)


####### Model Checking
# Calculate the Posterior Predictive Distribution (Det stemmer vel dette?)
p.mean <- mean(posterior$p)
mu1.mean <- mean(posterior$mu1)
mu2.mean <- mean(posterior$mu2)
sigma.mean <- mean(posterior$sigma)

N <- 10000
components <- sample(1:2,prob=c(p.mean,1-p.mean),size=N,replace=TRUE)
mus <- c(mu1.mean,mu2.mean)
sds <- c(sigma.mean,sigma.mean) 
samples <- rnorm(N)*sds[components]+mus[components]
tibble(samples) %>% 
  ggplot(aes(samples)) +
  geom_density(aes(y = (..count..)/sum(..count..))) +
  ggtitle("Mix of Gaussian")
ggsave("../626fca86090ba51a6aff419a/plots/postpred1.pdf", width = 7, height = 5)

# Usikker på om denne er normalisert (slik at det er en density)
# eller om den som er plottet ovenfor er det?!
d <- density(samples, n = N)
plot(d)
# Uansett er begge to en kernel density estimator. 

n <- 10000
# We select the statistics 1st quart, median and 3rd quart. 
statistic.distrs <- list(first = rep(NA, n), median = rep(NA, n), third = rep(NA, n))

for(i in 1:n){
  # Simulate the posterior distribution. 
  components <- sample(1:2,prob=c(p.mean,1-p.mean),size=N,replace=TRUE)
  mus <- c(mu1.mean,mu2.mean)
  sds <- c(sigma.mean,sigma.mean) 
  samples <- rnorm(N)*sds[components]+mus[components]
  
  # Simulate from the posterior distribution.
  q <- quantile(samples, c(0.25, 0.5, 0.75))
  statistic.distrs$first[i] <- q[1]
  statistic.distrs$median[i] <- q[2]
  statistic.distrs$third[i] <- q[3]
}

# Compare with the same statistics in the data. 
q.data <- quantile(dur, c(0.25, 0.5, 0.75))

# Plot shows that the first quartile in the data is highly unlikely in the reference distribution. 
# Perhaps not a good model then!
tibble(statistic.distrs$first) %>% 
  ggplot(aes(statistic.distrs$first)) +
  geom_density(aes(y = (..count..)/sum(..count..))) +
  ggtitle("First Quartile") +
  geom_vline(xintercept = q.data[1])
# Also did numerical calculation.
min(mean(statistic.distrs$first < q.data[1]), mean(statistic.distrs$first > q.data[1]))

# Plot shows that median is highly unlikely.
tibble(statistic.distrs$median) %>% 
  ggplot(aes(statistic.distrs$median)) +
  geom_density(aes(y = (..count..)/sum(..count..))) +
  ggtitle("Median") +
  geom_vline(xintercept = q.data[2])
# Also did numerical calculation.
min(mean(statistic.distrs$median < q.data[2]), mean(statistic.distrs$median > q.data[2]))

# Plot shows that third quartile is highly unlikely.
# Model is not good according to any of these three statistics. 
tibble(statistic.distrs$third) %>% 
  ggplot(aes(statistic.distrs$third)) +
  geom_density(aes(y = (..count..)/sum(..count..))) +
  ggtitle("Third Quartile") +
  geom_vline(xintercept = q.data[3])
# Also did numerical calculation.
min(mean(statistic.distrs$third < q.data[3]), mean(statistic.distrs$third > q.data[3]))
