library(rstan)
library(bayesplot)
library(tidyverse)
library(Hmisc)
library(ggplot2)
library(xtable)

set.seed(1234)
setwd("/home/ajo/gitRepos/bayesian/project/models")

# Read the training data set that was sampled.
data <- readRDS("../15kpoints.rds") # Load the sampled data. 

# Omit the unknown genders for our regression model
data <- data %>% filter(gender == "Female" | gender == "Male")
data$gender <- as.integer(data$gender) - 1
describe(data)

points <- dim(data)[[1]] 

# We simulate values from the posterior distribution using Stan. 
# Define model and call stan. 

data_list <- list(
  n=points,
  y=data$duration,
  x1=data$age,
  x2=as.numeric(data$gender)
)

fit4 <- stan("../stan_models/model4.stan", iter = 1000, chains = 4,
             data = data_list, seed = 1)
# Save the fitted object in order to not run again every time. 
# Analysis can easily be done later by loading this object. 
saveRDS(fit4, file = "../model4_FIT15k.rds") # Used for saving one object. 


# Load the (already) generated object into scope. 
fit4 <- readRDS("../model4_FIT15k.rds") # Load one object.

# Convergence analysis.
print(fit4)
traceplot(fit4)
ggsave("../626fca86090ba51a6aff419a/plots/traceplot4.pdf", width = 7, height = 5)

# Lag en ok LaTeX tabell!
xtable(summary(fit4)$summary)

posterior <- as.data.frame(fit4)
y_pred <- posterior[, "y_pred"]
plot(density(y_pred))

tibble(y_pred) %>% 
  ggplot(aes(y_pred)) +
  geom_density(aes(y = (..count..)/sum(..count..))) +
  ggtitle("Mix of Gaussian")
ggsave("../626fca86090ba51a6aff419a/plots/postpred4.pdf", width = 7, height = 5)

beta1 <- posterior[, "beta1"]
tibble(beta1) %>% 
  ggplot(aes(beta1)) +
  geom_density(aes(y = (..count..)/sum(..count..))) +
  ggtitle("Posterior distribution of beta1")
ggsave("../626fca86090ba51a6aff419a/plots/postbeta1.pdf", width = 7, height = 5)

beta2 <- posterior[, "beta2"]
tibble(beta2) %>% 
  ggplot(aes(beta2)) +
  geom_density(aes(y = (..count..)/sum(..count..))) +
  ggtitle("Posterior distribution of beta2")
ggsave("../626fca86090ba51a6aff419a/plots/postbeta2.pdf", width = 7, height = 5)



# We select the statistics 1st quart, median and 3rd quart. 
q.sim <- quantile(y_pred, c(0.25, 0.5, 0.75))
q.sim

# Compare with the same statistics in the data. 
q.data <- quantile(data$duration, c(0.25, 0.5, 0.75))
q.data

mean(y_pred)
mean(data$duration)
quantile(posterior$beta1, c(0.025, 0.975))
quantile(posterior$beta2, c(0.025, 0.975))

mean(posterior$beta1)
mean(posterior$beta2)

