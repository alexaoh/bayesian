library(rstan)
library(bayesplot)
library(tidyverse)
library(Hmisc)
library(ggplot2)
library(xtable)

save <- F # Set parameter for saving images. 

set.seed(1234)
#setwd("/home/ajo/gitRepos/bayesian/project/models")

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

# fit4 <- stan("../stan_models/model4.stan", iter = 1000, chains = 4,
#             data = data_list, seed = 1)

# Save the fitted object in order to not run again every time. 
# Analysis can easily be done later by loading this object. 
#saveRDS(fit4, file = "../model4_FIT15k.rds") # Used for saving one object. 


# Load the (already) generated object into scope. 
fit4 <- readRDS("../fits/model4_FIT15k.rds") # Load one object.

# Convergence analysis.
print(fit4)
traceplot(fit4)
if (save) ggsave("../626fca86090ba51a6aff419a/plots/traceplot4.pdf", width = 7, height = 5)

# Make code for LaTeX table. 
xtable(summary(fit4)$summary)

posterior <- as.data.frame(fit4)

plot_title <- ggtitle("Posterior predictive distribution", "with medians and 80% intervals")
mcmc_areas(posterior %>% select(y_pred), 
           pars = c("y_pred"), 
           prob = 0.8) + plot_title + 
  theme_gray() + ylab("Posterior Predictive Distribution") +
  xlab("Duration [days]")
if (save) ggsave("../626fca86090ba51a6aff419a/plots/model4_postpred.pdf", width = 7, height = 5)

plot_title <- ggtitle("Posterior distribution of sigma1 and sigma2", "with medians and 80% intervals")
mcmc_areas(posterior %>% select(sigma1, sigma2), 
           pars = c("sigma1", "sigma2"), 
           prob = 0.8) + plot_title + 
  theme_gray() + ylab("Posterior Distribution") +
  xlab("Duration [days]")
if (save) ggsave("../626fca86090ba51a6aff419a/plots/model4_postsigma.pdf", width = 7, height = 5)

plot_title <- ggtitle("Posterior distribution of p", "with median and 80% intervals")
mcmc_areas(posterior %>% select(p), 
           pars = c("p"), 
           prob = 0.8) + plot_title + 
  theme_gray() + ylab("Posterior Distribution") +
  xlab("Probability")
if (save) ggsave("../626fca86090ba51a6aff419a/plots/model4_postp.pdf", width = 7, height = 5)

plot_title <- ggtitle("Posterior distribution of beta01 and beta02", "with median and 80% intervals")
mcmc_areas(posterior %>% select(beta01, beta02), 
           pars = c("beta01", "beta02"), 
           prob = 0.8) + plot_title + 
  theme_gray() + ylab("Posterior Distribution") +
  xlab("Duration [days]")
if (save) ggsave("../626fca86090ba51a6aff419a/plots/model4_postbeta0.pdf", width = 7, height = 5)

plot_title <- ggtitle("Posterior distribution of beta1 and beta2", "with median and 80% intervals")
mcmc_areas(posterior %>% select(beta1, beta2), 
           pars = c("beta1", "beta2"), 
           prob = 0.8) + plot_title + 
  theme_gray() + ylab("Posterior Distribution") +
  xlab("Duration [days]")
if (save) ggsave("../626fca86090ba51a6aff419a/plots/model4_postbeta.pdf", width = 7, height = 5)
