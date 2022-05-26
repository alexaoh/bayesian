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
describe(data)

points <- dim(data)[[1]] 

data_list <- list(
  n=points,
  y=data$duration
)

fit3 <- stan("../stan_models/model3.stan", iter = 2000, chains = 4,
             data = data_list, seed = 1)

# Save the fitted object in order to not run again every time. 
# Analysis can easily be done later by loading this object. 
saveRDS(fit3, file = "../model3_FIT15k.rds") # Used for saving one object. 

# Load the (already) generated object into scope. 
fit3 <- readRDS("../model3_FIT15k.rds") # Load one object.

# Convergence analysis.
print(fit3)
traceplot(fit3)
ggsave("../626fca86090ba51a6aff419a/plots/traceplot3.pdf", width = 7, height = 5)

 # Lag en ok LaTeX tabell!
xtable(summary(fit3)$summary)

posterior <- as.data.frame(fit3)
y_pred <- posterior[, "y_pred"]
plot(density(y_pred))

tibble(y_pred) %>% 
  ggplot(aes(y_pred)) +
  geom_density(aes(y = (..count..)/sum(..count..))) +
  ggtitle("Mix of Gaussian")
ggsave("../626fca86090ba51a6aff419a/plots/postpred3.pdf", width = 7, height = 5)


# We select the statistics 1st quart, median and 3rd quart. 
q.sim <- quantile(y_pred, c(0.25, 0.5, 0.75))
q.sim

# Compare with the same statistics in the data. 
q.data <- quantile(data$duration, c(0.25, 0.5, 0.75))
q.data
