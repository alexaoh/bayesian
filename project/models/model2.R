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

# We want to model the duration of the student exchange. 
dur <- data$duration

# We simulate values from the posterior distribution using Stan. 
# Define model and call stan. 

points <- dim(data)[[1]]
data_list <- list(
  n=points,
  y=dur
)

fit2 <- stan("../stan_models/model2.stan", iter = 2000, chains = 4,
             data = data_list, seed = 1)

# Save the fitted object in order to not run again every time. 
# Analysis can easily be done later by loading this object. 
#saveRDS(fit2, file = "../model2_FIT15k.rds") # Used for saving one object. 

# Load the (already) generated object into scope. 
fit2 <- readRDS("../model2_FIT15k.rds") # Load one object.

# Convergence analysis.
print(fit2)
traceplot(fit2)

xtable(summary(fit2)$summary)

posterior <- as.data.frame(fit2)
head(posterior)          
dim(posterior2)

data.frame(posterior %>% select(y_pred)) %>% 
  ggplot(aes(y_pred)) +
  geom_density(aes(y = (..count..)/sum(..count..))) +
  ggtitle("Mix of Gaussian") + 
  ylab("Posterior Predictive Distribution") +
  xlab("Duration [days]") 
#+ ggsave("../626fca86090ba51a6aff419a/plots/postpred2.pdf", width = 7, height = 5)
