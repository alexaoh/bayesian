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

# Sample 5k points from the 15k sample to begin with. 
points <- 5000
sample_df <- data[sample(1:nrow(data), points),]
summary(sample_df)

data_list <- list(
  n=points,
  y=sample_df$duration
)

fit3 <- stan("../stan_models/model3.stan", iter = 1000, chains = 4,
             data = data_list, seed = 1)

# Save the fitted object in order to not run again every time. 
# Analysis can easily be done later by loading this object. 
saveRDS(fit3, file = "../model3_FIT15k.rds") # Used for saving one object. 

# Load the (already) generated object into scope. 
fit3 <- readRDS("../model3_FIT15k.rds") # Load one object.

# Convergence analysis.
print(fit3)
traceplot(fit3)

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
q.data <- quantile(dur, c(0.25, 0.5, 0.75))
q.data

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
