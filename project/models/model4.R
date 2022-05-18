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
# Omit the unknown genders for our regression model
data <- data %>% filter(gender == "Female" | gender == "Male")
summary(data)
data$gender <- as.integer(data$gender) - 1
describe(data)



points <- 5000
sample_df <- data[sample(1:nrow(data), points),]
summary(sample_df)


# We simulate values from the posterior distribution using Stan. 
# Define model and call stan. 

data_list <- list(
  n=points,
  y=sample_df$duration,
  x1=sample_df$age,
  x2=sample_df$gender
)

fit1 <- stan("../stan_models/model4.stan", iter = 1000, chains = 4,
             data = data_list, seed = 1)

# Convergence analysis.
print(fit1)
traceplot(fit1)

# Lag en ok LaTeX tabell!
xtable(summary(fit1)$summary)

posterior <- as.data.frame(fit1)
y_pred <- posterior[, "y_pred"]
plot(density(y_pred))

tibble(y_pred) %>% 
  ggplot(aes(y_pred)) +
  geom_density(aes(y = (..count..)/sum(..count..))) +
  ggtitle("Mix of Gaussian")
ggsave("../626fca86090ba51a6aff419a/plots/postpred4.pdf", width = 7, height = 5)


# We select the statistics 1st quart, median and 3rd quart. 
q.sim <- quantile(y_pred, c(0.25, 0.5, 0.75))
q.sim

# Compare with the same statistics in the data. 
q.data <- quantile(data$duration, c(0.25, 0.5, 0.75))
q.data

quantile(posterior$beta1, c(0.025, 0.975))
quantile(posterior$beta2, c(0.025, 0.975))

mean(posterior$beta1)

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
