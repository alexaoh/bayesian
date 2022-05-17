library(rstan)
library(bayesplot)
library(tidyverse)
library(Hmisc)
library(ggplot2)

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

points <- 10000
train <- sample(dur, size = points)
data_list <- list(
  n=points,
  y=train # Sample `points` number of points from the dataset.
)

fit2 <- stan("../stan_models/model2.stan", iter = 1000, chains = 4,
             data = data_list, seed = 1)

# Save the fitted object in order to not run again every time. 
# Analysis can easily be done later by loading this object. 
save(fit1, train, file="model1_10k.RData") # Used for saving several objects. 
#saveRDS(fit1, file = "model1FIT50k.rds") # Used for saving one object. 

#fit1 <- readRDS("model1FIT50k.rds") # Load one object.
# Load several objects into scope.
# In this case we load "fit1" and "train".
load(file = "model1_10k.RData")


# Convergence analysis.
print(fit2)
traceplot(fit2)

# Sjekk om dette fungerer for å lage en ok LaTeX tabell!
xtable(summary(fit2)$summary)
