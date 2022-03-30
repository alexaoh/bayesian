# Session 5

# Exercise: discoveries ---------------------------------------------------

# a, b, c) discoveries.stan model

# d)
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())

# e) data
df <- read_csv(file = "________")


data_list <- list(
__________
  )

# f) run the model using Stan

# g) diagnose whether your model has converged by printing `fit`


# h) equivalent number of samples

# i) central posterior 80% credible interval for lambda
## Option 1: 
print(fit, pars = "______", __________)

## Option 2: extract lambda from the fit object




# j) histogram of the lambda posterior sample


# k) graph the data




# l) posterior predictive

# Option 1: in R




# Option 2: generated_quantities (modify the Stan file)



# m) negative binomial model.extract









# Exercise: hungover holiday regressions ----------------------------------

# a, b) data


# c) interpretation

# d) stan model

# Read the data


# Define data list


# Run model


# Analyze results