
# Example 1 - simple ------------------------------------------------------

library(rstan)
options(mc.cores = parallel::detectCores())

# Generative normal model
N <- 10
mu <- 1.5
sigma <- 0.1
Y <- rnorm(N, mu, sigma)

# Data
data_list <- list(
  N = N,
  Y = Y)

# Compile and run the MCMC on the Stan program
set.seed(1234)
fit <- stan("stan_models/simple.stan", iter = 2000, chains = 4,
  data = data_list)

fit


# Interpreting the results  -----------------------------------------------
# Model Summary
print(fit, probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95))

# Posterior predictions
posterior <- as.matrix(fit)
colnames(posterior)
# Autocorrelation plot
acf(posterior[, "mu"])
acf(posterior[, "sigma"])

# Samples vs. iteration plot
traceplot(fit)

# Posterior of prob1
plot(density(posterior[, "mu"]))
plot(density(posterior[, "sigma"]))


library(bayesplot)

plot_title <- ggtitle("Posterior distributions of mu and sigma", "with medians and 80% intervals")
mcmc_areas(posterior, 
  pars = c("mu", "sigma"), 
  prob = 0.8) + plot_title

posterior
mcmc_trace(posterior, 
  pars = c("mu", "sigma"))

mcmc_trace(posterior, 
           pars = c("mu", "sigma"),
           facet_args = list(nrow = 2))

#save(posterior, file=paste("results/simple.RData", sep=""))


# Example 2 - Poisson -----------------------------------------------------

# a, b, c, d) discoveries.stan model
library(rstan)
options(mc.cores = parallel::detectCores())

# e) data
discoveries.dat <- read.csv("data/evaluation_discoveries.csv")

# f) run the model using Stan

N <- dim(discoveries.dat)[[1]]

# Data
data_list <- list(
  N = N,
  Y = discoveries.dat[,2])

# Compile and run the MCMC on the Stan program
fit <- stan("stan_models/discoveries.stan", iter = 1000, chains = 4,
            data = data_list, seed = 1)

# g) diagnose whether your model has converged by printing `fit`
fit # Yes, it has converged!

# h) equivalent number of samples
# For your sample what is the equivalent number of samples for an independent sampler?

# I do not understand the question. Are they asking for n_eff?

# i) central posterior 80% credible interval for lambda
print(fit, probs = c(0.2,0.8))


# j) histogram of the lambda posterior sample
posterior <- as.matrix(fit)
colnames(posterior)

library(dplyr)
library(ggplot2)
ggplot(data = tibble("lambda" = posterior[,1], "lp" = posterior[,2]), aes(lambda)) +
  geom_histogram(bins = 70) +
  theme_bw()
hist(posterior[,1], breaks = 70)

# k) graph the data
plot(discoveries.dat, main = "Discoveries Data")
# Does this suggest anything about our model's assumptions? Not sure?!

# l) posterior predictive
# Added "generated quantities" and running the model again. 
fit <- stan("stan_models/discoveries.stan", iter = 1000, chains = 4,
            data = data_list, seed = 1)

post.pred <- as.matrix(fit)
par(mfrow = c(1,2))
plot(post.pred[2000,-c(102,103,104)], main = "Posterior Predictive Distribution") 
plot(discoveries.dat, main = "Discoveries Data")

# Negative binomial instead
fit <- stan("stan_models/discoveries_negbin.stan", iter = 1000, chains = 4,
            data = data_list, seed = 1)
fit # Yes, it has converged!
