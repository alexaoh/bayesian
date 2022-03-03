
# Exercise 2.1 ------------------------------------------------------------

## ------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

theme_set(theme_bw())

# a) Prior definition -----------------------------------------------------
set.seed(123456)

# Simulate 10000 draws from prior density: l_sim
prior <- c(90, 4)
l_sim <- rgamma(10000, shape = prior[1], rate = prior[2])
ggplot(tibble(l_sim), aes(l_sim)) +
  geom_density()

# b) Likelihood calculation -----------------------------------------------

y <- c(21, 17, 17, 19, 16, 18, 15, 10, 17, 16)
n <- length(y)

delta_l <- 0.01
lambda <- seq(0, 60, delta_l)
likelihood <- exp(-lambda*n)*lambda^sum(y)
df <- tibble(lambda, likelihood)
df$likelihood <- df$likelihood/(sum(df$likelihood)*delta_l) # Standardize the likelihood such that it can be plotted. 

ggplot(df) +
  geom_line(aes(x = lambda, y = likelihood), col = "green") +
  geom_density(data = tibble(l_sim), aes(l_sim),col = "blue") 

# c) Draw the prior predictive distribution (conjugated) ------------------

x <- 0:60
prior <- c(90,4)
pred <- dnbinom(x, prior[1], prior[2]/(1 + prior[2]))
df2 <- tibble(x, pred) 
ggplot(df2) +
  geom_bar(aes(x, pred), stat = "identity")



# d) Posterior calculation (conjugated) -----------------------------------

# Simulate 10000 draws from posterior density: l_post_sim
n <- length(y)

posterior <- c(_____, _____)
l_post_sim <- _____
ggplot(tibble(l_post_sim), aes(l_post_sim)) +
  geom_density()


# Simulate 10000 draws from the posterior predictive density: y_sim



# Plot the posterior predictive density




## d) Posterior calculation (revisited)

y <- c(21, 17, 17, 19, 16, 18, 15, 10, 17, 16)
delta_l <- 0.01

df <- tibble(
  lambda = seq(0, 60, delta_l),
  likelihood = map_dbl(lambda, ~ prod(dpois(y, .x))),
  prior = dgamma(____, shape = ____, rate = ____),
  product = ____,
  posterior = ____
)

#  Use ggplot() to create a figure with the prior, the likelihood and the posterior
# distributions





# e) Probability that the number of visitors next week will be lower than 10





# f) Draw the posterior predictive distribution ---------------------------





# g) For what option will you bet? ----------------------------------------





# h) Flat prior -----------------------------------------------------------


