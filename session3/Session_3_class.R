# Session 3: Comparison of 2 treatments


# Exercise 3.1: Burns

# > Optional exercise:
#
# Redo the calculations using a normal priori

# The calculations are redone with a normal priori below, 
# in order to see if the results are similar. 
# The results are pretty much the same (since I was able to make the normals resemble the betas relatively well)
# By making the priors even more similar, the results will become even more similar as well. 
library(ggplot2)
library(dplyr)
library(tidyr)

## Prior distributions
delta_p <- 0.01
p <- seq(0, 1, delta_p)
# Conventional prior: 0.4 < p < 0.8
prior_par_c <- c(mean = 0.6, sd = 3/30)
prior_c <- dnorm(x = p, mean = prior_par_c[1], sd = prior_par_c[2])

# Experimental prior: 0.6 < p < 0.9
prior_par_e <- c(mean = 0.75, sd = 0.075)
prior_e <- dnorm(x = p, mean = prior_par_e[1], sd = prior_par_e[2])

# Plot prior comparison
df <- tibble(p, prior_c, prior_e)
ggplot(df) +
  geom_line(aes(x = p, y = prior_c, linetype = "c")) +
  geom_line(aes(x = p, y = prior_e, linetype = "e")) +
  ggtitle("Prior distribution") +
  ylab("density") +
  scale_linetype_manual(name = "Treatment",
                        values = 1:2, labels = c("Conventional", "Experimental"))


## Data
n <- 40
y_c <- 24
y_e <- 30

## Likelihood distributions
likelihood_c <- dbinom(y_c, n, p)
likelihood_e <- dbinom(y_e, n, p)
## Function to standardize
std_dist <- function(x, delta) {
  x / (sum(x) * delta)
}
## Posterior distributions
# b.1) Grid solution:
# Standardize likelihoods
# Calculate products
# Posteriors: standardize products
df <- df %>%
  mutate(likelihood_c = std_dist(likelihood_c, delta_p),
         likelihood_e = std_dist(likelihood_e, delta_p),
         product_c = prior_c * likelihood_c,
         product_e = prior_e * likelihood_e,
         posterior_c = std_dist(product_c, delta_p),
         posterior_e = std_dist(product_e, delta_p))

ggplot(df) +
  geom_line(aes(x = p, y = prior_c, linetype = "c", col = "1")) +
  geom_line(aes(x = p, y = prior_e, linetype = "e", col = "1")) +
  geom_line(aes(x = p, y = likelihood_c, linetype = "c", col = "2")) +
  geom_line(aes(x = p, y = likelihood_e, linetype = "e", col = "2")) +
  geom_line(aes(x = p, y = posterior_c, linetype = "c", col = "3")) +
  geom_line(aes(x = p, y = posterior_e, linetype = "e", col = "3")) +
  ggtitle("Prior distribution") +
  ylab("density") +
  scale_linetype_manual(name = "Treatment",
                        values = 1:2, labels = c("Conventional", "Experimental")) +
  scale_color_manual(name = "Distribution",
                     values = 1:3, labels = c("Prior", "Likelihood", "Posterior"))

# New variable: gamma = difference between rates of improvement
# c.1) Simulate from grid solution:
n_sim <- 10000
df %>%
  select(p, posterior_c, posterior_e)
post_sim_c <- sample(p, size = n_sim, replace = TRUE, prob = df$posterior_c)
post_sim_e <- sample(p, size = n_sim, replace = TRUE, prob = df$posterior_e)
gamma <- post_sim_e - post_sim_c

fig <- ggplot() +
  geom_density(data = tibble(gamma), aes(gamma)) 
fig
fig +
  geom_vline(xintercept = 0, color = "red")
mean(gamma > 0)

# odds_ratio = p / (1-p)
odds_c <- post_sim_c / (1 - post_sim_c)
odds_e <- post_sim_e / (1 - post_sim_e)
# 95% credible interval
ci95_c <- quantile(odds_c, c(0.025, 0.975))
ci95_e <- quantile(odds_e, c(0.025, 0.975))
# Plot
odds <- tibble(
  odds_c, odds_e) %>%
  gather(treatment, odds)
ggplot(odds) +
  geom_density(aes(odds, col = treatment))
