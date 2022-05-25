# Plot the prior predictive distributions of mu1 and mu2. 

library(invgamma)
library(tidyverse)
library(ggplot2)

delta.x <- 0.1
x <- seq(0, 500, delta.x)
N <- length(x)
m1 <- 120
m2 <- 280
prior.sigma <- rinvgamma(N, 10, 1000)
sigma <- rinvgamma(N, 1, 1)

s <- seq(0, 500, .01)
plot(s, dinvgamma(s, 10, 1000), type = 'l')

hist(prior.sigma, breaks = 50, freq = F)

prior.mu1 <- rnorm(x, mean = m1, sd = prior.sigma)
plot(density(prior.mu1))
prior.mu2 <- rnorm(x, mean = m2, sd = prior.sigma)
plot(density(prior.mu2))

prior.p <- runif(x)
prior.y_pred <- prior.p*rnorm(x, mean = prior.mu1, prior.sigma) +
                (1 - prior.p)*rnorm(x, mean = prior.mu2, prior.sigma)
plot(density(prior.y_pred))

tibble(x, prior.sigma, prior.mu1, prior.mu2, prior.y_pred) %>% 
  ggplot() +
  geom_density(aes(x = prior.sigma, linetype = "c")) +
  geom_density(aes(x = prior.mu1, linetype = "e")) +
  geom_density(aes(x = prior.mu2, linetype = "d")) +
  geom_density(aes(x = prior.y_pred , linetype = "b")) +
  ggtitle("Prior Distributions") +
  ylab("Density") +
  xlab("Duration") +
  xlim(c(0, 500)) + 
  scale_linetype_manual(name = "Mean Parameters", 
                       values = 1:4, labels = c("sigma", "mu1", "mu2", "y_pred"))
ggsave("./626fca86090ba51a6aff419a/plots/priorpreds.pdf", width = 7, height = 5)

