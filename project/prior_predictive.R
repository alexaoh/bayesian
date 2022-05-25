# Plot the prior predictive distributions of mu1 and mu2. 

library(invgamma)
library(tidyverse)
library(ggplot2)

delta.x <- 0.1
x <- seq(0, 500, delta.x)
N <- length(x)
m1 <- 120
m2 <- 280
sigma.sq <- rinvgamma(N, 10, 1000)
sigma <- rinvgamma(N, 1, 1)

s <- seq(0, 500, .01)
plot(s, dinvgamma(s, 10, 1000), type = 'l')

hist(sigma.sq, breaks = 50, freq = F)

prior.mu1 <- rnorm(x, mean = m1, sd = sigma.sq)
plot(density(prior.mu1))
prior.mu2 <- rnorm(x, mean = m2, sd = sigma.sq)
plot(density(prior.mu2))
tibble(x, prior.mu1, prior.mu2) %>% 
  ggplot() +
  geom_density(aes(x = prior.mu1, linetype = "c")) +
  geom_density(aes(x = prior.mu2, linetype = "e")) +
  ggtitle("Prior Distributions") +
  ylab("Density") +
  scale_linetype_manual(name = "Mean Parameters", 
                       values = 1:2, labels = c("mu1", "mu2"))
ggsave("../626fca86090ba51a6aff419a/plots/priorpreds.pdf", width = 7, height = 5)

prior.p <- runif(x)
pror.y_pred <- prior.p*rnorm(x, mean = prior.mu1, sigma.sq) +
                (1 - prior.p)*rnorm(x, mean = prior.mu2, sigma.sq)
plot(density(prior.mu2))
