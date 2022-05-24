# Plot the prior predictive distributions of mu1 and mu2. 

library(invgamma)
library(tidyverse)
library(ggplot2)

delta.x <- 0.001
x <- seq(0, 500, delta.x)
N <- length(x)
m1 <- 120
m2 <- 280
sigma.sq <- rinvgamma(N, 10, 10000)
sigma <- rinvgamma(N, 1, 1)

s <- seq(0, 5, .01)
plot(s, dinvgamma(s, 10, 10000), type = 'l')

hist(sigma.sq, breaks = 50, freq = F)

prior.mu1 <- dnorm(x, mean = m1, sd = sigma)
plot(density(prior.mu1))
prior.mu2 <- dnorm(x, mean = m2, sd = sigma)
tibble(x, prior.mu1, prior.mu2) %>% 
  ggplot() +
  geom_line(aes(x = x, y = prior.mu1, linetype = "c")) +
  geom_line(aes(x = x, y = prior.mu2, linetype = "e")) +
  ggtitle("Prior Distributions") +
  ylab("Density") +
  scale_linetype_manual(name = "Mean Parameters", 
                       values = 1:2, labels = c("mu1", "mu2"))
#+ ggsave("../626fca86090ba51a6aff419a/plots/priorpreds.pdf", width = 7, height = 5)