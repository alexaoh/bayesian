# Plot the prior predictive distributions of mu1 and mu2. 

library(invgamma)
library(tidyverse)
library(ggplot2)

delta.x <- 0.1
x <- seq(0, 500, delta.x)
N <- length(x)
m1 <- 120
m2 <- 280
prior.sigma <- sqrt(rinvgamma(N, 10, 10000))

s <- seq(0, 500, .01)
plot(s, sqrt(dinvgamma(s, 10, 1000)), type = 'l')
plot(s, dinvgamma(s, 10, 1000), type = 'l')

hist(prior.sigma, breaks = 50, freq = F)

prior.mu1 <- rnorm(x, mean = m1, sd = prior.sigma)
plot(density(prior.mu1))
prior.mu2 <- rnorm(x, mean = m2, sd = prior.sigma)
plot(density(prior.mu2))

prior.p <- runif(x)
components <- sample(1:2,prob=c(0.5,0.5),size=N,replace=TRUE)
mus <- c(m1,m2)
prior.y_pred2 <- rnorm(N, mean = 0, sd = prior.sigma)+mus[components]
prior.y_pred <- prior.p*rnorm(x, mean = prior.mu1, prior.sigma) +
                (1 - prior.p)*rnorm(x, mean = prior.mu2, prior.sigma)
plot(density(prior.y_pred))
plot(density(prior.y_pred2))

tibble(prior.sigma, prior.mu1, prior.mu2, prior.y_pred2) %>% 
  ggplot() +
  geom_density(aes(prior.sigma, y = (..count..)/sum(..count..), color = "blue")) +
  geom_density(aes(prior.mu1, y = (..count..)/sum(..count..), color = "red")) +
  geom_density(aes(prior.mu2, y = (..count..)/sum(..count..), color = "green")) +
  geom_density(aes(prior.y_pred2, y = (..count..)/sum(..count..), color = "yellow")) +
  ggtitle("Prior Distributions") +
  labs(x = "Duration",
      y = "Density",
      ) +
  xlim(c(0, 500)) +
  scale_color_manual(name = "Prior", values = c("blue", "red", "green", "yellow"),
                     labels = c("sigma", "mu1", "mu2", "y_pred"))
ggsave("./626fca86090ba51a6aff419a/plots/priorpreds.pdf", width = 7, height = 5)
