# Plot the prior predictive distributions of mu1 and mu2. 

library(invgamma)
library(tidyverse)
library(ggplot2)

delta.x <- 0.1
x <- seq(0, 500, delta.x)
N <- length(x)
m1 <- 120
m2 <- 280
prior.sigma <- rinvgamma(N, 10,100)


prior.mu1 <- rnorm(x, mean = m1, sd = prior.sigma)
prior.mu2 <- rnorm(x, mean = m2, sd = prior.sigma)

prior.p <- runif(x)
components <- sample(1:2,prob=c(0.5,0.5),size=N,replace=TRUE)
mus <- c(m1,m2)
prior.y_pred <- rnorm(N, mean = 0, sd = prior.sigma)+mus[components]

tibble(prior.sigma, prior.mu1, prior.mu2, prior.y_pred) %>% 
  ggplot() +
  geom_density(aes(prior.sigma, y = (..count..)/sum(..count..), color = "blue")) +
  geom_density(aes(prior.mu1, y = (..count..)/sum(..count..), color = "red")) +
  geom_density(aes(prior.mu2, y = (..count..)/sum(..count..), color = "green")) +
  geom_density(aes(prior.y_pred, y = (..count..)/sum(..count..), color = "yellow")) +
  ggtitle("Prior Distributions") +
  labs(x = "Duration",
      y = "Density",
      ) +
  xlim(c(0, 500)) +
  scale_color_manual(name = "Prior", values = c("blue", "red", "green", "yellow"),
                     labels = c("sigma", "mu1", "mu2", "y_pred"))
ggsave("./626fca86090ba51a6aff419a/plots/priorpreds.pdf", width = 7, height = 5)
