library(rstan)
library(bayesplot)
library(tidyverse)
library(Hmisc)
library(ggplot2)
library(xtable)

save <- T # Set parameter for saving images. 

set.seed(1234)
setwd("/home/ajo/gitRepos/bayesian/project/models")

# Read the training data set that was sampled.
data <- readRDS("../15kpoints.rds") # Load the sampled data. 

# Omit the unknown genders for our regression model
data <- data %>% filter(gender == "Female" | gender == "Male")
data$gender <- as.integer(data$gender) - 1
describe(data)

points <- dim(data)[[1]] 

# We simulate values from the posterior distribution using Stan. 
# Define model and call stan. 

data_list <- list(
  n=points,
  y=data$duration,
  x1=data$age,
  x2=as.numeric(data$gender)
)

#fit4 <- stan("../stan_models/model4.stan", iter = 1000, chains = 4,
#             data = data_list, seed = 1)

# Save the fitted object in order to not run again every time. 
# Analysis can easily be done later by loading this object. 
#saveRDS(fit4, file = "../model4_FIT15k.rds") # Used for saving one object. 


# Load the (already) generated object into scope. 
fit4 <- readRDS("../model4_FIT15k.rds") # Load one object.

# Convergence analysis.
print(fit4)
traceplot(fit4)
if (save) ggsave("../626fca86090ba51a6aff419a/plots/traceplot4.pdf", width = 7, height = 5)

# Lag en ok LaTeX tabell!
xtable(summary(fit4)$summary)

posterior <- as.data.frame(fit4)

plot_title <- ggtitle("Posterior predictive distribution", "with medians and 80% intervals")
mcmc_areas(posterior %>% select(y_pred), 
           pars = c("y_pred"), 
           prob = 0.8) + plot_title + 
  theme_gray() + ylab("Posterior Predictive Distribution") +
  xlab("Duration [days]")
if (save) ggsave("../626fca86090ba51a6aff419a/plots/model4_postpred.pdf", width = 7, height = 5)

plot_title <- ggtitle("Posterior distribution of sigma1 and sigma2", "with medians and 80% intervals")
mcmc_areas(posterior %>% select(sigma1, sigma2), 
           pars = c("sigma1", "sigma2"), 
           prob = 0.8) + plot_title + 
  theme_gray() + ylab("Posterior Distribution") +
  xlab("Duration [days]")
if (save) ggsave("../626fca86090ba51a6aff419a/plots/model4_postsigma.pdf", width = 7, height = 5)

# Hvordan kan vi plotte post_mu! og post_mu2?
# plot_title <- ggtitle("Posterior distribution of mu1", "with median and 80% intervals")
# mcmc_areas(posterior %>% select(mu1), 
#            pars = c("mu1"), 
#            prob = 0.8) + plot_title + 
#   theme_gray() + ylab("Posterior Distribution") +
#   xlab("Duration [days]")
# if (save) ggsave("../626fca86090ba51a6aff419a/plots/model3_postmu1.pdf", width = 7, height = 5)
# 
# plot_title <- ggtitle("Posterior distribution of mu2", "with median and 80% intervals")
# mcmc_areas(posterior %>% select(mu2), 
#            pars = c("mu2"), 
#            prob = 0.8) + plot_title + 
#   theme_gray() + ylab("Posterior Distribution") +
#   xlab("Duration [days]")
# if (save) ggsave("../626fca86090ba51a6aff419a/plots/model3_postmu2.pdf", width = 7, height = 5)

plot_title <- ggtitle("Posterior distribution of p", "with median and 80% intervals")
mcmc_areas(posterior %>% select(p), 
           pars = c("p"), 
           prob = 0.8) + plot_title + 
  theme_gray() + ylab("Posterior Distribution") +
  xlab("Probability")
if (save) ggsave("../626fca86090ba51a6aff419a/plots/model4_postp.pdf", width = 7, height = 5)

plot_title <- ggtitle("Posterior distribution of beta01 and beta02", "with median and 80% intervals")
mcmc_areas(posterior %>% select(beta01, beta02), 
           pars = c("beta01", "beta02"), 
           prob = 0.8) + plot_title + 
  theme_gray() + ylab("Posterior Distribution") +
  xlab("Duration [days]")
if (save) ggsave("../626fca86090ba51a6aff419a/plots/model4_postbeta0.pdf", width = 7, height = 5)

plot_title <- ggtitle("Posterior distribution of beta1 and beta2", "with median and 80% intervals")
mcmc_areas(posterior %>% select(beta1, beta2), 
           pars = c("beta1", "beta2"), 
           prob = 0.8) + plot_title + 
  theme_gray() + ylab("Posterior Distribution") +
  xlab("Duration [days]")
if (save) ggsave("../626fca86090ba51a6aff419a/plots/model4_postbeta.pdf", width = 7, height = 5)


# Model Checking. These calculations are not possible to for this model!
# n <- dim(posterior)[[1]]
# # We select the statistics 1st quart, median, mean and 3rd quart. 
# statistic.distrs <- list(first = rep(NA, n), median = rep(NA, n), 
#                          mean = rep(NA, n), third = rep(NA, n))
# N <- 10000
# for(i in 1:n){
#   # Simulate the posterior distribution using every simulated value from MCMC fit (Stan) once. 
#   p <- posterior$p[i]
#   mu1 <- posterior$mu1[i]
#   mu2 <- posterior$mu2[i]
#   sigma1 <- posterior$sigma1[i]
#   sigma2 <- posterior$sigma2[i]
#   
#   components <- sample(1:2,prob=c(p,1-p),size=N,replace=TRUE)
#   mus <- c(mu1,mu2)
#   sds <- c(sigma1,sigma2) 
#   samples <- rnorm(N)*sds[components]+mus[components]
#   
#   # Simulate from the posterior distribution.
#   q <- quantile(samples, c(0.25, 0.5, 0.75))
#   statistic.distrs$first[i] <- q[1][[1]]
#   statistic.distrs$median[i] <- q[2][[1]]
#   statistic.distrs$mean[i] <- mean(samples)
#   statistic.distrs$third[i] <- q[3][[1]]
# }
# 
# # Compare with the same statistics in the data. The "data" in this case is the sampled data points
# # used to fit the model fit Stan.
# q.data <- quantile(dur, c(0.25, 0.5, 0.75)) 
# stat.data <- list(first = q.data[1][[1]], median = q.data[2][[1]], 
#                   mean = mean(dur), third = q.data[3][[1]])
# 
# df <- cbind(statistic.distrs$first, statistic.distrs$mean, statistic.distrs$median, statistic.distrs$third)
# colnames(df) <- c("25%", "Mean", "50%", "75%")
# 
# # Want to make nice ggplot with all four statistics (using facets).
# df2 <- as.data.frame(df) %>% 
#   gather(key = Statistic, value = value_dens)
# df2 <- df2 %>% mutate(line = c(rep(stat.data$first, n), rep(stat.data$mean, n),
#                                rep(stat.data$median, n), rep(stat.data$third, n)))
# 
# df2 %>% 
#   ggplot() +
#   geom_density(aes(x = value_dens, y = (..count..)/sum(..count..))) +
#   geom_vline(aes(xintercept = line)) + 
#   #facet_grid(rows = vars(Statistic), scales = "free")
#   facet_wrap(~Statistic, scales = "free")
# if (save) ggsave("../626fca86090ba51a6aff419a/plots/checkingModel4.pdf", width = 7, height = 5)
# # Nice plot showing all the chosen statistics at the same time!
# 
# # Numerical calculations:
# num.first <- min(mean(statistic.distrs$first < stat.data$first), mean(statistic.distrs$first > stat.data$first))
# vec1 <- c(mean(statistic.distrs$first < stat.data$first), mean(statistic.distrs$first > stat.data$first), num.first)
# num.mean <- min(mean(statistic.distrs$mean < stat.data$mean), mean(statistic.distrs$mean > stat.data$mean))
# vec2 <- c(mean(statistic.distrs$mean < stat.data$mean), mean(statistic.distrs$mean > stat.data$mean), num.mean)
# num.median <- min(mean(statistic.distrs$median < stat.data$median), mean(statistic.distrs$median > stat.data$median))
# vec3 <- c(mean(statistic.distrs$median < stat.data$median), mean(statistic.distrs$median > stat.data$median), num.median)
# num.third <- min(mean(statistic.distrs$third < stat.data$third), mean(statistic.distrs$third > stat.data$third))
# vec4 <- c(mean(statistic.distrs$third < stat.data$third), mean(statistic.distrs$third > stat.data$third), num.third)
# 
# table.numerical.model4 <- cbind(vec1, vec2, vec3, vec4)
# colnames(table.numerical.model4) <- c("25%", "Mean", "50%", "75%")
# rownames(table.numerical.model4) <- c("Left", "Right", "min")
# xtable(as.data.frame(table.numerical.model4), digits = 5)
# #knitr::kable(table.numerical.model4, format = "latex") # Alternative. 
