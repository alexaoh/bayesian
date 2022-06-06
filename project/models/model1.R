library(rstan)
library(bayesplot)
library(tidyverse)
library(Hmisc)
library(ggplot2)
library(xtable)

save <- T # Set parameter for saving images. 

# Set parameter that tells Stan how many cores the machine has. 
# For computing chains in parallel. 
#options(mc.cores = parallel::detectCores()) 

set.seed(1234)
setwd("/home/ajo/gitRepos/bayesian/project/models")
# Read the training data set that was sampled.
data <- readRDS("../15kpoints.rds") # Load the sampled data. 

# We want to model the duration of the student exchange. 
dur <- data$duration 

# We simulate values from the posterior distribution using Stan. 
# Define model and call stan. 

points <- dim(data)[[1]] # Or length(dur)
data_list <- list(
  n=points,
  y=dur 
)

# fit1 <- stan("../stan_models/model1.stan", iter = 2000, chains = 4,
#              data = data_list, seed = 1)

# Save the fitted object in order to not run again every time. 
#saveRDS(fit1, file = "../model1_FIT15k.rds") # Used for saving one object. 

fit1 <- readRDS("../model1_FIT15k.rds") # Load one object.

# Convergence analysis.
print(fit1)
traceplot(fit1)
if (save) ggsave("../626fca86090ba51a6aff419a/plots/traceplot1.pdf", width = 7, height = 5) 

# Lag en ok LaTeX tabell!
xtable(summary(fit1)$summary)


# Another way to plot the chains. 
# https://mc-stan.org/users/documentation/case-studies/identifying_mixture_models.html
# Borrowed from the link above. Nice visualization!
chains1 <- as.data.frame(rstan::extract(fit1, permuted=FALSE)[,1,])
chains2 <- as.data.frame(rstan::extract(fit1, permuted=FALSE)[,2,])
chains3 <- as.data.frame(rstan::extract(fit1, permuted=FALSE)[,3,])
chains4 <- as.data.frame(rstan::extract(fit1, permuted=FALSE)[,4,])

par(mar = c(4, 4, 1.5, 1))
plot(chains1$mu1, chains1$mu2, col="black", pch=16, cex=0.8,
     xlab="mu1", ylab="mu2", xlim = c(130, 140), ylim = c(270, 300), 
     main = "Chains for mu1 and mu2 Plotted in Two Dimensions")
points(chains2$mu1, chains2$mu2, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.5), pch=16, cex=0.8)
points(chains3$mu1, chains3$mu2, col=rgb(red = 0, green = 1, blue = 0, alpha = 0.4), pch=16, cex=0.8)
points(chains4$mu1, chains4$mu2, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.3), pch=16, cex=0.8)
#lines(0.08*(1:100) - 4, 0.08*(1:100) - 4, col="grey", lw=2)
legend("topright", c("Chain 1", "Chain 2", "Chain 3", "Chain 4"),
       fill=c("black", "red",
              "yellow", "blue"), box.lty=0, inset=0.0005)

posterior <- as.data.frame(fit1)
head(posterior)          
dim(posterior)

plot_title <- ggtitle("Posterior predictive distribution", "with medians and 80% intervals")
mcmc_areas(posterior %>% select(y_pred), 
           pars = c("y_pred"), 
           prob = 0.8) + plot_title + 
  theme_gray() + ylab("Posterior Predictive Distribution") +
  xlab("Duration [days]")
if (save) ggsave("../626fca86090ba51a6aff419a/plots/model1_postpred.pdf", width = 7, height = 5)

plot_title <- ggtitle("Posterior distribution of sigma", "with medians and 80% intervals")
mcmc_areas(posterior %>% select(sigma), 
           pars = c("sigma"), 
           prob = 0.8) + plot_title + 
  theme_gray() + ylab("Posterior Distribution") +
  xlab("Duration [days]") 
if (save) ggsave("../626fca86090ba51a6aff419a/plots/model1_postsigma.pdf", width = 7, height = 5)

plot_title <- ggtitle("Posterior distribution of mu1", "with median and 80% intervals")
mcmc_areas(posterior %>% select(mu1), 
           pars = c("mu1"), 
           prob = 0.8) + plot_title + 
  theme_gray() + ylab("Posterior Distribution") +
  xlab("Duration [days]")
if (save) ggsave("../626fca86090ba51a6aff419a/plots/model1_postmu1.pdf", width = 7, height = 5)

plot_title <- ggtitle("Posterior distribution of mu2", "with median and 80% intervals")
mcmc_areas(posterior %>% select(mu2), 
           pars = c("mu2"), 
           prob = 0.8) + plot_title + 
  theme_gray() + ylab("Posterior Distribution") +
  xlab("Duration [days]")
if (save) ggsave("../626fca86090ba51a6aff419a/plots/model1_postmu2.pdf", width = 7, height = 5)

plot_title <- ggtitle("Posterior distribution of p", "with median and 80% intervals")
mcmc_areas(posterior %>% select(p), 
           pars = c("p"), 
           prob = 0.8) + plot_title + 
  theme_gray() + ylab("Posterior Distribution") +
  xlab("Probability")
if (save) ggsave("../626fca86090ba51a6aff419a/plots/model1_postp.pdf", width = 7, height = 5)


### Model Checking. 
n <- dim(posterior)[[1]]
# We select the statistics 1st quart, median, mean and 3rd quart. 
statistic.distrs <- list(first = rep(NA, n), median = rep(NA, n), 
                         mean = rep(NA, n), third = rep(NA, n))
N <- 10000
for(i in 1:n){
  # Simulate from the posterior distribution using every simulated value from MCMC fit (Stan) once. 
  p <- posterior$p[i]
  mu1 <- posterior$mu1[i]
  mu2 <- posterior$mu2[i]
  sigma <- posterior$sigma[i]
  
  components <- sample(1:2,prob=c(p,1-p),size=N,replace=TRUE)
  mus <- c(mu1,mu2)
  sds <- c(sigma,sigma) 
  samples <- rnorm(N)*sds[components]+mus[components]

  # Simulate from the posterior distribution.
  q <- quantile(samples, c(0.25, 0.5, 0.75))
  statistic.distrs$first[i] <- q[1][[1]]
  statistic.distrs$median[i] <- q[2][[1]]
  statistic.distrs$mean[i] <- mean(samples)
  statistic.distrs$third[i] <- q[3][[1]]
}

# Compare with the same statistics in the data. The "data" in this case is the sampled data points
# used to fit the model fit Stan.
q.data <- quantile(dur, c(0.25, 0.5, 0.75)) 
stat.data <- list(first = q.data[1][[1]], median = q.data[2][[1]], 
                  mean = mean(dur), third = q.data[3][[1]])

df <- cbind(statistic.distrs$first, statistic.distrs$mean, statistic.distrs$median, statistic.distrs$third)
colnames(df) <- c("25%", "Mean", "50%", "75%")
                 
# Want to make nice ggplot with all four statistics (using facets).
df2 <- as.data.frame(df) %>% 
  gather(key = Statistic, value = value_dens)
df2 <- df2 %>% mutate(line = c(rep(stat.data$first, n), rep(stat.data$mean, n),
                               rep(stat.data$median, n), rep(stat.data$third, n)))

df2 %>% 
  ggplot() +
  geom_density(aes(x = value_dens, y = (..count..)/sum(..count..))) +
  geom_vline(aes(xintercept = line)) + 
  #facet_grid(rows = vars(Statistic), scales = "free")
  facet_wrap(~Statistic, scales = "free") + xlab("Duration [Days]") + ylab("Reference Distributions")
if (save) ggsave("../626fca86090ba51a6aff419a/plots/checkingModel1.pdf", width = 7, height = 5)
# Nice plot showing all the chosen statistics at the same time!

# Numerical calculations:
num.first <- min(mean(statistic.distrs$first < stat.data$first), mean(statistic.distrs$first > stat.data$first))
vec1 <- c(mean(statistic.distrs$first < stat.data$first), mean(statistic.distrs$first > stat.data$first), num.first)
num.mean <- min(mean(statistic.distrs$mean < stat.data$mean), mean(statistic.distrs$mean > stat.data$mean))
vec2 <- c(mean(statistic.distrs$mean < stat.data$mean), mean(statistic.distrs$mean > stat.data$mean), num.mean)
num.median <- min(mean(statistic.distrs$median < stat.data$median), mean(statistic.distrs$median > stat.data$median))
vec3 <- c(mean(statistic.distrs$median < stat.data$median), mean(statistic.distrs$median > stat.data$median), num.median)
num.third <- min(mean(statistic.distrs$third < stat.data$third), mean(statistic.distrs$third > stat.data$third))
vec4 <- c(mean(statistic.distrs$third < stat.data$third), mean(statistic.distrs$third > stat.data$third), num.third)

table.numerical.model1 <- cbind(vec1, vec2, vec3, vec4)
colnames(table.numerical.model1) <- c("25%", "Mean", "50%", "75%")
rownames(table.numerical.model1) <- c("Left", "Right", "min") 
xtable(as.data.frame(table.numerical.model1), digits = 5)
