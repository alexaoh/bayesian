## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
setwd("/home/ajo/gitRepos/bayesian/HW4")
library(rstan)
library(bayesplot)
library(tidyverse)


## --------------------------------------------------------------------------------------------------------------------------------
method.A <- c(115, 120, 111, 123, 116, 121, 118, 116, 127, 129)
method.B <- c(123, 131, 113, 119, 123, 113, 128, 126, 125, 128)
df <- rbind(c(method.A,mean(method.A), sd(method.A)), c(method.B,mean(method.B), sd(method.B)))
rownames(df) <- c("Method A", "Method B")
colnames(df) <- c(rep("", 10), "Mean", "Standard Error")


## ---- echo = F-------------------------------------------------------------------------------------------------------------------
knitr::kable(df, caption = "Times Taken to Complete Task for each Method")


## ---- cache = T, results='hide'--------------------------------------------------------------------------------------------------
# Define model and call stan. 
data_list <- list(
  nA = length(method.A),
  nB = length(method.B),
  tA = method.A,
  tB = method.B
)

fit <- stan("4-4_training_workers.stan", iter = 1000, chains = 4,
             data = data_list, seed = 1)


## --------------------------------------------------------------------------------------------------------------------------------
# Convergence analysis.
print(fit)
traceplot(fit)

posterior <- as.data.frame(fit)
head(posterior)          
dim(posterior)

post.muA <- posterior[,1]
post.muB <- posterior[,2]


## ---- echo = F-------------------------------------------------------------------------------------------------------------------
par(mfrow = c(2, 1))


## ---- fig.height=5---------------------------------------------------------------------------------------------------------------
acf(post.muA)
acf(post.muB)


## ---- echo = F-------------------------------------------------------------------------------------------------------------------
par(mfrow = c(1, 1))


## --------------------------------------------------------------------------------------------------------------------------------
plot_title <- ggtitle("Posterior distributions of mu", "with medians and 80% intervals")
mcmc_areas(posterior, 
  pars = c("muA", "muB"), 
  prob = 0.8) + plot_title


## ---- eval = F-------------------------------------------------------------------------------------------------------------------
## data{
##   int<lower=0> nA;
##   int<lower=0> nB;
## 
##   real<lower=0> tA[nA];
##   real<lower=0> tB[nB];
## }
## 
## parameters{
##   real muA;
##   real muB;
## }
## 
## model{
##   tA ~ normal(muA, 6);
##   tB ~ normal(muB, 6);
## 
##   muA ~ normal(100, 20);
##   muB ~ normal(100, 20);
## }


## --------------------------------------------------------------------------------------------------------------------------------
post.diff <- post.muA - post.muB
ggplot(tibble(post.diff)) +
  geom_density(aes(post.diff)) +
  theme_minimal() +
  ggtitle("Posterior Distribution of muA - muB")


## --------------------------------------------------------------------------------------------------------------------------------
(CI.perc <- quantile(post.diff, probs = c(0.025, 0.975)))


## ---- cache = T, results="hide"--------------------------------------------------------------------------------------------------
fit2 <- stan("4-4_training_workers2.stan", iter = 1000, chains = 4,
             data = data_list, seed = 1)


## --------------------------------------------------------------------------------------------------------------------------------
# Convergence analysis.
print(fit2)
traceplot(fit2)

posterior2 <- as.data.frame(fit2)
head(posterior2)          
dim(posterior2)

post.muA2 <- posterior2[,1]
post.muB2 <- posterior2[,2]
post.sigma2 <- posterior[,3]


## ---- echo = F-------------------------------------------------------------------------------------------------------------------
par(mfrow = c(3, 1))


## ---- fig.height=5---------------------------------------------------------------------------------------------------------------
acf(post.muA2)
acf(post.muB2)
acf(post.sigma2)


## ---- echo = F-------------------------------------------------------------------------------------------------------------------
par(mfrow = c(1, 1))


## --------------------------------------------------------------------------------------------------------------------------------
plot_title <- ggtitle("Case 1: Posterior distributions of mu", "with medians and 80% intervals")
mcmc_areas(posterior2, 
  pars = c("muA", "muB"), 
  prob = 0.8) + plot_title


## --------------------------------------------------------------------------------------------------------------------------------
plot_title <- ggtitle("Case 1: Posterior distribution of sigma", "with median and 80% interval")
mcmc_areas(posterior2, 
  pars = c("sigma"), 
  prob = 0.8) + plot_title


## --------------------------------------------------------------------------------------------------------------------------------
post.diff2 <- post.muA2 - post.muB2
ggplot(tibble(post.diff2)) +
  geom_density(aes(post.diff2)) +
  theme_minimal() +
  ggtitle("Case 1: Posterior Distribution of muA - muB")


## --------------------------------------------------------------------------------------------------------------------------------
(CI.perc2 <- quantile(post.diff2, probs = c(0.025, 0.975)))


## ---- eval = F-------------------------------------------------------------------------------------------------------------------
## data{
##   int<lower=0> nA;
##   int<lower=0> nB;
## 
##   real<lower=0> tA[nA];
##   real<lower=0> tB[nB];
## }
## 
## parameters{
##   real muA;
##   real muB;
##   real<lower=0> sigma;
## }
## 
## model{
##   tA ~ normal(muA, sigma);
##   tB ~ normal(muB, sigma);
## 
##   muA ~ normal(100, 20);
##   muB ~ normal(100, 20);
##   sigma ~ normal(6,100);
## }


## ---- cache = T, results="hide"--------------------------------------------------------------------------------------------------
fit3 <- stan("4-4_training_workers3.stan", iter = 1000, chains = 4,
             data = data_list, seed = 1)


## --------------------------------------------------------------------------------------------------------------------------------
print(fit3)
traceplot(fit3)

posterior3 <- as.data.frame(fit3)
head(posterior3)          
dim(posterior3)

post.muA3 <- posterior3[,1]
post.muB3 <- posterior3[,2]
post.sigma3 <- posterior3[,3]


## ---- echo = F-------------------------------------------------------------------------------------------------------------------
par(mfrow = c(3, 1))


## ---- fig.height=5---------------------------------------------------------------------------------------------------------------
acf(post.muA3)
acf(post.muB3)
acf(post.sigma3)


## ---- echo = F-------------------------------------------------------------------------------------------------------------------
par(mfrow = c(1, 1))


## --------------------------------------------------------------------------------------------------------------------------------
plot_title <- ggtitle("Case 2: Posterior distributions of mu", "with medians and 80% intervals")
mcmc_areas(posterior3, 
  pars = c("muA", "muB"), 
  prob = 0.8) + plot_title


## --------------------------------------------------------------------------------------------------------------------------------
plot_title <- ggtitle("Case 2: Posterior distribution of sigma", "with median and 80% interval")
mcmc_areas(posterior3, 
  pars = c("sigma"), 
  prob = 0.8) + plot_title


## --------------------------------------------------------------------------------------------------------------------------------
post.diff3 <- post.muA3 - post.muB3
ggplot(tibble(post.diff3)) +
  geom_density(aes(post.diff3)) +
  theme_minimal() +
  ggtitle("Case 2: Posterior Distribution of muA - muB")


## --------------------------------------------------------------------------------------------------------------------------------
(CI.perc3 <- quantile(post.diff3, probs = c(0.025, 0.975)))


## ---- eval = F-------------------------------------------------------------------------------------------------------------------
## data{
##   int<lower=0> nA;
##   int<lower=0> nB;
## 
##   real<lower=0> tA[nA];
##   real<lower=0> tB[nB];
## }
## 
## parameters{
##   real muA;
##   real muB;
##   real<lower=0> sigma;
## }
## 
## model{
##   tA ~ normal(muA, sigma);
##   tB ~ normal(muB, sigma);
## 
##   muA ~ normal(100, 20);
##   muB ~ normal(100, 20);
##   sigma ~ normal(6,10);
## }


## --------------------------------------------------------------------------------------------------------------------------------
df <- rbind(CI.perc, CI.perc2, CI.perc3)
rownames(df) <- c("Const. sigma", "Case 1", "Case 2")
knitr::kable(df)
