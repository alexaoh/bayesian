# Session 7

library(rstan)
library(bayesplot)
library(tidyverse)


# Exercise 4.1 - Suicides -------------------------------------------------

# a) p point estimate

## Stan model
data_list <- list(
  n=874,
  y=103
)

fit2 <- stan("stan_models/4-1_suicides.stan", iter = 1000, chains = 4,
             data = data_list, seed = 1, thin=2)

## Convergence analysis
print(fit2)

posterior <- as.data.frame(fit2)


### Autocorrelation plot
acf(posterior[, "p"])

### Samples vs. iteration plot
traceplot(fit2)

### Posterior distribution
p <- posterior[, "p"]
ggplot(tibble(p)) +
  geom_density(aes(p))


plot_title <- ggtitle("Posterior distributions of p", "with medians and 80% intervals")
mcmc_areas(posterior, 
           pars = c("p"), 
           prob = 0.8) + plot_title


## Point estimate:
mean(p)
median(p)


# b) posterior predictive: y
n_sim <- 10000
post_predict <- rbinom(n_sim, 100, p)

ggplot(tibble(post_predict)) +
  geom_bar(aes(x = post_predict)) +
  ggtitle("Posterior predictive of y", "for 100 missing patients")

## credibility interval
quantile(post_predict, c(0.025, 0.975))



# Exercise 4.10 - Leukemia ------------------------------------------------

# a) p point estimate

## data
t1 <- c(65, 156, 100, 134, 16, 108, 121, 4, 39, 143, 56, 26, 22, 1, 1, 5, 65)

t2 <- c(56, 65, 17, 7, 16, 22, 3, 4, 2, 3, 8, 4, 3, 30, 4, 43)

t <- rexp(10000, 1/200)
plot(density(t))

## Stan model
data_list <- list(
  n1=length(t1),
  n2=length(t2),
  t1=t1,
  t2=t2
)
fit3 <- stan("stan_models/4-10_leukemia.stan", iter = 1000, chains = 4,
             data = data_list, seed = 1)

## Convergence analysis
print(fit3)

posterior <- as.data.frame(fit3)
colnames(posterior)

### Autocorrelation plot
par(mfrow = c(2, 1))
acf(posterior[, "lambda1"])
acf(posterior[, "lambda2"])
par(mfrow = c(1, 1))

### Samples vs. iteration plot
traceplot(fit3)

plot_title <- ggtitle("Posterior distributions of lambda", "with medians and 80% intervals")
mcmc_areas(posterior, 
  pars = c("lambda1", "lambda2"), 
  prob = 0.8) + plot_title


## Survival function: S(t) = 1 - F(t) = exp(-lambda t)
lambda1 <- posterior[, "lambda1"]
lambda2 <- posterior[, "lambda2"]

# Lambda Point estimate
lambda1_point <- median(lambda1)
lambda2_point <- median(lambda2)

tiempo <- seq(1, 150, 1)

df <- tibble(tiempo = tiempo) %>% 
  mutate(
    superv_1 = exp(- lambda1_point * tiempo),
    superv_2 = exp(- lambda2_point * tiempo)
  )

df %>% 
  gather("superv", "valor", -tiempo) %>% 
  ggplot() +
  geom_line(aes(tiempo, valor, col=superv))


# Confidence interval:
lambda1_est <- quantile(lambda1, probs = c(0.025, 0.975))
lambda2_est <- quantile(lambda2, probs = c(0.025, 0.975))

# df <- df %>% 
#   mutate(
#     L_1 = exp(- median(lambda1_est[1]) * time),    
#     U_1 = exp(- median(lambda1_est[2]) * time),    
#     L_2 = exp(- median(lambda2_est[1]) * time),    
#     U_2 = exp(- median(lambda2_est[2]) * time),    
#   )

# df_long <- df  %>% 
#   gather("survival", "value", -time) %>% 
#   separate(survival, c("survival", "color"), sep="_")

# ggplot(df_long %>% 
#   arrange(survival, color, time)) +
#   geom_line(aes(time, value, col = color))

aux <- map(c(lambda1_est, lambda2_est), ~ exp(- .x * time))
aux <- data.frame(matrix(unlist(aux), ncol=length(aux)))
colnames(aux) <- c("L1", "U1", "L2", "U2")

df_long <- df  %>% 
  cbind(aux) %>% 
  gather("survival", "value", -time) %>% 
  cbind(color = rep(c(1, 2, 1, 1, 2, 2), each = length(time))) %>% 
  cbind(tipo  = rep(c(1, 1, 2, 2, 2, 2), each = length(time)))


ggplot(df_long) +
  geom_line(aes(time, value, 
                group = survival, col = I(color), linetype = I(tipo)))



# b) Survival function: S(t) = 1- F(t)
diff24 <- 
  

# b) Survival function: S(t) = 1- F(t)
# 95% CI difference of 24-week
dif_24w <- exp(-lambda1*24)-exp(-lambda2*24)
quantile(dif_24w, c(0.025, 0.975))






# Exercise 4.12 - Basquet -------------------------------------------------

# Visualize y simulated with our prior (prior predictive)
lambda_sim <- runif(10000, 50, 400)
y <- rpois(10000, lambda_sim)
hist(lambda_sim)
hist(y)

lambda_sim <- rnorm(10000, 160, 30)
y <- rpois(10000, lambda_sim)
hist(lambda_sim)
hist(y)

# Definir datos
df <- read.table("data/basquet.txt", header = TRUE)

# Regression acb-nba: no paired data
# ggplot(df) +
#  geom_point(aes(acb,nba))

data_list <- list(
  n=nrow(df),
  y1=df$acb,
  y2=df$nba
)
fit <- stan("stan_models/4-12_basquet_1.stan", iter = 1000, chains = 4,
            data = data_list, seed = 1)

## Convergence analysis
print(fit)

posterior <- as.data.frame(fit)
colnames(posterior)

### Autocorrelation plot
par(mfrow = c(2, 1))
acf(posterior[, "lambda1"])
acf(posterior[, "lambda2"])
par(mfrow = c(1, 1))

### Samples vs. iteration plot
traceplot(fit, inc_warmup = TRUE)

plot_title <- ggtitle("Posterior distributions of lambda", "with medians and 80% intervals")
mcmc_areas(posterior, 
           pars = c("lambda1", "lambda2"), 
           prob = 0.8) + plot_title


# Diferencia = 30
lambda1 <- posterior[, "lambda1"]
lambda2 <- posterior[, "lambda2"]

y1_sim <- rpois(10000, lambda1)
y2_sim <- rpois(10000, lambda1)

mean((y2_sim - y1_sim) == 30)
mean((y2_sim - y1_sim) > 30)

# Estimacion puntual de los parametros
mean(lambda1)
median(lambda1)



# Definir priori para modelo normal
mu_sim <- rnorm(10000, 160, 30)
sigma_sim <- rnorm(30000, 0, 20)
sigma_sim <- sigma_sim[sigma_sim > 0][1:10000]
y <- rnorm(10000, mu_sim, sigma_sim)
hist(y)


fit2 <- stan("stan_models/4-12_basquet_2.stan", iter = 1000, chains = 4,
             data = data_list, seed = 1)

# P[29.5 < X < 30.5]
