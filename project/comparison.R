# Make LaTeX tables for comparing model statistics between data and models. 

setwd("/home/ajo/gitRepos/bayesian/project/models")

# Load the fit of each model and save the posterior. 
fit1 <- readRDS("../model1_FIT15k.rds")
fit2 <- readRDS("../model2_FIT15k.rds") 
fit3 <- readRDS("../model3_FIT15k.rds") 
fit4 <- readRDS("../model4_FIT15k.rds") 

post1 <- as.data.frame(fit1)
post2 <- as.data.frame(fit2)
post3 <- as.data.frame(fit3)
post4 <- as.data.frame(fit4)

# Load data. 
data <- readRDS("../15kpoints.rds") # Load the sampled data. 

# We make tables summarizing the chosen statistics for model checking
# from each model and the same statistics for our data. 
nam <- c("25%", "50%", "75%", "Mean")

q.data <- c(quantile(data$duration, c(0.25, 0.5, 0.75), names = T), mean(data$duration))
names(q.data) <- nam
#q.data <- as.data.frame(q.data)
#knitr::kable(q.data)
#xtable(as.data.frame(q.data), digits = 5)

q.sim1 <- c(quantile(post1$y_pred, c(0.25, 0.5, 0.75), names = T), mean(post1$y_pred))
names(q.sim1) <- nam

q.sim2 <- c(quantile(post2$y_pred, c(0.25, 0.5, 0.75), names = T), mean(post2$y_pred))
names(q.sim2) <- nam

q.sim3 <- c(quantile(post3$y_pred, c(0.25, 0.5, 0.75), names = T), mean(post3$y_pred))
names(q.sim3) <- nam

q.sim4 <- c(quantile(post4$y_pred, c(0.25, 0.5, 0.75), names = T), mean(post4$y_pred))
names(q.sim4) <- nam

comp1 <- rbind(q.data, q.sim1, q.sim2, q.sim3, q.sim4)
rownames(comp1) <- c("Data", "Model 1", "Model 2", "Model 3", "Model 4")
xtable(as.data.frame(comp1), digits = 5)

# Make another table calculating relative errors of the statistics from each of 
# the models compared to the statistics in the data. 
comp2 <- rbind(q.data, (q.sim1-q.data)/q.data*100, (q.sim2-q.data)/q.data*100, 
               (q.sim3-q.data)/q.data*100, (q.sim4-q.data)/q.data*100)
comp2 <- as.data.frame(rbind(comp2, apply(abs(comp2), 2, which.min) - 1))
rownames(comp2) <- c("Data", "Model 1", "Model 2", "Model 3", "Model 4", "Lowest (Model)")
xtable(as.data.frame(comp2), digits = 5)
