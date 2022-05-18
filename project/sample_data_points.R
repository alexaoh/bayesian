# Sample 10k rows from the data set for modelling with Stan.
# We sample only 10k rows for computational purposes. 

set.seed(1234)
setwd("/home/ajo/gitRepos/bayesian/project")
# Read the data that was cleaned in "erasmus.R".
data <- read.csv("cleaned.csv")
summary(data)
dim(data)

# Change the data types again. 
data <- data %>% transmute(duration = as.numeric(duration),
                           age = as.numeric(age),
                           gender = as.factor(gender),
                           nationality = as.factor(nationality),
                           sending.country = as.factor(sending.country),
                           receiving.country = as.factor(receiving.country),
                           activity = as.factor(activity),
                           participants = participants)

describe(data)

# Sample 10k rows. 
n <- 15000
indices <- sample(1:dim(data)[[1]], size = n)
new.sample <- data %>% slice(indices)
saveRDS(new.sample, file = "15kpoints.rds")
# This variable can be loaded from this file into the other files!

# Make table of quantiles in the duration data, will be used for model checking later. 
print(xtable(summary(subset(new.sample, select=-c(activity,participants)))))
