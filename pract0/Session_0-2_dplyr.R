
# 1. Data Transformation --------------------------------------------------

# 1.1 Tibble --------------------------------------------------------------

library(ggplot2)
library(dplyr)
setwd("/home/ajo/gitRepos/bayesian/pract0")
df <- read.csv2("companies.csv")
df

df <- as_tibble(df)
df



# 1.2 dplyr verbs ---------------------------------------------------------

# 1.2.1 filter()
filter(df, City == "Barcelona")

filter(df, City == "Barcelona", Revenue > 3000)


# 1.2.2 arrange()
arrange(df, Revenue)

arrange(df, desc(Revenue))


# 1.2.3 select()
select(df, Company, City)

select(df, Company:Activity)

select(df, -c(Company, City))

select(df, starts_with("C"))

select(df, ends_with("y"))

select(df, contains("it"))


# 1.2.4 mutate()
mutate(df, 
  Rev_per_month = Revenue / 12, 
  Rev_per_week = Rev_per_month / 4)

transmute(df, Rev_per_month = Revenue / 12)


# 1.2.5 summarise()
summarise(df, n = n(), 
  Rev_total = sum(Revenue), 
  Rev_mean = mean(Revenue), 
  Tot_act = n_distinct(Activity))



# 1.3 %>% operator --------------------------------------------------------

df %>% 
	summarise(n = n(), 
	  Rev_total = sum(Revenue), 
	  Rev_mean = mean(Revenue), 
	  Tot_act = n_distinct(Activity))


# 1.3.1 group_by()
df %>% 
	group_by(City) %>% 
	summarise(n = n(), 
	  Rev_total = sum(Revenue), 
	  Rev_mean = mean(Revenue), 
	  Tot_act = n_distinct(Activity))


#' Exercise: 
#' How many companies are there in each city with a revenue> 3000 and how many with a revenue <3000?



# 2. Exercises ------------------------------------------------------------

# Tools for Bayesian Analysis - Exercises

# Exercise 1.1
set.seed(2020)
s1 <- rnorm(10000, mean = 5, sd = 3)
s2 <- rnorm(10000, mean = 7, sd = 1)
df <- tibble("s1" = s1, "s2" = s2) %>% mutate("prod" = s1*s2)

ggplot(df) +
  geom_density(aes(s1), fill = "blue", alpha = 0.5) +
  geom_density(aes(s2), fill = "red", alpha = 0.5) +
  geom_density(aes(prod), fill = "green", alpha = 0.5)

# Exercise 1.2
ggplot() +
  xlim(-5, 15) + 
  stat_function(fun = dnorm, aes(colour = "normal"), geom = "line",  color = "black") +
  stat_function(fun = dnorm, geom = "line",  args = list(mean = 7, sd = 3), color = "red") +
  theme_minimal()

# 3. tidyr package --------------------------------------------------------

kids <- tibble(
  room = c("P3", "P3", "P4", "P4", "P5", "P5"), 
  gender = c("boys", "girls", "boys", "girls", "boys", "girls"),
  amount = c(10, 15, 12, 13, 18, 7)
)
kids


# 3.1 spread()
library(tidyr)

kids_wide <- kids %>% 
  spread(key = gender, value = amount)

kids_wide


# 3.2 gather()
kids_long <- kids_wide %>% 
  gather(key = gender, value = amount, -room)

kids_long


#' Exercise 1 (revisited)
df2 <- df %>% 
  gather(key = sample, value = value)

ggplot(df2) +
  geom_density(aes(value, fill = sample), alpha = 0.5) 
