# Session 3: Comparison of 2 treatments


# Exercise 3.1: Burns

# a) Choose a priori distribution for every treatment according to the statement

#http://www.wolframalpha.com/
#solve(a/(a+b)=, sqrt((ab)/((a+b)^2*(a+b+1)))=)

## Prior distributions
delta_p <- 0.01
p <- seq(0, 1, delta_p)

# Conventional prior: 0.4 < p < 0.8
prior_par_c <- c(alpha = ___, beta = ___)
prior_c <- 

# Experimental prior: 0.6 < p < 0.9

  
# Plot prior comparison

  
  

# b) Draw the prior distribution, the posterior distribution and the likelihood
# function for every treatment in the same graph.

## Data
n <- 40
y_c <- 24
y_e <- 30


## Likelihood distributions


## Posterior distributions

#Plots



# > Exercise:
# Instead of using 2 aesthetics (linetype and color), 
# use facet_wrap to compare distributions.



# c) Draw the posterior distribution of the difference between rates of
# improvement.





# d) Compute the probability that the probability to improve using the
# experimental treatment is larger than using the conventional treatment.





# e) Compute and draw the posterior distribution for the Odds Ratio and give a
# 95% credible interval for it. Interpret the result.

# odds_ratio = p / (1-p)


# 95% credible interval


# Plot







# > Optional exercise:
#
# Redo the calculations using a normal priori