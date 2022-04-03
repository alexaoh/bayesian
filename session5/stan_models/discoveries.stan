data {
  int<lower=0> N;     // sample size
  int<lower=0> Y[N];  //number of invention
  real mu_prior;
  real sigma_prior;
}

parameters {
  real<lower=0> lambda;
}

model {
  Y ~ poisson(lambda);  	// likelihood
  
  lambda ~ lognormal(mu_prior, sigma_prior); // prior for lambda
}