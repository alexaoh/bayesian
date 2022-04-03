data { 
  int N;
  int<lower=0> x[N];
}

parameters {
  real<lower=0> mu;
  real<lower=0> kappa;
}

model {
  x ~ neg_binomial_2(mu, kappa);

  mu ~ lognormal(2, 1);
  kappa ~ lognormal(2, 1);
}