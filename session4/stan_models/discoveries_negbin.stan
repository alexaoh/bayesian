data {
  int N;      // sample size
  int Y[N]; // discoveries in the data
}
 
parameters {
  real<lower=0> mu; // mean number of discoveries per year
  real<lower=0> kappa; // over-dispersion
}

model { 
  Y ~ neg_binomial_2(mu, kappa); // likelihood
  mu ~ lognormal(2,1); // prior for mu
  kappa ~ lognormal(2,1); // prior for kappa
}

