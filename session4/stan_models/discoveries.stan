data {
  int N;      // sample size
  int Y[N]; // discoveries in the data
}
 
parameters {
  real<lower=0> lambda; // rate of discovery
}

model { 
  Y ~ poisson(lambda); // likelihood
  lambda ~ lognormal(2,1); // prior for lambda
}

generated quantities {
  vector[N] lSimData;
  int aMax_indicator;
  int aMin_indicator;
  
  // Generate posterior predictive samples
  for (i in 1:N) {
    lSimData[i] = poisson_rng(lambda);  
  }
  
  // Compare with real data
  aMax_indicator = max(lSimData) > max(Y);
  aMin_indicator = min(lSimData) > min(Y);
}
