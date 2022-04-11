data{ 
  int<lower=0> nA;
  int<lower=0> nB;
  
  real<lower=0> tA[nA];
  real<lower=0> tB[nB];
}

parameters{
  real muA;
  real muB;
  real<lower=0> sigma;
}

model{
  tA ~ normal(muA, sigma);
  tB ~ normal(muB, sigma);
  
  muA ~ normal(100, 20);
  muB ~ normal(100, 20);
  sigma ~ normal(6,10);
}
