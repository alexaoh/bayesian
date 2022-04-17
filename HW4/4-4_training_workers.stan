data{ 
  int<lower=0> nA;
  int<lower=0> nB;
  
  real<lower=0> tA[nA];
  real<lower=0> tB[nB];
}

parameters{
  real muA;
  real muB;
}

model{
  tA ~ normal(muA, 6);
  tB ~ normal(muB, 6);
  
  muA ~ normal(100, 20);
  muB ~ normal(100, 20);
}

