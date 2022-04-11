data{ 
  int<lower=0> n1;
  int<lower=0> n2;
  
  real<lower=0> t1[n1];
  real<lower=0> t2[n2];
}

parameters{
  real<lower=0> lambda1;
  real<lower=0> lambda2;
}

model{
  t1 ~ exponential(lambda1);
  t2 ~ exponential(lambda2);
  
  lambda1 ~ uniform(0, 1);
  lambda2 ~ uniform(0, 1);
}