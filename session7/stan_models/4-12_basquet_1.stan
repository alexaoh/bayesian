data{ 
  int <lower=0> n;
  
  int y1[n];
  int y2[n];
}

parameters{
  real<lower=0> lambda1;
  real<lower=0> lambda2;
}

model{
  y1 ~ poisson(lambda1);
  y2 ~ poisson(lambda2);
  
  lambda1 ~ normal(160, 30);
  lambda2 ~ normal(160, 30);
}
