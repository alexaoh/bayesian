data{ 
  int <lower=0> n;
  
  real y1[n];
  real y2[n];
}

parameters{
  real mu1;
  real mu2;
  real<lower=0> sigma1;
  real<lower=0> sigma2;
}

model{
  y1 ~ normal(mu1, sigma1);
  y2 ~ normal(mu2, sigma2);
  
  mu1 ~ normal(160, 30);
  mu2 ~ normal(160, 30);
  sigma1 ~ normal(0, 20);
  sigma2 ~ normal(0, 20); 
}
