data{ 
  int <lower=0> n;
  real y[n];
}

parameters{
  real<lower=0,upper=1> p;
  real<lower=0> sigma1;
  real<lower=0> sigma2;
  real mu1;
  real mu2;
  real<lower=0> c;
  real<lower=0> d;
  real<lower=0> a;
  real<lower=0> b;
}

model{
  for (i in 1:n) {
  	target += log_sum_exp(log(p)
                          + normal_lpdf(y[i] | mu1, sigma1),
                        log1m(p)
                          + normal_lpdf(y[i] | mu2, sigma2));
  };
  p ~ beta(1,1);
  sigma1 ~ gamma(c, d);
  sigma2 ~ gamma(c, d); 
  mu1 ~ normal(a, b);
  mu2 ~ normal(a, b);
  a ~ gamma(0.01,0.01);
  b ~ gamma(0.01,0.01); 
  c ~ gamma(0.01,0.01);
  d ~ gamma(0.01,0.01);
}
