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
}

model{
  for (i in 1:n) {
  	target += log_sum_exp(log(p)
                          + normal_lpdf(y[i] | mu1, sigma1),
                        log1m(p)
                          + normal_lpdf(y[i] | mu2, sigma2));
  };
  p ~ beta(1,1);
  sigma1 ~ normal(5, 200);
  sigma2 ~ normal(5, 200); //Tester for å se om den kompilerer.
  mu1 ~ normal(120, 3);
  mu2 ~ normal(280, 3);
}
