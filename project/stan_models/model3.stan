data{
    int <lower=0> n;
    real y[n];
}

parameters{
    real mu1;
    real<lower=mu1> mu2;
    real<lower=0> sigma1;
    real<lower=0> sigma2;
    real<lower=0,upper=1> p;
    real<lower=0> nu;
    real<lower=0> gam;
}

model{
    mu1 ~ normal(120, sigma1);
    mu2 ~ normal(280, sigma2);
    sigma1 ~ inv_gamma(nu,gam);
    sigma2 ~ inv_gamma(nu,gam);
    nu ~ normal(1,10);
    gam ~ inv_gamma(1,1);
    p ~ uniform(0,1);
    for (i in 1:n) {
        target += log_sum_exp(
            log(p) + normal_lpdf(y[i] | mu1, sigma1),
            log1m(p) + normal_lpdf(y[i] | mu2, sigma2));
    }
}
