data{
    int <lower=0> n;
    real y[n];
    real x1[n];
    real x2[n];
}

parameters{
    real<lower=0> sigma1;
    real<lower=0> sigma2;
    real<lower=0,upper=1> p;
    real<lower=0> nu;
    real<lower=0> gam;
    real beta10;
    real<lower=beta10> beta20;
    real beta11;
    real beta21;
    real beta12;
    real beta22;
    real mu1;
    real mu2;
    real gam1;
    real gam2;
}

model{
    beta10 ~ normal(120, sigma1);
    beta20 ~ normal(280, sigma2);
    beta21 ~ normal(mu1, gam1);
    beta22 ~ normal(mu2, gam2);
    mu1 ~ normal(1,10);
    mu2 ~ normal(1,10);
    gam1 ~ inv_gamma(1,1);
    gam2 ~ inv_gamma(1,1);
    sigma1 ~ inv_gamma(nu,gam);
    sigma2 ~ inv_gamma(nu,gam);
    nu ~ normal(1,10);
    gam ~ inv_gamma(1,1);
    p ~ uniform(0,1);
    for (i in 1:n) {
        target += log_sum_exp(
            log(p) + normal_lpdf(y[i] | beta10 + x1[i]*beta11 + x2[i]*beta21, sigma1),
            log1m(p) + normal_lpdf(y[i] | beta20 + x1[i]*beta21 + x2[i]*beta22, sigma2));
    }
}
