data{
    int <lower=0> n;
    real y[n];
}

parameters{
    real mu1;
    real<lower=mu1> mu2;
    real<lower=0> sigma;
    real<lower=0,upper=1> p;
}

model{
    mu1 ~ normal(120, sigma);
    mu2 ~ normal(280, sigma);
    sigma ~ inv_gamma(10,100);
    p ~ uniform(0,1);
    for (i in 1:n) {
        target += log_sum_exp(
            log(p) + normal_lpdf(y[i] | mu1, sigma),
            log1m(p) + normal_lpdf(y[i] | mu2, sigma));
    }
}

generated quantities{
    real y_pred;
    if (bernoulli_rng(p) == 1) {
        y_pred = normal_rng(mu1, sigma);
    } else {
        y_pred = normal_rng(mu2, sigma);
    }
}
