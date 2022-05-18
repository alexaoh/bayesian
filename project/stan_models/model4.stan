data{
    int <lower=0> n;
    real y[n];
    real x1[n];
    real x2[n];
}

parameters{
    real<lower=0> sigma;
    real<lower=0, upper=1> p;
    real beta01;
    real<lower=beta01>  beta02;
    real beta1;
    real beta2;
}

model{
    beta01 ~ normal(120, sigma);
    beta02 ~ normal(280, sigma);
    beta1 ~ normal(0,100);
    beta2 ~ normal(0,100);
    sigma ~ inv_gamma(1,1);
    p ~ uniform(0,1);
    for (i in 1:n) {
        target += log_sum_exp(
            log(p) + normal_lpdf(y[i] | beta01 + beta1*x1[i] + beta2*x2[i], sigma),
            log1m(p) + normal_lpdf(y[i] | beta02 + beta1*x1[i] + beta2*x2[i], sigma));
    }
}

generated quantities {
    real y_pred;
    if (bernoulli_rng(p) == 1) {
        y_pred = normal_rng(beta01 + mean(x1)*beta1 + mean(x2)*beta2, sigma);
    } else {
        y_pred = normal_rng(beta02 + mean(x1)*beta1 + mean(x2)*beta2, sigma);
    }
}

