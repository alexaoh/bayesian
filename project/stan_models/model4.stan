data{
    int <lower=0> n;
    real y[n];
    real x1[n];
    real x2[n];
}

parameters{
    real<lower=0> sigma1;
    real<lower=sigma1> sigma2;
    real<lower=0, upper=1> p;
    real beta01;
    real<lower=beta01>  beta02;
    real beta1;
    real beta2;
}

model{
    beta01 ~ normal(120, sigma1);
    beta02 ~ normal(280, sigma2);
    beta1 ~ normal(0,100);
    beta2 ~ normal(0,100);
    sigma1 ~ inv_gamma(10,1000);
    sigma2 ~ inv_gamma(10,1000);
    p ~ uniform(0,1);
    for (i in 1:n) {
        target += log_sum_exp(
            log(p) + normal_lpdf(y[i] | beta01 + beta1*x1[i] + beta2*x2[i], sigma1),
            log1m(p) + normal_lpdf(y[i] | beta02 + beta1*x1[i] + beta2*x2[i], sigma2));
    }
}

generated quantities {
    real y_pred;
    for (i in 1:n) {
    if (bernoulli_rng(p) == 1) {
        y_pred = normal_rng(beta01 + x1[i]*beta1 + x2[i]*beta2, sigma1);
    } else {
        y_pred = normal_rng(beta02 + x1[i]*beta1 + x2[i]*beta2, sigma2);
    }}
}

