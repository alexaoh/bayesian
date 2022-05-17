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
    sigma1 ~ inv_gamma(1,1);
    sigma2 ~ inv_gamma(1,1);
    p ~ uniform(0,1);
    for (i in 1:n) {
        target += log_sum_exp(
            log(p) + normal_lpdf(y[i] | beta01 + beta1*x1[i] + beta2*x2[i], sigma1),
            log1m(p) + normal_lpdf(y[i] | beta02 + beta1*x1[i] + beta2*x2[i], sigma2));
    }
}

generated quantities {
    real y_pred[n];
    y_pred = p*normal_rng(beta01 + beta1*x1 + beta2*x2, sigma1) + (1- p)*normal_rng(beta02 + beta1*x1 + beta2*x2, sigma2);
}
