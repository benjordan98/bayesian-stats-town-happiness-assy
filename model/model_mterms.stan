data {
  int<lower=1> n;       // total number of observation
  int<lower=0> m;       // number of independent variables
  matrix[n, m] X;       // independent variables
  vector[n] y;          // observations
}

parameters {
  vector[m] b;         // betas 
  real<lower=0> sigma; // stdev
}

model {
  // priors
  b ~ cauchy(0, 2.5);
  
  for (i in 1:n) {
    // storage for linear terms
    vector[m] mu;
    
    // calculate terms
    for (j in 1:m) {
      mu[j] = X[i,j] * b[j];
    }
    
    // model
    y[i] ~ normal(sum(mu), sigma);
  }
}

generated quantities {
  // log-likelihood
  vector[n] log_lik;
  
  for (i in 1:n) {
    // mu
    vector[m] mu;
     
    // calculate terms
    for (j in 1:m) {
      mu[j] = X[i,j] * b[j];
    }
     
    log_lik[i] = normal_lpdf(y[i] | sum(mu), sigma);
  }
}
