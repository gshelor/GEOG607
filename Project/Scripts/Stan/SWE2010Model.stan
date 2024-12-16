//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] peak_swe;
  vector[N] tmean_CDM_sum;
  vector[N] prcpSum_CDM_sum;
  vector[N] srad_mean;
  vector[N] tmin_mean;
  vector[N] tmax_mean;
  vector[N] elevation;
  vector[N] slope;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real b0;
  real b1;
  real b2;
  real b3;
  real b4;
  real b5;
  real b6;
  real b7;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  peak_swe ~ normal(b0 + b1 * tmean_CDM_sum + b2 * prcpSum_CDM_sum + b3 * srad_mean + b4 * tmin_mean + b5 * tmax_mean + b6 * elevation + b7 * slope, sigma);
}

