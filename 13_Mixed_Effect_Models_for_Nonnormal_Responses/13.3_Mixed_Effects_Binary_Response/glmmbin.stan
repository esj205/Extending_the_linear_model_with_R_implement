data{
  int<lower=0> Nobs;
  int<lower=0> Nsubs;
  int<lower=0> Npreds;
  int<lower=0, upper=1> y[Nobs];
  int<lower=1, upper=Nsubs> subject[Nobs];
  matrix[Nobs, Npreds] x;
}

parameters {
  vector[Nsubs] subeff;
  real<lower=0> sigmasubj;
  vector[Npreds] beta;
}

model {
  subeff ~ normal(0, sigmasubj);
  sigmasubj ~ cauchy(0, 1);
  for(n in 1:Nobs){
    y[n] ~ bernoulli_logit(x[n]*beta + subeff[subject[n]] );
  }
}
