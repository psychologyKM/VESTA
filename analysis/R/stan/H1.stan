data{
  int N_W; //number of fixations in "Narrative mode" condition
  int N_P; //number of fixations in "Process mode" condition
  int N_I; //number of fixations in "Creator mode" condition
  int K_W; //number of participants in "Narrative mode" condition
  int K_P; //number of participants in "Process mode" condition
  int K_I; //number of participants in "Creator mode" condition
  
  vector<lower=-pi(),upper=pi()>[N_W] Y_W; //degrees in "Narrative mode" condition
  vector<lower=-pi(),upper=pi()>[N_P] Y_P; //degrees in "Process mode" condition
  vector<lower=-pi(),upper=pi()>[N_I] Y_I; //degrees in "Creator mode" condition
  array[N_W] int<lower=1,upper=K_W+K_P+K_I> person_W; //participant's index in "Narrative mode" condition
  array[N_P] int<lower=1,upper=K_W+K_P+K_I> person_P; //participant's index in "Process mode" condition
  array[N_I] int<lower=1,upper=K_W+K_P+K_I> person_I; //participant's index in "Creator mode" condition
}

parameters{
  real<lower=-pi(),upper=pi()> mu0;
  real<lower=-pi(),upper=pi()> mu_PWdif;
  real<lower=-pi(),upper=pi()> mu_IWdif;
  vector<lower=-pi(),upper=pi()>[K_W] mu_W;
  vector<lower=-pi(),upper=pi()>[K_P] mu_P;
  vector<lower=-pi(),upper=pi()>[K_I] mu_I;

  real<lower=0> sig_mu; //variance of mu
  
  real<lower=0> kappa0; 
  real kappa_PWdif; 
  real kappa_IWdif; 
  vector<lower=0>[K_W] kappa_W;
  vector<lower=0>[K_P] kappa_P;
  vector<lower=0>[K_I] kappa_I;

  real<lower=0> sig_kappa; //variance of kappa
}

model{
  target += normal_lpdf(mu_W | mu0, sig_mu);
  target += normal_lpdf(mu_P | mu0+mu_PWdif, sig_mu);
  target += normal_lpdf(mu_I | mu0+mu_IWdif, sig_mu);
  target += normal_lpdf(kappa_W | kappa0, sig_kappa);
  target += normal_lpdf(kappa_P | kappa0+kappa_PWdif, sig_kappa);
  target += normal_lpdf(kappa_I | kappa0+kappa_IWdif, sig_kappa);
  
  target += von_mises_lpdf(Y_W | mu_W[person_W], kappa_W[person_W]);
  target += von_mises_lpdf(Y_P | mu_P[person_P], kappa_P[person_P]);
  target += von_mises_lpdf(Y_I | mu_I[person_I], kappa_I[person_I]);
}

generated quantities{
  array[N_W] real y_W;
  array[N_P] real y_P;
  array[N_I] real y_I;
  y_W = von_mises_rng(mu_W[person_W], kappa_W[person_W]);
  y_P = von_mises_rng(mu_P[person_P], kappa_P[person_P]);
  y_I = von_mises_rng(mu_I[person_I], kappa_I[person_I]);
}
