data {
  int<lower=1> N_species_obs;
  int<lower=1> N_complexes;
  int<lower=1> N_species;

  int<lower=0> r_species[N_species_obs];
  int<lower=0> N_species_[N_species_obs];
  int<lower=1> species_id[N_species_obs];

  int<lower=0, upper=N_complexes> species_complex[N_species];
}

parameters {
  real beta0;
  vector[N_complexes] beta1;
  vector[N_species] beta2;

  real<lower=0> tau0;
  real<lower=0> tau1;
  real<lower=0> tau2;
}

model {
  // Priors
  tau0 ~ cauchy(0, 2.5);
  tau1 ~ cauchy(0, 2.5);
  tau2 ~ cauchy(0, 0.5);

  beta0 ~ normal(0, tau0);
  beta1 ~ normal(beta0, tau1);

  for (s in 1:N_species) {
      beta2[s] ~ normal(beta1[species_complex[s]], tau2);
  }

  // Likelihood: species only
  for (j in 1:N_species_obs) {
    r_species[j] ~ binomial_logit(N_species_[j], beta2[species_id[j]]);
  }
}

generated quantities {
  vector[N_complexes] p1;
  vector[N_species] p2;

  for (c in 1:N_complexes) {
    p1[c] = inv_logit(beta1[c]);
  }

  for (s in 1:N_species) {
    p2[s] = inv_logit(beta2[s]);
  }
}
