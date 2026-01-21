data {
  int<lower=1> N_species_obs;
  int<lower=1> N_complexes;
  int<lower=1> N_species;

  int<lower=0> r_species[N_species_obs];
  int<lower=0> N_species_[N_species_obs];
  int<lower=1> species_id[N_species_obs];

  int<lower=1, upper=N_complexes> species_complex[N_species];
}

parameters {
  real beta0_raw;

  vector[N_complexes] z1;
  vector[N_species]  z2;

  real<lower=0> tau0;
  real<lower=0> tau1;
  real<lower=0> tau2;
}

transformed parameters {
  real beta0;
  vector[N_complexes] beta1;
  vector[N_species]  beta2;

  beta0 = tau0 * beta0_raw;
  beta1 = beta0 + tau1 * z1;

  for (s in 1:N_species)
    beta2[s] = beta1[species_complex[s]] + tau2 * z2[s];
}

model {
  beta0_raw ~ normal(0, 1);

  tau0 ~ normal(0, 1);
  tau1 ~ normal(0, 1);
  tau2 ~ normal(0, 0.5);

  z1 ~ normal(0, 1);
  z2 ~ normal(0, 1);

  r_species ~ binomial_logit(N_species_, beta2[species_id]);
}

generated quantities {
  vector[N_complexes] p1;
  vector[N_species] p2;

  for (c in 1:N_complexes) {
    p1[c] = 1 / inv_logit(beta1[c]);

  }

  for (s in 1:N_species) {
    p2[s] = 1 / inv_logit(beta2[s]);

  }
}
