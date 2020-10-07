functions {
  vector gp_to_pars(real mu, real sigma, real[] x, real eta, real rho, vector z, int N) {
    matrix[N, N] K = cov_exp_quad(x, eta, rho) + diag_matrix(rep_vector(square(sigma), N));
    matrix[N, N] LK = cholesky_decompose(K);
    vector[N] out = rep_vector(mu, N) + LK * z;
    return(out);
  }
}


data {
  int<lower = 0> N_days;
  int<lower = 0> pred_days;
  int<lower = 0> local[N_days]; // Local new cases
  vector<lower = 0>[N_days] total; // Total new cases
  vector<lower=0>[N_days] prop_quarantine;
  real<lower = 0> SI_shape; // 1.54 calculated from Icelandic data (Flaxman et al. use 4.5)
  real<lower = 0> SI_rate; // 0.28 calculated from Icelandic data (Flaxman et al. use 0.6)
}

transformed data {
  vector[N_days] SI_rev;
  vector[N_days - 1] lambda;
  vector[N_days - 1] lambda_q;
  real day[N_days + pred_days - 1];
  
  // Calculate SI but put results in reverse vector for easy dot product calculations
  for (t in 1:N_days) {
    SI_rev[N_days - t + 1] = gamma_cdf(1.0 * t + 0.5, SI_shape, SI_rate) - gamma_cdf(1.0 * t - 0.5, SI_shape, SI_rate);
  }
  SI_rev = SI_rev / sum(SI_rev);
  
  // Calculate expectd number of new cases if R_t = 1 by convolving SI with past total new cases
  // The division is used such that the truncated SI at the start of the data sums to 1
  for (t in 2:N_days) {
    lambda[t - 1] = dot_product(head((1-prop_quarantine) .* total, t - 1), tail(SI_rev, t - 1) / sum(tail(SI_rev, t - 1)));
    lambda_q[t - 1] = dot_product(head(prop_quarantine .* total, t - 1), tail(SI_rev, t - 1) / sum(tail(SI_rev, t - 1)));
  }
  
  for (t in 1:(N_days + pred_days - 1)) {
    day[t] = t;
  }
}

parameters {
  // Want to put normal prior on sqrt(phi) where Var(x) = mu + phi * mu^2
  // Stan's NB family is parametrized as Var(x) = mu + 1/phi * mu^2 so we have to use inverse of phi.
  real<lower = 0> phi_inv_sqrt;
  
  // Standard deviation of random jumps in second order random walk for R_t
  
  // Need to estimate first two R_t for second order random walk
  real<lower = 0> R_q;
  real<lower = 0> rho;
  real<lower = 0> eta;
  real<lower = 0> sigma_R;
  // vector[N_days + pred_days - 1] z_R;
  real<lower = 0> mu_R;
  vector<lower = 0>[N_days + pred_days - 1] R;
} 

transformed parameters {
  // vector[N_days + pred_days - 1] log_R = gp_to_pars(mu_log_R, sigma_R, day, eta, rho, z_R, N_days + pred_days - 1);
  // vector[N_days + pred_days - 1] R = exp(log_R);
  real<lower = 0> phi = inv_sqrt(phi_inv_sqrt);
  
}

model {
  matrix[N_days + pred_days - 1, N_days + pred_days - 1] L_K;
  matrix[N_days + pred_days - 1, N_days + pred_days - 1] K = cov_exp_quad(day, eta, rho) + diag_matrix(rep_vector(square(sigma_R), N_days + pred_days - 1));
  L_K = cholesky_decompose(K);
  sigma_R ~ exponential(1);
  phi_inv_sqrt ~ exponential(1);
  R_q ~ normal(0.6, 1);
  R ~ multi_normal_cholesky(rep_vector(mu_R, N_days + pred_days - 1), L_K);
  // z_R ~ std_normal();
  eta ~ exponential(1);
  rho ~ inv_chi_square(1);
  
  local[2:N_days] ~ neg_binomial_2(R[1:(N_days - 1)] .* lambda + R_q * lambda_q, phi);
}

generated quantities {
  // Calculate R_t and y_hat for model checking
  int y_hat[N_days - 1] = neg_binomial_2_rng(R[1:(N_days - 1)] .* lambda + R_q * lambda_q, phi);
}


