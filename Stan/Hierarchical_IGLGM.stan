functions {
  real partial_sum(int[] slice_wave,
                    int start, int end,
                    vector days,
                    int[] new_cases,
                    int[] wave_start,
                    int[] wave_stop,
                    int[] wave_length,
                    int N_waves,
                    vector log_population,
                    vector log_phi_inv,
                    real mu_phi_inv,
                    real sigma_phi_inv,
                    matrix pre_pars,
                    vector mu_pars,
                    matrix cov_mat) {
                      real out = 0;
                      vector[N_waves] alpha = exp(to_vector(pre_pars[1]));
                      vector[N_waves] log_beta = to_vector(pre_pars[2]);
                      vector[N_waves] beta = exp(log_beta);
                      vector[N_waves] nu = exp(to_vector(pre_pars[3]));
                      vector[N_waves] log_S = log_inv_logit(to_vector(pre_pars[4]));
                      
                      out += normal_lpdf(log_phi_inv[start:end] | mu_phi_inv, sigma_phi_inv);
                      
                      for (i in start:end) {
                        out += multi_normal_cholesky_lpdf(pre_pars[, i] | mu_pars, cov_mat);
                        out += neg_binomial_2_log_lpmf(new_cases[wave_start[i]:wave_stop[i]] | 
                                    rep_vector(log_beta[i], wave_length[i]) +
                                    rep_vector(log_S[i], wave_length[i]) +
                                    beta[i] * (days[wave_start[i]:wave_stop[i]] - rep_vector(alpha[i], wave_length[i])) -
                                    (inv(nu[i]) + 1) * 
                                    log(1 + nu[i] * exp(beta[i] * (days[wave_start[i]:wave_stop[i]] - rep_vector(alpha[i], wave_length[i])))) +
                                    rep_vector(log_population[i], wave_length[i]), 
                                  exp(-log_phi_inv[i]));
                      }
                      return out;
                    }
}

data {
  int<lower = 1> N_obs;
  int<lower = 1> N_waves;
  int<lower = 1> N_locations;
  // location[N_obs] takes on value i for location number i etc.
  // int wave[N_obs];
  // days is how many days since prevalence exceeded a limit we have chosen
  vector[N_obs] days;
  // We model the daily number of newly diagnosed cases instead of cumulative numbers
  int new_cases[N_obs];
  
  vector[N_waves] log_population;
  int location[N_waves];
  int wave_start[N_waves];
  int wave_stop[N_waves];
  int wave_length[N_waves];
  int<lower = 1> grainsize;
}

transformed data {
  // vector[N_locations] log_population = log(population);
  int wave[N_waves];
  vector[4] mu_priors;
  mu_priors[1] = 3.0;
  mu_priors[2] = -2.5;
  mu_priors[3] = 0.0;
  mu_priors[4] = -4.0;
  
  for (i in 1:N_waves) wave[i] = i;
}

parameters {
  
  cholesky_factor_corr[4] L_Omega;
  matrix[4, N_waves] pre_pars;
  vector<lower = 0>[4] sigma_pars;
  vector[4] mu_pars;
  
  real mu_phi_inv;
  real<lower = 0> sigma_phi_inv;
  vector[N_waves] log_phi_inv;
}


model {
  // Logistic equation calculations
  matrix[4, 4] cov_mat = diag_pre_multiply(sigma_pars, L_Omega);
  
  // Multivariate Normal prior for the GLGM parameters
  target += normal_lpdf(mu_pars | mu_priors, 1);
  // mu_pars ~ normal(mu_priors, 1);
  target += exponential_lpdf(sigma_pars | 1);
  // sigma_pars ~ exponential(1);
  // L_Omega ~ lkj_corr_cholesky(2);
  target += lkj_corr_cholesky_lpdf(L_Omega | 2);  
  
  // Centered prior for the overdispersion
  // mu_phi_inv ~ normal(-1, 1);
  target += normal_lpdf(mu_phi_inv | -1, 1);
  // sigma_phi_inv ~ exponential(1);
  target += exponential_lpdf(sigma_phi_inv | 1);
  // log_phi_inv ~ normal(mu_phi_inv, sigma_phi_inv);
  // target += normal_lpdf(log_phi_inv | mu_phi_inv, sigma_phi_inv);
  
  target += reduce_sum(partial_sum, wave, grainsize,
                        days, new_cases,  
                        wave_start, wave_stop, wave_length, N_waves, log_population, 
                        log_phi_inv, mu_phi_inv, sigma_phi_inv,
                        pre_pars, mu_pars, cov_mat);
}

generated quantities {
  matrix[4, 4] corr_mat = multiply_lower_tri_self_transpose(L_Omega);
  real<lower = 0> sigma_alpha = sigma_pars[1];
  real<lower = 0> sigma_beta = sigma_pars[2];
  real<lower = 0> sigma_nu = sigma_pars[3];
  real<lower = 0> sigma_S = sigma_pars[4];
  real mu_alpha = mu_pars[1];
  real mu_beta = mu_pars[2];
  real mu_nu = mu_pars[3];
  real mu_S = mu_pars[4];
  vector<lower = 0>[N_waves] alpha = exp(to_vector(pre_pars[1]));
  vector<lower = 0>[N_waves] beta = exp(to_vector(pre_pars[2]));
  vector<lower = 0>[N_waves] nu = exp(to_vector(pre_pars[3]));
  vector<lower = 0, upper = 1>[N_waves] S  = inv_logit(to_vector(pre_pars[4]));
  vector<lower = 0>[N_waves] phi = exp(-log_phi_inv);
}
