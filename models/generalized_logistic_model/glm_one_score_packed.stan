functions {
  real get_mu(int IDp, 
              int IDs, 
              int is_pbo, 
              real time, 
              int multiplicative_s, 
              int multiplicative_r, 
              vector X_s, 
              vector X_r,   // data
              real tau, 
              real beta, 
              real beta_pbo, 
              real k_el, 
              real k_eq, 
              vector theta_r, 
              vector theta_s, 
              real eta_pr, 
              real eta_sr, 
              real eta_ps, 
              real eta_ss, 
              real base_s, 
              real base_r) {            
      real mu;
      real cov_s = base_s + eta_ps + eta_ss;
      real cov_r = base_r + eta_pr + eta_sr;
      real S0;
      real r;
      real pbo_eff;
      if (rows(theta_s) != 0) cov_s += X_s' * theta_s;
      if (rows(theta_r) != 0) cov_r += X_r' * theta_r;
      if (multiplicative_s == 1) cov_s = exp(cov_s);
      if (multiplicative_r == 1) cov_r = exp(cov_r);
      S0 = inv_logit(cov_s);
      r = cov_r;

      pbo_eff = beta_pbo * (k_eq / (k_eq - k_el))  * (exp(-k_el * time) - exp(-k_eq * time));
      mu = S0 / (S0^beta + (1 - S0^beta) * exp(-beta * r * time))^(1 / beta) - is_pbo * pbo_eff;
    return mu;
  }
  
  
  real get_tau(int IDp, 
               int IDs, 
               int is_pbo, 
               real time, 
               int multiplicative_s, 
               int multiplicative_r, 
               vector X_s, 
               vector X_r,   // data
               real tau, 
               real beta, 
               real beta_pbo, 
               real k_el, 
               real k_eq, 
               vector theta_r, 
               vector theta_s, 
               real eta_pr, 
               real eta_sr, 
               real eta_ps, 
               real eta_ss, 
               real base_s, 
               real base_r) {            
      real mu;     
      real cov_s = base_s + eta_ps + eta_ss;
      real cov_r = base_r + eta_pr + eta_sr;
      real S0;
      real r;
      real pbo_eff;
      if (rows(theta_s) != 0) cov_s += X_s' * theta_s;
      if (rows(theta_r) != 0) cov_r += X_r' * theta_r;
      if (multiplicative_s == 1) cov_s = exp(cov_s);
      if (multiplicative_r == 1) cov_r = exp(cov_r);
      S0 = inv_logit(cov_s);
      r = cov_r;

      pbo_eff = beta_pbo * (k_eq / (k_eq - k_el))  * (exp(-k_el * time) - exp(-k_eq * time));
      mu = S0 / (S0^beta + (1 - S0^beta) * exp(-beta * r * time))^(1 / beta) - is_pbo * pbo_eff;
    return tau;
  }
}

data{
	int N;             			     // number of total Observations
  int P;              			   // number of subjects
	int M;                       // number of studies
	int Q_r;                     // number of features for r
	int Q_s;                     // number of features for S0
	int<lower=0,upper=1> multiplicative_s;        // multiplicative covariates instead of additive
	int<lower=0,upper=1> multiplicative_r;
	matrix[N,Q_r] X_r;           // features for rate
	matrix[N,Q_s] X_s;           // features for baseline
	int<lower=0,upper=1> is_pbo[M]; // is a placebo study
 	int IDp[N];            		   // patient ID
 	int IDs[N];            		   // study ID
  vector[N] time;      			   // time of observation
	vector[N] score1;          // measured scores
}

parameters{
  real<lower=0> tau1;
  real<lower=0> beta1;
  vector[Q_r] theta_r1; // coeff. for r
  vector[Q_s] theta_s1; // coeff. for S0

  // eta's
  vector[P] eta_ps1;
  vector[P] eta_pr1;
  vector[M] eta_ss1;
  vector[M] eta_sr1;
  real<lower=0> omega_ps1;
  real<lower=0> omega_pr1;
  real<lower=0> omega_ss1;
  real<lower=0> omega_sr1;

  // base baseline and rate
  real base_s1;
  real base_r1;

  // Bateman term
  real<lower=0> beta_pbo1;
  real<lower=0> k_el1;
  real<lower=0> delta1;
}

model{
  real tgt = 0;
  
  tau1 ~ normal(0, 10);
  beta1 ~ normal(0, 10);
  beta_pbo1 ~ normal(0, 10);
  k_el1 ~ normal(0, 10);
  delta1 ~ normal(0, 10);
  base_s1 ~ normal(0, 2);


  theta_r1 ~ normal(0, 10);
  theta_s1 ~ normal(0, 10);

  omega_ps1 ~ normal(0,10);
  omega_pr1 ~ normal(0,10);
  omega_ss1 ~ normal(0,10);
  omega_sr1 ~ normal(0,10);
  
  base_r1 ~ normal(0, 10);
  
  eta_ss1 ~ normal(0, omega_ss1);
  eta_ps1 ~ normal(0, omega_ss1);
  eta_pr1 ~ normal(0, omega_ss1);
  eta_sr1 ~ normal(0, omega_ss1);

  
  

  // Likelihood for first score
	tgt += generalized_logistic_model(IDp, IDs, is_pbo, time, score1, multiplicative_s, multiplicative_r, X_s, X_r,   // data
                  tau1, beta1, beta_pbo1, k_el1, k_el1 + delta1, theta_r1, theta_s1,
                  eta_pr1, eta_sr1, eta_ps1, eta_ss1, base_s1, base_r1);

  target += tgt;

}

generated quantities{
  vector[N] score1_pred;
  for (n in 1:N) {
    real mu_pred;
    real tau_pred;
    mu_pred = get_mu(IDp[n], 
                     IDs[n], 
                     is_pbo[IDs[n]], 
                     time[n], 
                     multiplicative_s, 
                     multiplicative_r, 
                     X_s[n, ]', 
                     X_r[n, ]',   // data
                     tau1, 
                     beta1, 
                     beta_pbo1, 
                     k_el1, 
                     k_el1 + delta1, 
                     theta_r1, 
                     theta_s1,
                     eta_pr1[IDp[n]], 
                     eta_sr1[IDs[n]], 
                     eta_ps1[IDp[n]], 
                     eta_ss1[IDs[n]], 
                     base_s1, 
                     base_r1);
    tau_pred = get_tau(IDp[n], 
                     IDs[n], 
                     is_pbo[IDs[n]], 
                     time[n], 
                     multiplicative_s, 
                     multiplicative_r, 
                     X_s[n, ]', 
                     X_r[n, ]',   // data
                     tau1, 
                     beta1, 
                     beta_pbo1, 
                     k_el1, 
                     k_el1 + delta1, 
                     theta_r1, 
                     theta_s1,
                     eta_pr1[IDp[n]], 
                     eta_sr1[IDs[n]], 
                     eta_ps1[IDp[n]], 
                     eta_ss1[IDs[n]], 
                     base_s1, 
                     base_r1);
    score1_pred[n] = beta_rng(mu_pred * tau_pred, (1 - mu_pred) * tau_pred);
  }
}

