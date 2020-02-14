functions {
  real my_func(int[] IDp, 
               int[] IDs, 
               int[] is_pbo, 
               vector time, 
               vector score, 
               int multiplicative_s, 
               int multiplicative_r, 
               matrix X_s, 
               matrix X_r,   // data
               real tau, 
               real beta, 
               real beta_pbo, 
               real k_el, 
               real k_eq, 
               vector theta_r, 
               vector theta_s, 
               vector eta_pr, 
               vector eta_sr, 
               vector eta_ps, 
               vector eta_ss, 
               real base_s, 
               real base_r) {
    // setup
    int N = size(IDp);
    vector[N] muS;
    real tgt = 0;
    for (i in 1:N) {

      real cov_s = base_s + eta_ps[IDp[i]] + eta_ss[IDs[i]];
      real cov_r = base_r + eta_pr[IDp[i]] + eta_sr[IDs[i]];
      real S0;
      real r;
      real pbo_eff;
      if (rows(theta_s) != 0) cov_s += X_s[i] * theta_s;
      if (rows(theta_r) != 0) cov_r += X_r[i] * theta_r;
      if (multiplicative_s == 1) cov_s = exp(cov_s);
      if (multiplicative_r == 1) cov_r = exp(cov_r);
      S0 = inv_logit(cov_s);
      r = cov_r;

      pbo_eff = beta_pbo * (k_eq / (k_eq - k_el))  * (exp(-k_el * time[i]) - exp(-k_eq * time[i]));
      muS[i] = S0 / (S0^beta + (1 - S0^beta) * exp(-beta * r * time[i]))^(1 / beta) - is_pbo[IDs[i]] * pbo_eff;
    }
    return  beta_lpdf(score | muS * tau, (1 - muS) * tau);
  }
  
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

	int N2;             			     // number of total Observations
  int P2;              			   // number of subjects
	int M2;                       // number of studies
	int Q_r2;                     // number of features for r
	int Q_s2;                     // number of features for S0
	int<lower=0,upper=1> multiplicative_s2;        // multiplicative covariates instead of additive
	int<lower=0,upper=1> multiplicative_r2;
	matrix[N2,Q_r2] X_r2;           // features for rate
	matrix[N2,Q_s2] X_s2;           // features for baseline
	int<lower=0,upper=1> is_pbo2[M2]; // is a placebo study
 	int IDp2[N2];            		   // patient ID
 	int IDs2[N2];            		   // study ID
  vector[N2] time2;      			   // time of observation
	vector[N2] score2;          // measured scores
	
	
	// FOR MAPPING ETAS
	int Pn; // Number of patients that have both scores
	int Sn; // Number of studies that have both scores
	int patient_map[Pn,2]; // Mapping of patients with both scores
	int study_map[Sn,2]; // Mapping of studies with both scores
	int other_patients1[P - Pn]; // Patients with only score1
	int other_patients2[P2 - Pn]; // Patients with only score2
	int other_studies1[M - Sn]; // Studies with only score1
	int other_studies2[M2 - Sn]; // Studies with only score2
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
  
  
  
  real<lower=0> tau2;
  real<lower=0> beta2;
  vector[Q_r2] theta_r2; // coeff. for r
  vector[Q_s2] theta_s2; // coeff. for S0

  // eta's
  vector[P2] eta_ps2;
  vector[P2] eta_pr2;
  vector[M2] eta_ss2;
  vector[M2] eta_sr2;
  real<lower=0> omega_ps2;
  real<lower=0> omega_pr2;
  real<lower=0> omega_ss2;
  real<lower=0> omega_sr2;

  // base baseline and rate
  real base_s2;
  real base_r2;

  // Bateman term
  real<lower=0> beta_pbo2;
  real<lower=0> k_el2;
  real<lower=0> delta2;
  
  
  // Correlations
  corr_matrix[2] Omega_ps;
  corr_matrix[2] Omega_pr;
  corr_matrix[2] Omega_ss;
  corr_matrix[2] Omega_sr;
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
  omega_ps2 ~ normal(0,10);
  omega_pr2 ~ normal(0,10);
  omega_ss2 ~ normal(0,10);
  omega_sr2 ~ normal(0,10);
  
  base_r1 ~ normal(0, 10);


  tau2 ~ normal(0, 10);

  beta2 ~ normal(0, 10);
  beta_pbo2 ~ normal(0, 10);
  k_el2 ~ normal(0, 10);
  delta2 ~ normal(0, 10);
  base_s2 ~ normal(0, 2);

  theta_r2 ~ normal(0, 10);
  theta_s2 ~ normal(0, 10);
  
  base_r2 ~ normal(0, 10);
  
  // Correlations
  Omega_pr ~ lkj_corr(2);
  Omega_ps ~ lkj_corr(2);
  Omega_sr ~ lkj_corr(2);
  Omega_ss ~ lkj_corr(2);

  // etas for patients that have both scores -- correlated
  for (i in 1:Pn) {
    [eta_ps1[patient_map[i,1]], eta_ps2[patient_map[i,2]]] ~ multi_normal(rep_vector(0,2), quad_form_diag(Omega_ps, [omega_ps1, omega_ps2]));
    [eta_pr1[patient_map[i,1]], eta_pr2[patient_map[i,2]]] ~ multi_normal(rep_vector(0,2), quad_form_diag(Omega_pr, [omega_pr1, omega_pr2]));
  }
  
  // etas for studies that have both scores -- correlated
  for (i in 1:Sn) {
    [eta_ss1[study_map[i,1]], eta_ss2[study_map[i,2]]] ~ multi_normal(rep_vector(0,2), quad_form_diag(Omega_ss, [omega_ss1, omega_ss2]));
    [eta_sr1[study_map[i,1]], eta_sr2[study_map[i,2]]] ~ multi_normal(rep_vector(0,2), quad_form_diag(Omega_sr, [omega_sr1, omega_sr2]));
  }
  
  // etas for patients that have only score1
  if ((P - Pn) >= 1) {
    for (i in 1:(P - Pn)) {
      eta_ps1[other_patients1[i]] ~ normal(0, omega_ps1);
      eta_pr1[other_patients1[i]] ~ normal(0, omega_pr1);
    }
  }
  
  // etas for patients that have only score2
  if ((P2 - Pn) >= 1) {
    for (i in 1:(P2 - Pn)) {
      eta_ps2[other_patients2[i]] ~ normal(0, omega_ps2);
      eta_pr2[other_patients2[i]] ~ normal(0, omega_pr2);
    }
  }
  
  // etas for studies that have only score1
  if ((M - Sn) >= 1) {
    for (i in 1:(M - Sn)) {
      eta_ss1[other_studies1[i]] ~ normal(0, omega_ss1);
      eta_sr1[other_studies1[i]] ~ normal(0, omega_sr1);
    }
  }
  
  // etas for studies that have only score2
  if ((M2 - Sn) >= 1) {
    for (i in 1:(M2 - Sn)) {
      eta_ss2[other_studies2[i]] ~ normal(0, omega_ss2);
      eta_sr2[other_studies2[i]] ~ normal(0, omega_sr2);
    }
  }
  

  // Likelihood for first score
	tgt += my_func(IDp, IDs, is_pbo, time, score1, multiplicative_s, multiplicative_r, X_s, X_r,   // data
                  tau1, beta1, beta_pbo1, k_el1, k_el1 + delta1, theta_r1, theta_s1,
                  eta_pr1, eta_sr1, eta_ps1, eta_ss1, base_s1, base_r1);
      
  // Likelihood for second score            
  tgt += my_func(IDp2, IDs2, is_pbo2, time2, score2, multiplicative_s2, multiplicative_r2, X_s2, X_r2,   // data
                  tau2, beta2, beta_pbo2, k_el2, k_el2 + delta2, theta_r2, theta_s2,
                  eta_pr2, eta_sr2, eta_ps2, eta_ss2, base_s2, base_r2);

  target += tgt;

}

generated quantities{
  vector[N] score1_pred;
  vector[N2] score2_pred;
  for (n in 1:N) {
    real mu_pred;
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
    score1_pred[n] = beta_rng(mu_pred * tau1, (1 - mu_pred) * tau1);
  }
  for (n in 1:N2) {
    real mu_pred;
    mu_pred = get_mu(IDp2[n], 
                     IDs2[n], 
                     is_pbo2[IDs2[n]], 
                     time2[n], 
                       multiplicative_s2, 
                       multiplicative_r2, 
                       X_s2[n, ]', 
                       X_r2[n, ]',   // data
                       tau2, 
                       beta2, 
                       beta_pbo2, 
                       k_el2, 
                       k_el2 + delta2, 
                       theta_r2, 
                       theta_s2,
                       eta_pr2[IDp2[n]], 
                       eta_sr2[IDs2[n]], 
                       eta_ps2[IDp2[n]], 
                       eta_ss2[IDs2[n]], 
                       base_s2, 
                       base_r2);
    score2_pred[n] = beta_rng(mu_pred * tau2, (1 - mu_pred) * tau2);
  }
}

