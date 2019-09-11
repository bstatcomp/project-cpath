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
	vector[N] score;          // measured scores
}

parameters{
  // real parameters
  
      
  real<lower=0> tau;
  real<lower=0> beta;
  vector[Q_r] theta_r; // coeff. for r
  vector[Q_s] theta_s; // coeff. for S0
  
  // eta's
  vector[P] eta_ps;
  vector[P] eta_pr;
  vector[M] eta_ss;
  vector[M] eta_sr;
  real<lower=0> omega_ps;
  real<lower=0> omega_pr;
  real<lower=0> omega_ss;
  real<lower=0> omega_sr;
  
  // base baseline and rate
  real base_s;
  real base_r;

  // Bateman term
  real<lower=0> beta_pbo;
  real<lower=0> k_el;
  real<lower=0> delta;
  
  
}

transformed parameters {
  //real tmp_tgt;
  real k_eq;
  k_eq = k_el - delta;
  /*tmp_tgt = generalized_logistic_model(IDp, IDs, is_pbo, time, score, multiplicative_s, multiplicative_r, X_s, X_r,   // data
                  tau, beta, beta_pbo, k_el, k_eq, theta_r, theta_s, 
                  eta_pr, eta_sr, eta_ps, eta_ss, base_s, base_r);*/
  
}

model{
  real tgt = 0;
    
    tau ~ normal(0, 10);
    beta ~ normal(0, 10);
    beta_pbo ~ normal(0, 10);
    k_el~ normal(0, 10);
    delta ~ normal(0, 10);
    base_s ~ normal(0, 2);
    
    theta_r~ normal(0, 10);
  theta_s~ normal(0, 10);
  eta_ps~ normal(0, omega_ps);
  eta_pr~ normal(0, omega_pr);
  eta_ss~ normal(0, omega_ss);
  eta_sr~ normal(0, omega_sr);
  omega_ps ~ normal(0,10);
  omega_pr ~ normal(0,10);
  omega_ss ~ normal(0,10);
  omega_sr ~ normal(0,10);
  base_r~ normal(0, 10);

  // Likelihood for first score
	tgt += generalized_logistic_model(IDp, IDs, is_pbo, time, score, multiplicative_s, multiplicative_r, X_s, X_r,   // data
                  tau, beta, beta_pbo, k_el, k_eq, theta_r, theta_s, 
                  eta_pr, eta_sr, eta_ps, eta_ss, base_s, base_r);
  target += tgt;

}