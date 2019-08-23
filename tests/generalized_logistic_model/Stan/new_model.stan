functions {


  real my_func(int[] IDp, int[] IDs, int[] is_pbo, vector time, vector score, int multiplicative_s, int multiplicative_r, matrix X_s, matrix X_r,   // data
                  real tau, real beta, real beta_pbo, real k_el, real k_eq, vector theta_r, vector theta_s, vector eta_pr, vector eta_sr, vector eta_ps, vector eta_ss, real base_s, real base_r) {                 // parameters
    // constants

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
     # muS[i] = S0 / (S0^beta + (1 - S0^beta) * exp(-beta * r * time[i] - beta * is_pbo[IDs[i]] * pbo_eff))^(1 / beta);

    }

    return  beta_lpdf(score | muS * tau, (1 - muS) * tau);
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
  real tmp_tgt;
  tmp_tgt = my_func(IDp, IDs, is_pbo, time, score, multiplicative_s, multiplicative_r, X_s, X_r,   // data
                  tau, beta, beta_pbo, k_el, k_el + delta, theta_r, theta_s, 
                  eta_pr, eta_sr, eta_ps, eta_ss, base_s, base_r);
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

  //print("1tgt = ", tgt);
// Likelihood for first score
	tgt += my_func(IDp, IDs, is_pbo, time, score, multiplicative_s, multiplicative_r, X_s, X_r,   // data
                  tau, beta, beta_pbo, k_el, k_el + delta, theta_r, theta_s, 
                  eta_pr, eta_sr, eta_ps, eta_ss, base_s, base_r);
  

  // print("IDp = ", IDp);
  // print("IDs = ", IDs);
  // print("is_pbo = ", is_pbo);
  // print("time = ", time);
  // print("score = ", score);
  // print("multiplicative_s = ", multiplicative_s);
  // print("multiplicative_r = ", multiplicative_r);
  // print("X_s = ", X_s);
  // print("X_r = ", X_r);
  // print("tau = ", tau);
  // print("beta = ", beta);
  // print("beta_pbp = ", beta_pbo);
  // print("k_el = ", k_el);
  // print("k_eq = ", k_el + delta);
  // print("theta_r = ", theta_r);
  // print("theta_s = ", theta_s);
  // print("eta_pr = ", eta_pr);
  // print("eta_sr = ", eta_sr);
  // print("eta_pr = ", eta_ps);
  // print("eta_pr = ", eta_ss);
  // print("base_s = ", base_s);
  // print("base_r = ", base_r);
  // print("tgt = ", tgt);
  
  //print("2tgt = ", tgt);
  target += tgt;

}































