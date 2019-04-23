data{

	int N;             			// number of total Observations
  	int P;              			// number of subjects
	int M;                    // number of studies
 	int IDp[N];            			// Patient ID 
 	int IDs[N];            			// Study ID 
 	int SEX[N];
 	int AGE[N];
 	int COMED[N];
 	vector[N] APOE4;
  	vector[N] time;      			// time of observation (years)
	vector[N] S;           			// measured ADAScog score
 
}

transformed data {
  int pbo_flag[N];
  for (i in 1:N) {
    pbo_flag[i] = 0;
    if (IDs[i]<=15) pbo_flag[i] = 1;
  }
}

parameters{

  	//Fixed effects and covariates 
  	real<lower=0,upper=1> theta_S0;         // Mean baseline score for average individual, orig no lower
  	real theta_r;                           // Mean progression rate for average individual, orig no lower
  	real theta_SEX;
  	real theta_AGE;
  	real theta_APOE4_b;
  	real theta_APOE4_r;
  	real theta_COMED;
  	real tau;            
  	real beta;
  	real beta_bateman;
    real kel;
    real keq;
    
  	//Inter-patient re
  	vector[P] eta_pb;                     	// re for patient baseline
  	vector[P] eta_pr;            		      // re for patient rate
  	vector[M] eta_sb;
  	vector[M] eta_sr;
	  real<lower=0> omega_pb;               	// std for patient baseline
	  real<lower=0> omega_pr;               	// std for patient rate
	  real<lower=0> omega_sb;
	  real<lower=0> omega_sr;
}

model{
  real tgt;
//Priors
	omega_pb~normal(0,1); 
	omega_pr~normal(0,1);
	omega_sb~normal(0,1); 
	omega_sr~normal(0,1); 
	eta_pb~normal(0,omega_pb);
	eta_pr~normal(0,omega_pr);
	eta_sb~normal(0,omega_sb);
	eta_sr~normal(0,omega_sr);

	theta_S0~normal(0,1); 
	theta_r~normal(0,1);
	theta_SEX~normal(0,1);
	theta_AGE~normal(0,1);
	theta_APOE4_b~normal(0,1);
  	theta_APOE4_r~normal(0,1);
	theta_COMED~normal(0,1);
	tau~normal(0,1); 
	beta~normal(0,1);
	kel~normal(0,1);
	keq~normal(0,1);
	beta_bateman~normal(0,1);
  	
	// Likelihood
	target += generalized_logistic_model( IDp, IDs, time, S, APOE4, AGE, SEX, pbo_flag, COMED,   // this row is data
           theta_S0, theta_r, tau, theta_AGE, theta_APOE4_r, theta_APOE4_b,
		   theta_COMED, beta, theta_SEX, beta_bateman, kel, keq,
           eta_pb, eta_pr, eta_sb, eta_sr);
} 































