data{

	int N;             			// number of total Observations
  int P;              			// number of subjects
	int M;                    // number of studies
 	int IDp[N];            			// Patient ID 
 	int IDs[N];            			// Study ID 
 	vector[N] SEX;
 	vector[N] AGE;
 	vector[N] COMED;
 	vector[N] APOE4;
  vector[N] time;      			// time of observation (years)
	vector[N] S;           			// measured ADAScog score
 
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
transformed parameters{
  
  	vector[N] baseline_cov;
  	vector[N] rate_cov;
  	vector[N] S0;
  	vector[N] r;
  	vector<lower=0,upper=1>[N] muS;
    real tau_trans;
    real beta_trans;
    real beta_bateman_trans;
    real keq_trans;
    real kel_trans;
    vector[N] pbo;
    
    tau_trans = (tau+1)*80;
    beta_trans = (beta +1)*5;
    beta_bateman_trans = exp(beta_bateman-3.5);
    keq_trans = exp(keq+1.88);
    kel_trans = exp(kel+.46);
    
  	for(i in 1:N) {

      	baseline_cov[i] = theta_S0*(1+theta_SEX*SEX[i])*(1+theta_APOE4_b*(APOE4[i]-0.72));
      	rate_cov[i] = theta_r*(1+theta_AGE*(AGE[i]-75))*(1+theta_APOE4_r*(APOE4[i]-0.72))*(1+theta_COMED*COMED[i]);
        
        r[i] = rate_cov[i] + eta_pr[IDp[i]] + eta_sr[IDs[i]];
      	S0[i] = baseline_cov[i]*exp(eta_pb[IDp[i]] + eta_sb[IDs[i]]);
      	
      	pbo[i] = beta_bateman_trans*(exp(-kel_trans*time[i])-exp(-keq_trans*time[i]));
      	
      	if (IDs[i]<=15) muS[i] = S0[i]/(S0[i]^beta_trans +(1-S0[i]^beta_trans)*exp(-beta_trans*r[i]*time[i]))^(1/beta_trans) - pbo[i];
      	else            muS[i] = S0[i]/(S0[i]^beta_trans +(1-S0[i]^beta_trans)*exp(-beta_trans*r[i]*time[i]))^(1/beta_trans);
  	}
}
model{

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
    	S ~beta(muS*tau_trans, (1-muS)*tau_trans);
} 































