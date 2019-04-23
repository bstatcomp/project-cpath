functions {
  
  real my_func(int[] IDp, int[] IDs, vector time, vector S, vector SEX, vector AGE, vector COMED, vector APOE4, int[] pbo_flag, // this row is data
                      real theta_S0, real theta_r, real theta_SEX, real theta_AGE, real theta_APOE4_b, real theta_APOE4_r, 
                      real theta_COMED, real tau, real beta, real beta_bateman, real kel, real keq,
                      vector eta_pb, vector eta_pr, vector eta_sb, vector eta_sr) {

    int N = size(IDp);
    real tgt = 0;
    vector[N] muS;
    //tau_trans = (80 * tau + 80);
    //beta_trans = (5 * beta + 5);
    //beta_bateman_trans = exp(beta_bateman-3.5);
    //keq_trans = exp(keq+1.88);
    //kel_trans = exp(kel+.46);
    
  	for(i in 1:N) {
      	real baseline_cov = theta_S0*(1+theta_SEX*SEX[i])*(1+theta_APOE4_b*(APOE4[i]-0.72));
      	real rate_cov = theta_r*(1+theta_AGE*(AGE[i]-75))*(1+theta_APOE4_r*(APOE4[i]-0.72))*(1+theta_COMED*COMED[i]);
        real r = rate_cov + eta_pr[IDp[i]] + eta_sr[IDs[i]];
      	real S0 = baseline_cov*exp(eta_pb[IDp[i]] + eta_sb[IDs[i]]);
      	real pbo = exp(beta_bateman-3.5)*(exp(-exp(kel+.46)*time[i])-exp(-exp(keq+1.88)*time[i]));
      	//real muS;
      	
      	muS[i] = S0/(S0^(5 * beta + 5) +(1-S0^(5 * beta + 5))*exp(-(5 * beta + 5)*r*time[i]))^(1/(5 * beta + 5)) - pbo_flag[i] * pbo;
      	
      
  	}
    tgt += beta_lpdf(S | muS*(80 * tau + 80), (1-muS)*(80 * tau + 80));
    return tgt;
  }
}

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
	 tgt = my_func( IDp, IDs, time, S, SEX, AGE, COMED, APOE4, pbo_flag, // this row is data
           theta_S0, theta_r, theta_SEX, theta_AGE, theta_APOE4_b, theta_APOE4_r, 
           theta_COMED, tau, beta, beta_bateman, kel, keq,
           eta_pb, eta_pr, eta_sb, eta_sr);
           
  // print("IDp = ", IDp);
  // print("IDs = ", IDs);
  // print("time = ", time);
  // print("S = ", S);
  // print("SEX = ", SEX);
  // print("AGE = ", AGE);
  // print("COMED = ", COMED);
  // print("APOE4 = ", APOE4);
  // print("pbo_flag = ", pbo_flag);
  // print("theta_S0 = ", theta_S0);
  // print("theta_r = ", theta_r);
  // print("theta_SEX = ", theta_SEX);
  // print("theta_AGE = ", theta_AGE);
  // print("theta_APOE4_b = ", theta_APOE4_b);
  // print("theta_APOE4_r = ", theta_APOE4_r);
  // print("theta_COMED = ", theta_COMED);
  // print("tau = ", tau);
  // print("beta = ", beta);
  // print("beta_bateman = ", beta_bateman);
  // print("kel = ", kel);
  // print("keq = ", keq);
  // print("eta_pb = ", eta_pb);
  // print("eta_pr = ", eta_pr);
  // print("eta_sb = ", eta_sb);
  // print("eta_sr = ", eta_sr);
  // print("tgt = ", tgt);
  target += tgt;
} 































