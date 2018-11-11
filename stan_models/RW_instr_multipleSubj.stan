data {
  int<lower=1> T; 
  int<lower=1> N;
  int<lower=1,upper=2> stimulus[N,T];     
  int shock[N,T];  // electric shocks 
  real response[N,T];
  int revtrial[N, T];  // reversed trials indicator

}

transformed data {
  vector[2] initV;  // initial values for response
  initV = rep_vector(0.0, 2);
  initV[1] = 0.75;
  initV[2] = 0.25;
}

parameters {
    // Hyper(group)-parameters  
  vector[3] mu_p; // means of Hyperparameters
  vector<lower=0>[3] sigma; // variances of Hyperparameters
  real<lower=0> sigma0; // noise in response
  
  // Subject-lresponseel raw parameters (for Matt trick)
  vector[N] A_pr;    // learning rate
  vector[N] k_pr;  // scaling factor
  vector[N] P_pr;  // instruction effect
}

transformed parameters {
  // subject-lresponseel parameters
  vector<lower=0,upper=1>[N] A;
  vector<lower=0,upper=1>[N] k;
  vector<lower=0,upper=1>[N] P;

   for (i in 1:N) {
  A[i]   = Phi_approx( mu_p[1]  + sigma[1]  * A_pr[i]);
  k[i]   = Phi_approx( mu_p[2]  + sigma[2]  * k_pr[i])*1; // scale according to upper of k
  P[i]   = Phi_approx( mu_p[3]  + sigma[3]  * P_pr[i]);
   }
}


model{
  // Hyperparameters:
  mu_p  ~ normal(0, 1); 
  sigma ~ cauchy(0, 5);  
  k_pr ~ normal(0, 1);
  A_pr ~ normal(0, 1);
  P_pr ~ normal(0, 1);
  sigma0 ~ cauchy(0, 10); // depends on scale of response

for (i in 1:N) {
    vector[2] EV; // expected value
    real PE;      // prediction error
    
    EV = initV;
  for (t in 1:T) {      
    if (revtrial[i,t]==1){
      EV[1] = P[i] * EV[2] + (1-P[i]) * EV[1];
      EV[2] = P[i] * EV[1] + (1-P[i]) * EV[2];
    }
    
    else if (shock[i,t]==0){
       // Response
    response[i,t] ~ normal( EV[stimulus[i,t]] * k[i],sigma0);
    }
    // prediction error 
    PE = shock[i,t] - EV[stimulus[i,t]];
    // value updating (learning) 
   EV[stimulus[i,t]] = EV[stimulus[i,t]] + A[i] * PE; 
   }
   }
}
