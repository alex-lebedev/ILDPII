data {
  int<lower=1> T; 
  int<lower=1> N;
  int<lower=1,upper=2> stimulus[N,T];     
  int shock[N,T];  // electric shocks 
  real response[N,T]; 
}

transformed data {
  vector[2] initV;  // initial values for response
  initV = rep_vector(0.0, 2);
}

parameters {
    // Hyper(group)-parameters  
  vector[2] mu_p;  
  vector<lower=0>[2] sigma;
  
  // Subject-lresponseel raw parameters (for Matt trick)
  vector[N] A_pr;    // learning rate
  vector[N] k_pr;  // inverse temperature
}

transformed parameters {
  // subject-lresponseel parameters
  vector<lower=0,upper=1>[N] A;

   for (i in 1:N) {
  A[i]   = Phi_approx( mu_p[1]  + sigma[1]  * A_pr[i]);
   }
}


model{
  // Hyperparameters:
  mu_p  ~ normal(0, 1); 
  sigma ~ cauchy(0, 5);  
//A   ~ normal(0,1);
k_pr ~ normal(0,1);

for (i in 1:N) {
    vector[2] EV; // expected value
    real PE;      // prediction error
    
    EV = initV;
  for (t in 1:T) {        
    // compute action probabilities
    response[i,t] ~ normal( EV[stimulus[i,t]] * k_pr,1);
      
    // prediction error 
    PE = shock[i,t] - EV[stimulus[i,t]];
      
    // value updating (learning) 
   EV[stimulus[i,t]] = EV[stimulus[i,t]] + A[i] * PE; 
   }
   }
}
