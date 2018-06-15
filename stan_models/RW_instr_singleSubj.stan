data {
  int<lower=1> T;               
  int<lower=1,upper=2> stimulus[T];     
  int shock[N, T];  // electric shocks 
  int revtrial[N, T];  // reversed trials indicator
  real response[N, T]; 
}

transformed data {
  vector[2] initV;  // initial values for response
  initV[1] = 0.75;
  initV[2] = 0.25;
}

parameters {
  // Subject-lresponseel raw parameters (for Matt trick)
  real A_pr;    // learning rate
  real k_pr; 
  real<lower=0, upper=1> P_pr;    
  real<lower=0> sigma0;    
  
}

transformed parameters {
  // subject-level parameters
  vector<lower=0,upper=1>[N] A;
  vector<lower=0,upper=100>[N] k;

   for (i in 1:N) {
  A[i]   = Phi_approx( mu_p[1]  + sigma[1]  * A_pr[i]);
  k[i]   = Phi_approx( mu_p[2]  + sigma[2]  * k_pr[i])*100; // scale according to upper on line 28
  P[i]   = Phi_approx( mu_p[2]  + sigma[2]  * k_pr[i])*100; // scale according to upper on line 28
   }
}

model{
A_pr   ~ normal(0,1);
k_pr ~ normal(0,1);
//P_pr ~ normal(0.5,1);
sigma0 ~ cauchy(0, 100);

for (i in 1:N) {
    vector[2] EV; // expected value
    real PE;      // prediction error
    
    EV = initV;
  for (t in 1:T) {       
    if (revtrial[t]==1){
      response[t] ~ normal( EV[stimulus[t]] * k[i], sigma0);
      PE = shock[t] - EV[stimulus[t]];
      EV[stimulus[t]] = P_pr * EV[stimulus[t]] + (1 - P_pr) * A_pr * PE; 
    }
    else
    // compute action probabilities
   response[t] ~ normal( EV[stimulus[t]] * k[i], sigma0);
      
    // prediction error 
    PE = shock[t] - EV[stimulus[t]];
      
    // value updating (learning) 
   EV[stimulus[t]] = EV[stimulus[t]] + A[i] * PE; 
  
   }
   }
}


