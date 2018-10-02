data {
  int<lower=1> T; 
  //int<lower=1> N;
  int<lower=1,upper=2> stimulus[T];     
  int shock[T];  // electric shocks 
  real response[T];
  int revtrial[T];  // reversed trials indicator
}

transformed data {
  vector[2] initV;  // initial values for response
  initV[1] = 0.75;
  initV[2] = 0.25;
}

parameters {
  // Subject-lresponseel raw parameters (for Matt trick)
  real A;    // learning rate
  real k; 
  real<lower=0, upper=1> P;    
  real<lower=0> sigma0;    
}

model{
//A_pr   ~ normal(0,1);
//k_pr ~ normal(0,1);
//P_pr ~ normal(0.5,1);
sigma0 ~ cauchy(0, 100);

for (i in 1:1) {
    vector[2] EV; // expected value
    real PE;      // prediction error
    EV = initV;
  for (t in 1:T) {       
    if (revtrial[t]==1){
      response[t] ~ normal( EV[stimulus[t]] * k, sigma0);
      PE = shock[t] - EV[stimulus[t]];
      EV[stimulus[t]] = P * EV[stimulus[t]] + (1 - P) * A * PE; 
    }
    else
    // compute action probabilities
   response[t] ~ normal( EV[stimulus[t]] * k, sigma0);
      
    // prediction error 
    PE = shock[t] - EV[stimulus[t]];
      
    // value updating (learning) 
   EV[stimulus[t]] = EV[stimulus[t]] + A * PE; 
  
   }
   }
}
