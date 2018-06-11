data {
  int<lower=1> T;               
  int<lower=1,upper=2> stimulus[T];     
  int shock[T];  // electric shocks 
  int revtrial[T];  // electric shocks 
  real response[T]; 
}

transformed data {
  vector[2] initV;  // initial values for response
  initV = rep_vector(0.0, 2);
}

parameters {
  // Subject-lresponseel raw parameters (for Matt trick)
  real A_pr;    // learning rate
  real k_pr; 
  real P_pr;    

}

model{
A_pr   ~ normal(0,1);
k_pr ~ normal(0,1);
P_pr ~ normal(0,1);

for (i in 1:1) {
    vector[2] EV; // expected value
    real PE;      // prediction error
    
    EV = initV;
  for (t in 1:80) {       
    if (revtrial[t]==1){
      response[t] ~ normal( EV[stimulus[t]] * k_pr,1);
      PE = shock[t] - EV[stimulus[t]];
      EV[stimulus[t]] = P_pr * EV[stimulus[t]] + (1 - P_pr) * A_pr * PE; 
    }
    else
    // compute action probabilities
   response[t] ~ normal( EV[stimulus[t]] * k_pr,1);
      
    // prediction error 
    PE = shock[t] - EV[stimulus[t]];
      
    // value updating (learning) 
   EV[stimulus[t]] = EV[stimulus[t]] + A_pr * PE; 
  
   }
   }
}


