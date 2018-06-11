data {
 //int<lower=1> N;
  int<lower=1> T;               
 // int<lower=1,upper=T> Tsubj[N];                 
  int<lower=1,upper=2> stimulus[T];     
 // real<lower=0,upper=1> rresponsetrial[N,T];  // indicates rresponseersed trials
  int shock[T];  // electric shocks 
  real response[T];  // electric shocks   
}

transformed data {
  vector[2] initV;  // initial values for response
  initV = rep_vector(0.0, 2);
}

parameters {
  // Subject-lresponseel raw parameters (for Matt trick)
  real A_pr;    // learning rate
  real k_pr;    
}

model{
A_pr   ~ normal(0,1);
k_pr ~ normal(0,1);

for (i in 1:1) {
    vector[2] EV; // expected value
    real PE;      // prediction error
    
    EV = initV;
  for (t in 1:80) {        
    // compute action probabilities
   response[t] ~ normal( EV[stimulus[t]] * k_pr,1);
      
    // prediction error 
    PE = shock[t] - EV[stimulus[t]];
      
    // value updating (learning) 
   EV[stimulus[t]] = EV[stimulus[t]] + A_pr * PE; 
  
   }
   }
}


