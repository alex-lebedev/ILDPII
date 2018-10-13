rm(list=ls())

library(rstan)
library(rstantools)


load('/Users/alebedev/GitHub/ILDPII/Pilot_2/sALL_instructed.rda')


subjList <- unique(sALL[,'subject'])  # list of subjects x blocks
numSubjs <- length(subjList)  # number of subjects
maxTrials <- max(sALL$trial)
Tsubj <- as.vector(rep(0, numSubjs))
response <- array(0, c(numSubjs, maxTrials) )
stimulus <- array(0, c(numSubjs, maxTrials) )
shock <- array(0, c(numSubjs, maxTrials) )
revtrial <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  Tsubj[i] <- sum(sALL$subject == curSubj)  # Tsubj[N]
  useTrials    <- Tsubj[i]
  tmp          <- subset(sALL, sALL$subject == curSubj)
  response[i, 1:useTrials]   <- tmp$response
  stimulus[i, 1:useTrials]   <- tmp$stimulus
  shock[i, 1:useTrials]   <- tmp$shock
  revtrial[i, 1:useTrials]   <- tmp$revtrial
}

dataList <- list(
  N             = numSubjs,
  T             = maxTrials,
  response      = response,
  stimulus      = stimulus,
  shock         = shock,
  Tsubj         = Tsubj,
  revtrial      = revtrial
)



adapt_delta   = 0.99
stepsize      = 0.1
max_treedepth = 12

### Run Model:
mysamples <- stan(file='/Users/alebedev/GitHub/ILDPII/stan_models/RW_instr_multipleSubj.stan',   
                  data=dataList, 
                  #                pars=parameters,
                  iter=50000, warmup = 48500,
                  chains=4, 
                  thin=1,     
                  control = list(adapt_delta   = adapt_delta, 
                                 max_treedepth = max_treedepth, 
                                 stepsize      = stepsize) 
)

# Traceplots look ok except perhaps for sigmas:
traceplot(mysamples)
traceplot(mysamples, par=c('P_pr', 'P'))

# However, the estimates are completely off:
plot(mysamples)


dd <- extract(mysamples) # estract the estimated parameters
mean(dd$P[,3]) # mean of the P-parameter


