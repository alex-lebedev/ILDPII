# generateEVs.R
#
# Author: Alexander V. Lebedev
# Date: 2018-11-06
# Generate EVs

##############
# E-life data:
##############

rm(list=ls())

library(rstan)
library(rstantools)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

### 40 trials:

load('/Users/alebedev/GitHub/ILDPII/Pilot_2/sALL_final.rda');
sALL_eLife<- subset(sALL_eLife, is.element(sALL_eLife$subject,c(2,71))==F);
sALL_eLife<- subset(sALL_eLife, is.element(sALL_eLife$trial,c(1:40)));
sALL_eLife <- subset(sALL_eLife, sALL_eLife$instr=='X')
sALL_eLife <- subset(sALL_eLife, sALL_eLife$order=='A1' | sALL_eLife$order=='A2')
subjList <- unique(sALL_eLife[,'subject'])  # list of subjects x blocks
numSubjs <- length(subjList)  # number of subjects
maxTrials <- max(sALL_eLife$trial)
Tsubj <- as.vector(rep(0, numSubjs))
response <- array(0, c(numSubjs, maxTrials) )
stimulus <- array(0, c(numSubjs, maxTrials) )
shock <- array(0, c(numSubjs, maxTrials) )
revtrial <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  Tsubj[i] <- sum(sALL_eLife$subject == curSubj)  # Tsubj[N]
  useTrials    <- Tsubj[i]
  tmp          <- subset(sALL_eLife, sALL_eLife$subject == curSubj)
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
mysamples <- stan(file='/Users/alebedev/GitHub/ILDPII/stan_models/RW_instr_multipleSubj_initFree_GenQuant.stan',   
                  data=dataList, 
                  #                pars=parameters,
                  iter=5000, warmup = 2000,
                  chains=4, 
                  thin=1,     
                  control = list(adapt_delta   = adapt_delta, 
                                 max_treedepth = max_treedepth, 
                                 stepsize      = stepsize) 
)

dd <- extract(mysamples)

EV1_Ains_40 <- apply(dd$EV1,3,mean)
EV2_Ains_40 <- apply(dd$EV2,3,mean)

rm(mysamples, dd)
load('/Users/alebedev/GitHub/ILDPII/Pilot_2/sALL_final.rda');
sALL_eLife<- subset(sALL_eLife, is.element(sALL_eLife$subject,c(2,71))==F);
sALL_eLife<- subset(sALL_eLife, is.element(sALL_eLife$trial,c(1:40)));
sALL_eLife <- subset(sALL_eLife, sALL_eLife$instr=='X')
sALL_eLife <- subset(sALL_eLife, sALL_eLife$order=='B1' | sALL_eLife$order=='B2')
subjList <- unique(sALL_eLife[,'subject'])  # list of subjects x blocks
numSubjs <- length(subjList)  # number of subjects
maxTrials <- max(sALL_eLife$trial)
Tsubj <- as.vector(rep(0, numSubjs))
response <- array(0, c(numSubjs, maxTrials) )
stimulus <- array(0, c(numSubjs, maxTrials) )
shock <- array(0, c(numSubjs, maxTrials) )
revtrial <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  Tsubj[i] <- sum(sALL_eLife$subject == curSubj)  # Tsubj[N]
  useTrials    <- Tsubj[i]
  tmp          <- subset(sALL_eLife, sALL_eLife$subject == curSubj)
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
mysamples <- stan(file='/Users/alebedev/GitHub/ILDPII/stan_models/RW_instr_multipleSubj_initFree_GenQuant.stan',   
                  data=dataList, 
                  #                pars=parameters,
                  iter=5000, warmup = 2000,
                  chains=4, 
                  thin=1,     
                  control = list(adapt_delta   = adapt_delta, 
                                 max_treedepth = max_treedepth, 
                                 stepsize      = stepsize) 
)

dd <- extract(mysamples)

EV1_Bins_40 <- apply(dd$EV1,3,mean)
EV2_Bins_40 <- apply(dd$EV2,3,mean)


load('/Users/alebedev/GitHub/ILDPII/Pilot_2/sALL_final.rda');
sALL_eLife<- subset(sALL_eLife, is.element(sALL_eLife$subject,c(2,71))==F);
sALL_eLife<- subset(sALL_eLife, is.element(sALL_eLife$trial,c(1:40)));
sALL_eLife <- subset(sALL_eLife, sALL_eLife$instr=='Y')
sALL_eLife <- subset(sALL_eLife, sALL_eLife$order=='A1' | sALL_eLife$order=='A2')
subjList <- unique(sALL_eLife[,'subject'])  # list of subjects x blocks
numSubjs <- length(subjList)  # number of subjects
maxTrials <- max(sALL_eLife$trial)
Tsubj <- as.vector(rep(0, numSubjs))
response <- array(0, c(numSubjs, maxTrials) )
stimulus <- array(0, c(numSubjs, maxTrials) )
shock <- array(0, c(numSubjs, maxTrials) )
revtrial <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  Tsubj[i] <- sum(sALL_eLife$subject == curSubj)  # Tsubj[N]
  useTrials    <- Tsubj[i]
  tmp          <- subset(sALL_eLife, sALL_eLife$subject == curSubj)
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
mysamples <- stan(file='/Users/alebedev/GitHub/ILDPII/stan_models/RW_instr_multipleSubj_initFree_GenQuant.stan',   
                  data=dataList, 
                  #                pars=parameters,
                  iter=5000, warmup = 2000,
                  chains=4, 
                  thin=1,     
                  control = list(adapt_delta   = adapt_delta, 
                                 max_treedepth = max_treedepth, 
                                 stepsize      = stepsize) 
)

dd <- extract(mysamples)

EV1_Aunins_40 <- apply(dd$EV1,3,mean)
EV2_Aunins_40 <- apply(dd$EV2,3,mean)

rm(mysamples, dd)
load('/Users/alebedev/GitHub/ILDPII/Pilot_2/sALL_final.rda');
sALL_eLife<- subset(sALL_eLife, is.element(sALL_eLife$subject,c(2,71))==F);
sALL_eLife<- subset(sALL_eLife, is.element(sALL_eLife$trial,c(1:40)));
sALL_eLife <- subset(sALL_eLife, sALL_eLife$instr=='Y')
sALL_eLife <- subset(sALL_eLife, sALL_eLife$order=='B1' | sALL_eLife$order=='C2')
subjList <- unique(sALL_eLife[,'subject'])  # list of subjects x blocks
numSubjs <- length(subjList)  # number of subjects
maxTrials <- max(sALL_eLife$trial)
Tsubj <- as.vector(rep(0, numSubjs))
response <- array(0, c(numSubjs, maxTrials) )
stimulus <- array(0, c(numSubjs, maxTrials) )
shock <- array(0, c(numSubjs, maxTrials) )
revtrial <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  Tsubj[i] <- sum(sALL_eLife$subject == curSubj)  # Tsubj[N]
  useTrials    <- Tsubj[i]
  tmp          <- subset(sALL_eLife, sALL_eLife$subject == curSubj)
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
mysamples <- stan(file='/Users/alebedev/GitHub/ILDPII/stan_models/RW_instr_multipleSubj_initFree_GenQuant.stan',   
                  data=dataList, 
                  #                pars=parameters,
                  iter=5000, warmup = 2000,
                  chains=4, 
                  thin=1,     
                  control = list(adapt_delta   = adapt_delta, 
                                 max_treedepth = max_treedepth, 
                                 stepsize      = stepsize) 
)

dd <- extract(mysamples)

EV1_Bunins_40 <- apply(dd$EV1,3,mean)
EV2_Bunins_40 <- apply(dd$EV2,3,mean)

save('EV1_Ains_40','EV2_Ains_40','EV1_Bins_40','EV2_Bins_40','EV1_Aunins_40','EV2_Aunins_40','EV1_Bunins_40','EV2_Bunins_40',file='/Users/alebedev/GitHub/ILDPII/Pilot_2/EVs_EL_40trials.rda')


### FULL MODEL:
rm(list=ls())
load('/Users/alebedev/GitHub/ILDPII/Pilot_2/sALL_final.rda');
sALL_eLife<- subset(sALL_eLife, is.element(sALL_eLife$subject,c(2,71))==F);
sALL_eLife <- subset(sALL_eLife, sALL_eLife$instr=='X')
sALL_eLife <- subset(sALL_eLife, sALL_eLife$order=='A1' | sALL_eLife$order=='A2')
subjList <- unique(sALL_eLife[,'subject'])  # list of subjects x blocks
numSubjs <- length(subjList)  # number of subjects
maxTrials <- max(sALL_eLife$trial)
Tsubj <- as.vector(rep(0, numSubjs))
response <- array(0, c(numSubjs, maxTrials) )
stimulus <- array(0, c(numSubjs, maxTrials) )
shock <- array(0, c(numSubjs, maxTrials) )
revtrial <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  Tsubj[i] <- sum(sALL_eLife$subject == curSubj)  # Tsubj[N]
  useTrials    <- Tsubj[i]
  tmp          <- subset(sALL_eLife, sALL_eLife$subject == curSubj)
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
mysamples <- stan(file='/Users/alebedev/GitHub/ILDPII/stan_models/RW_instr_multipleSubj_initFree_GenQuant.stan',   
                  data=dataList, 
                  #                pars=parameters,
                  iter=5000, warmup = 2000,
                  chains=4, 
                  thin=1,     
                  control = list(adapt_delta   = adapt_delta, 
                                 max_treedepth = max_treedepth, 
                                 stepsize      = stepsize) 
)

dd <- extract(mysamples)

EV1_Ains <- apply(dd$EV1,3,mean)
EV2_Ains <- apply(dd$EV2,3,mean)

rm(mysamples, dd)
load('/Users/alebedev/GitHub/ILDPII/Pilot_2/sALL_final.rda');
sALL_eLife<- subset(sALL_eLife, is.element(sALL_eLife$subject,c(2,71))==F);
sALL_eLife <- subset(sALL_eLife, sALL_eLife$instr=='X')
sALL_eLife <- subset(sALL_eLife, sALL_eLife$order=='B1' | sALL_eLife$order=='B2')
subjList <- unique(sALL_eLife[,'subject'])  # list of subjects x blocks
numSubjs <- length(subjList)  # number of subjects
maxTrials <- max(sALL_eLife$trial)
Tsubj <- as.vector(rep(0, numSubjs))
response <- array(0, c(numSubjs, maxTrials) )
stimulus <- array(0, c(numSubjs, maxTrials) )
shock <- array(0, c(numSubjs, maxTrials) )
revtrial <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  Tsubj[i] <- sum(sALL_eLife$subject == curSubj)  # Tsubj[N]
  useTrials    <- Tsubj[i]
  tmp          <- subset(sALL_eLife, sALL_eLife$subject == curSubj)
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
mysamples <- stan(file='/Users/alebedev/GitHub/ILDPII/stan_models/RW_instr_multipleSubj_initFree_GenQuant.stan',   
                  data=dataList, 
                  #                pars=parameters,
                  iter=5000, warmup = 2000,
                  chains=4, 
                  thin=1,     
                  control = list(adapt_delta   = adapt_delta, 
                                 max_treedepth = max_treedepth, 
                                 stepsize      = stepsize) 
)

dd <- extract(mysamples)

EV1_Bins <- apply(dd$EV1,3,mean)
EV2_Bins <- apply(dd$EV2,3,mean)


load('/Users/alebedev/GitHub/ILDPII/Pilot_2/sALL_final.rda');
sALL_eLife<- subset(sALL_eLife, is.element(sALL_eLife$subject,c(2,71))==F);
sALL_eLife <- subset(sALL_eLife, sALL_eLife$instr=='Y')
sALL_eLife <- subset(sALL_eLife, sALL_eLife$order=='A1' | sALL_eLife$order=='A2')
subjList <- unique(sALL_eLife[,'subject'])  # list of subjects x blocks
numSubjs <- length(subjList)  # number of subjects
maxTrials <- max(sALL_eLife$trial)
Tsubj <- as.vector(rep(0, numSubjs))
response <- array(0, c(numSubjs, maxTrials) )
stimulus <- array(0, c(numSubjs, maxTrials) )
shock <- array(0, c(numSubjs, maxTrials) )
revtrial <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  Tsubj[i] <- sum(sALL_eLife$subject == curSubj)  # Tsubj[N]
  useTrials    <- Tsubj[i]
  tmp          <- subset(sALL_eLife, sALL_eLife$subject == curSubj)
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
mysamples <- stan(file='/Users/alebedev/GitHub/ILDPII/stan_models/RW_instr_multipleSubj_initFree_GenQuant.stan',   
                  data=dataList, 
                  #                pars=parameters,
                  iter=5000, warmup = 2000,
                  chains=4, 
                  thin=1,     
                  control = list(adapt_delta   = adapt_delta, 
                                 max_treedepth = max_treedepth, 
                                 stepsize      = stepsize) 
)

dd <- extract(mysamples)

EV1_Aunins <- apply(dd$EV1,3,mean)
EV2_Aunins <- apply(dd$EV2,3,mean)

rm(mysamples, dd)
load('/Users/alebedev/GitHub/ILDPII/Pilot_2/sALL_final.rda');
sALL_eLife<- subset(sALL_eLife, is.element(sALL_eLife$subject,c(2,71))==F);
sALL_eLife <- subset(sALL_eLife, sALL_eLife$instr=='Y')
sALL_eLife <- subset(sALL_eLife, sALL_eLife$order=='B1' | sALL_eLife$order=='B2')
subjList <- unique(sALL_eLife[,'subject'])  # list of subjects x blocks
numSubjs <- length(subjList)  # number of subjects
maxTrials <- max(sALL_eLife$trial)
Tsubj <- as.vector(rep(0, numSubjs))
response <- array(0, c(numSubjs, maxTrials) )
stimulus <- array(0, c(numSubjs, maxTrials) )
shock <- array(0, c(numSubjs, maxTrials) )
revtrial <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  Tsubj[i] <- sum(sALL_eLife$subject == curSubj)  # Tsubj[N]
  useTrials    <- Tsubj[i]
  tmp          <- subset(sALL_eLife, sALL_eLife$subject == curSubj)
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
mysamples <- stan(file='/Users/alebedev/GitHub/ILDPII/stan_models/RW_instr_multipleSubj_initFree_GenQuant.stan',   
                  data=dataList, 
                  #                pars=parameters,
                  iter=5000, warmup = 2000,
                  chains=4, 
                  thin=1,     
                  control = list(adapt_delta   = adapt_delta, 
                                 max_treedepth = max_treedepth, 
                                 stepsize      = stepsize) 
)

dd <- extract(mysamples)

EV1_Bunins <- apply(dd$EV1,3,mean)
EV2_Bunins <- apply(dd$EV2,3,mean)

save('EV1_Ains','EV2_Ains','EV1_Bins','EV2_Bins','EV1_Aunins','EV2_Aunins','EV1_Bunins','EV2_Bunins',file='/Users/alebedev/GitHub/ILDPII/Pilot_2/EVs_EL.rda')

