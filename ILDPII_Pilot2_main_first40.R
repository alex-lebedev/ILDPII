# ILDPII_Pilot2_main_first40.R
#
# Author: Alexander V. Lebedev
# Date: 2018-11-06
# Testing models on different data sets

##############
# E-life data:
##############

rm(list=ls())

library(rstan)
library(rstantools)

##################
# INSTRUCTED-EL: #
##################

load('/Users/alebedev/GitHub/ILDPII/Pilot_2/sALL_final.rda');
sALL_eLife<- subset(sALL_eLife, is.element(sALL_eLife$subject,c(2,71))==F);
sALL_eLife<- subset(sALL_eLife, is.element(sALL_eLife$trial,c(1:40)));
sALL_eLife <- subset(sALL_eLife, sALL_eLife$instr=='X')

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
mysamples <- stan(file='/Users/alebedev/GitHub/ILDPII/stan_models/RW_instr_multipleSubj.stan',   
                  data=dataList, 
                  #                pars=parameters,
                  iter=5000, warmup = 2000,
                  chains=4, 
                  thin=1,     
                  control = list(adapt_delta   = adapt_delta, 
                                 max_treedepth = max_treedepth, 
                                 stepsize      = stepsize) 
)


# Extract the estimated rho parameters:
dd <- extract(mysamples) 
rhos_inst<- apply(dd$P,2,mean)
mysamplesINST_EL <- mysamples
save('mysamplesINST_EL', 'rhos_inst',file= '/Users/alebedev/GitHub/ILDPII/Pilot_2/mysamplesINST_EL_first40.rda')


####################
# UNINSTRUCTED-EL: #
####################

load('/Users/alebedev/GitHub/ILDPII/Pilot_2/sALL_final.rda')
sALL_eLife<- subset(sALL_eLife, is.element(sALL_eLife$subject,c(2,71))==F);
sALL_eLife<- subset(sALL_eLife, is.element(sALL_eLife$trial,c(1:40)));
sALL_eLife <- subset(sALL_eLife, sALL_eLife$instr=='Y')

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
mysamples <- stan(file='/Users/alebedev/GitHub/ILDPII/stan_models/RW_instr_multipleSubj.stan',   
                  data=dataList, 
                  #                pars=parameters,
                  iter=5000, warmup = 2000,
                  chains=4, 
                  thin=1,     
                  control = list(adapt_delta   = adapt_delta, 
                                 max_treedepth = max_treedepth, 
                                 stepsize      = stepsize) 
)


# Extract the estimated rho parameters:
dd <- extract(mysamples) 
rhos_uninst<- apply(dd$P,2,mean)
mysamplesUNINST_EL <- mysamples
save('mysamplesUNINST_EL', 'rhos_uninst',file= '/Users/alebedev/GitHub/ILDPII/Pilot_2/mysamplesUNINST_EL_first40.rda')



##################
# INSTRUCTED-IL: #
##################

# For first 40 trials:
load('/Users/alebedev/GitHub/ILDPII/Pilot_2/sALL_final.rda');
sALL$instr <- 'X'
sALL<- subset(sALL, is.element(sALL$trial,c(1:40)));
sALL <- subset(sALL, sALL$instr=='X')

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
                  iter=5000, warmup = 2000,
                  chains=4, 
                  thin=1,     
                  control = list(adapt_delta   = adapt_delta, 
                                 max_treedepth = max_treedepth, 
                                 stepsize      = stepsize) 
)


# Extract the estimated rho parameters:
dd <- extract(mysamples) 
rhos_inst_il<- apply(dd$P,2,mean)
mysamplesINST_IL <- mysamples
save('mysamplesINST_IL', 'rhos_inst_il',file= '/Users/alebedev/GitHub/ILDPII/Pilot_2/mysamplesINST_IL_first40.rda')



# 1-rev data:

load('/Users/alebedev/GitHub/ILDPII/Pilot_2/sALL_1rev.rda')
subjList <- unique(sALL_1rev[,'subject'])  # list of subjects x blocks
numSubjs <- length(subjList)  # number of subjects
maxTrials <- max(sALL_1rev$trial)
Tsubj <- as.vector(rep(0, numSubjs))
response <- array(0, c(numSubjs, maxTrials) )
stimulus <- array(0, c(numSubjs, maxTrials) )
shock <- array(0, c(numSubjs, maxTrials) )
revtrial <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  Tsubj[i] <- sum(sALL_1rev$subject == curSubj)  # Tsubj[N]
  useTrials    <- Tsubj[i]
  tmp          <- subset(sALL_1rev, sALL_1rev$subject == curSubj)
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
                  iter=5000, warmup = 2000,
                  chains=4, 
                  thin=1,     
                  control = list(adapt_delta   = adapt_delta, 
                                 max_treedepth = max_treedepth, 
                                 stepsize      = stepsize) 
)


# Extract the estimated rho parameters:
dd <- extract(mysamples) 
rhos_1rev<- apply(dd$P,2,mean)
mysamples_1REV <- mysamples
save('mysamples_1REV_IL', 'rhos_1rev',file= '/Users/alebedev/GitHub/ILDPII/Pilot_2/mysamples1REV.rda')