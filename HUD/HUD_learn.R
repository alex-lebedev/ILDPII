# HUD modeling.
 
rm(list=ls())
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('/Users/alebedev/Documents/Projects/HUD/pupillometry/HUD_RALT_pupillometry_df.rda')
HUD_df <- subset(HUD_RALT_pupillometry_df, HUD_RALT_pupillometry_df$group=='NP')

subjList <- unique(HUD_df[,'subject'])  # list of subjects x blocks
numSubjs <- length(subjList)  # number of subjects
maxTrials <- max(HUD_df$trial)
Tsubj <- as.vector(rep(0, numSubjs))
response <- array(0, c(numSubjs, maxTrials) )
stimulus <- array(0, c(numSubjs, maxTrials) )
shock <- array(0, c(numSubjs, maxTrials) )
revtrial <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  Tsubj[i] <- sum(HUD_df$subject == curSubj)  # Tsubj[N]
  useTrials    <- Tsubj[i]
  tmp          <- subset(HUD_df, HUD_df$subject == curSubj)
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
mysamples <- stan(file='/Users/alebedev/GitHub/ILDPII/stan_models/RW_instr_multipleSubj_initFree.stan',   
                  data=dataList, 
                  #                pars=parameters,
                  iter=10000, warmup = 3000,
                  chains=4, 
                  thin=1,     
                  control = list(adapt_delta   = adapt_delta, 
                                 max_treedepth = max_treedepth, 
                                 stepsize      = stepsize) 
)


# Extract the estimated rho parameters:
dd <- extract(mysamples) 
rhos<- apply(dd$P,2,mean)
alpha<- apply(dd$A,2,mean)
evDelta<- apply(dd$EVinit1,2,mean)-apply(dd$EVinit2,2,mean)

mysamplesHUD <- mysamples
save('mysamplesHUD', 'rhos', 'alpha', 'evDelta',
     file= '/Users/alebedev/Documents/Projects/HUD/pupillometry/mysamplesHUD_NP.rda')

save(SCREEN_DF, SCRFU_df,
     file= '/Users/alebedev/Documents/Projects/HUD/pupillometry/mysamplesHUD_NP.rda')
rm(list=ls())
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('/Users/alebedev/Documents/Projects/HUD/pupillometry/HUD_RALT_pupillometry_df.rda')
HUD_df <- subset(HUD_RALT_pupillometry_df, HUD_RALT_pupillometry_df$group=='PP')

subjList <- unique(HUD_df[,'subject'])  # list of subjects x blocks
numSubjs <- length(subjList)  # number of subjects
maxTrials <- max(HUD_df$trial)
Tsubj <- as.vector(rep(0, numSubjs))
response <- array(0, c(numSubjs, maxTrials) )
stimulus <- array(0, c(numSubjs, maxTrials) )
shock <- array(0, c(numSubjs, maxTrials) )
revtrial <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  Tsubj[i] <- sum(HUD_df$subject == curSubj)  # Tsubj[N]
  useTrials    <- Tsubj[i]
  tmp          <- subset(HUD_df, HUD_df$subject == curSubj)
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
mysamples <- stan(file='/Users/alebedev/GitHub/ILDPII/stan_models/RW_instr_multipleSubj_initFree.stan',   
                  data=dataList, 
                  #                pars=parameters,
                  iter=10000, warmup = 3000,
                  chains=4, 
                  thin=1,     
                  control = list(adapt_delta   = adapt_delta, 
                                 max_treedepth = max_treedepth, 
                                 stepsize      = stepsize) 
)


# Extract the estimated rho parameters:
dd <- extract(mysamples) 
rhos<- apply(dd$P,2,mean)
alpha<- apply(dd$A,2,mean)
evDelta<- apply(dd$EVinit1,2,mean)-apply(dd$EVinit2,2,mean)

mysamplesHUD <- mysamples
save('mysamplesHUD', 'rhos', 'alpha', 'evDelta',
     file= '/Users/alebedev/Documents/Projects/HUD/pupillometry/mysamplesHUD_PP.rda')




