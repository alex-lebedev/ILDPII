
# HUD modeling.
 
rm(list=ls())
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


slist <- dir('/Users/alebedev/Documents/Projects/HUD/gsr')
gsrDat <- data.frame(subject=NA, trial=rep(1:80, length(slist)), response=NA)
measure = 'TTP.AmpSum'
#measure = 'Global.MaxDeflection'


for (i in 1:length(slist)){
  tmp <- read.table(paste0('/Users/alebedev/Documents/Projects/HUD/gsr/',slist[i]),header = T)
  gsrDat$subject[(80*i-79):(80*i)] <- rep(strsplit(strsplit(slist[i], 'HUD_sub-')[[1]][2], '_era.txt')[[1]],80)
  gsrDat$response[(80*i-79):(80*i)] <- tmp[,measure]
}

#gsrDat$response <- sqrt(gsrDat$response)
gsrDat$response <- (gsrDat$response)^(1/2)

load('/Users/alebedev/Documents/Projects/HUD/pupillometry/HUD_RALT_pupillometry_df.rda')
tmp <- merge(gsrDat, HUD_RALT_pupillometry_df, by=c('subject', 'trial'), sort=F)
cor(tmp$response.x,tmp$response.y)

HUD_RALT_gsr_df <- merge(gsrDat, HUD_RALT_pupillometry_df[,c('subject', 'trial', 'stimulus', 'shock', 'revtrial','order', 'group')],
                         by=c('subject', 'trial'), sort=F)
t.test(HUD_RALT_gsr_df$response[HUD_RALT_gsr_df$shock==0],HUD_RALT_gsr_df$response[HUD_RALT_gsr_df$shock==1])

save('HUD_RALT_gsr_df', file='/Users/alebedev/Documents/Projects/HUD/gsr_output/HUD_RALT_gsr_df.rda')


# Start here:

load('/Users/alebedev/Documents/Projects/HUD/gsr_output/HUD_RALT_gsr_df.rda')


HUD_df <- subset(HUD_RALT_gsr_df, HUD_RALT_gsr_df$group=='NP')

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
                  iter=10000, warmup = 2000,
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
     file= '/Users/alebedev/Documents/Projects/HUD/gsr_output/mysamplesHUD_NP.rda')

rm(list=ls())
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('/Users/alebedev/Documents/Projects/HUD/gsr_output/HUD_RALT_gsr_df.rda')
HUD_df <- subset(HUD_RALT_gsr_df, HUD_RALT_gsr_df$group=='PP')

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
                  iter=10000, warmup = 2000,
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
     file= '/Users/alebedev/Documents/Projects/HUD/gsr_output/mysamplesHUD_PP.rda')

load('/Users/alebedev/Documents/Projects/HUD/gsr_output/mysamplesHUD_PP.rda')
rhosPP<- rhos
alphaPP<- alpha

load('/Users/alebedev/Documents/Projects/HUD/gsr_output/mysamplesHUD_NP.rda')
rhosNP<- rhos
alphaNP<- alpha

t.test(rhosNP, rhosPP)
t.test(alphaNP, alphaPP)



load('/Users/alebedev/Documents/Projects/HUD/gsr_output/HUD_RALT_gsr_df.rda')
ids <- unique(HUD_RALT_gsr_df$subject)
respCheck <- data.frame(subject=ids, tv=NA, pv=NA)

for (i in 1:dim(respCheck)[1]){
  tmp <- HUD_RALT_gsr_df[HUD_RALT_gsr_df$subject==ids[i],]
  tr1 <- c(tmp[c(1:20, 41:60),])
  tr2 <- c(tmp[c(21:40, 61:80),])
  respCheck$tv[i] <- t.test(c(tr1$response[tr1$stimulus==1 & tr1$shock!=1],tr2$response[tr2$stimulus==2 & tr2$shock!=1]),
                            c(tr1$response[tr1$stimulus==2 & tr1$shock!=1],tr2$response[tr2$stimulus==1 & tr2$shock!=1]))$statistic
  respCheck$pv[i] <- t.test(c(tr1$response[tr1$stimulus==1 & tr1$shock!=1],tr2$response[tr2$stimulus==2 & tr2$shock!=1]),
                            c(tr1$response[tr1$stimulus==2 & tr1$shock!=1],tr2$response[tr2$stimulus==1 & tr2$shock!=1]),'greater')$p.value
}
inclSubj <- respCheck$subject[which(respCheck$pv<0.05)]


load('/Users/alebedev/Documents/Projects/HUD/gsr_output/mysamplesHUD_PP.rda')
rhosPP<- rhos
alphaPP<- alpha
load('/Users/alebedev/Documents/Projects/HUD/gsr_output/mysamplesHUD_NP.rda')
rhosNP<- rhos
alphaNP<- alpha

HUD_np <- subset(HUD_RALT_gsr_df, HUD_RALT_gsr_df$group=='NP')
HUD_pp <- subset(HUD_RALT_gsr_df, HUD_RALT_gsr_df$group=='PP')



rhosNP<- rhosNP[which(is.element(unique(HUD_np$subject), inclSubj))]
rhosPP<- rhosPP[which(is.element(unique(HUD_pp$subject), inclSubj))]
alphaNP<- alphaNP[which(is.element(unique(HUD_np$subject), inclSubj))]
alphaPP<- alphaPP[which(is.element(unique(HUD_pp$subject), inclSubj))]

t.test(rhosNP, rhosPP)
t.test(alphaNP, alphaPP)


