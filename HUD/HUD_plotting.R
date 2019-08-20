
rm(list=ls())

library(rstan)
library(rstantools)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

###############
# HUD- pilot: #
###############

load('/Users/alebedev/GitHub/ILDPII/HUD/pupillometry/finalEYE_d.rda')
#load('/Users/alebedev/GitHub/ILDPII/HUD/pupillometry/finalGSR_d.rda')
load('/Users/alebedev/GitHub/ILDPII/HUD/pupillometry/finalEYE_d_AlexuA1.rda')

final_d <- finalEYE_d
final_d$stimulus <- as.numeric(finalEYE_d$stype)
#final_d <- finalGSR_d

final_d$subject <- 1

subjList <- unique(final_d[,'subject'])  # list of subjects x blocks
numSubjs <- length(subjList)  # number of subjects
maxTrials <- max(final_d$trial)
Tsubj <- as.vector(rep(0, numSubjs))
response <- array(0, c(numSubjs, maxTrials) )
stimulus <- array(0, c(numSubjs, maxTrials) )
shock <- array(0, c(numSubjs, maxTrials) )
revtrial <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  Tsubj[i] <- sum(final_d$subject == curSubj)  # Tsubj[N]
  useTrials    <- Tsubj[i]
  tmp          <- subset(final_d, final_d$subject == curSubj)
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


# Extract the estimated rho parameters:
dd <- extract(mysamples) 
rho<- apply(dd$P,2,mean)
alpha<- apply(dd$A,2,mean)
#EVinit1<- apply(dd$EVinit1,2,mean); EVinit2<- apply(dd$EVinit2,2,mean) # equivalent to EV1/2_ts[1]

# generated EVs:
EV1_ts <- apply(dd$EV1,3,mean)
EV2_ts <- apply(dd$EV2,3,mean)

save(rho, alpha, EV1_ts, EV2_ts, file='/Users/alebedev/GitHub/ILDPII/HUD/pupillometry/finalEYE_d_SAMPLES.rda')
#save(rho, alpha, EV1_ts, EV2_ts, file='/Users/alebedev/GitHub/ILDPII/HUD/pupillometry/finalGSR_d_SAMPLES.rda')


# Plotting:
load('/Users/alebedev/GitHub/ILDPII/HUD/pupillometry/finalEYE_d_SAMPLES.rda')
#load('/Users/alebedev/GitHub/ILDPII/HUD/pupillometry/finalGSR_d_SAMPLES.rda')

ev1 <- as.data.frame(EV1_ts); ev1$type <- 1; ev1$trial <- 1:dim(ev1)[1]; colnames(ev1)[1] <- 'EV'
ev2 <- as.data.frame(EV2_ts); ev2$type <- 2; ev2$trial <- 1:dim(ev2)[1]; colnames(ev2)[1] <- 'EV'
df <- rbind(ev1,ev2)

ggplot(df,aes(x = trial, y = EV)) + 
  geom_point(data=ev1, aes(x = trial, y = EV), size=3, color=2) +geom_line(data=ev1, color=2, size=1.5)+
  geom_point(data=ev2, aes(x = trial, y = EV),size=3, color=3) + geom_line(data=ev2, color=3,size=1.5)+
  theme_bw() +
  theme(axis.text=element_text(size=20), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank())

# Real data:
ev1 <- subset(final_d,final_d$stimulus==1)
ev2 <- subset(final_d,final_d$stimulus==2)
ggplot(final_d,aes(x = trial, y = response)) + 
  geom_point(data=ev1, aes(x = trial, y = response), size=3, color=2) +geom_line(data=ev1, color=2, size=1.5)+
  geom_point(data=ev2, aes(x = trial, y = response),size=3, color=3) + geom_line(data=ev2, color=3,size=1.5)+
  theme_bw() +
  theme(axis.text=element_text(size=20), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank())
