library(R.matlab)
library(rstan)
library(rstantools)

resp1 <- readMat('/Users/alebedev/Downloads/rematerialscodefromyour2016elifepaper/scrdata_1rev_stimtype.mat')$Base.to.peak
st1 <- readMat('/Users/alebedev/Downloads/rematerialscodefromyour2016elifepaper/subjdata_1rev_stimtype.mat')$stims

dim(resp[[2]][[1]])


# Elife, 2016:
EXPT <- readMat('/Users/alebedev/Downloads/redelusionpronenessprojectcollaborationwithsweden/EXPT.mat')$EXPT
using <- which(EXPT[,,1]$using==1)

gsr <- readMat('/Users/alebedev/Downloads/redelusionpronenessprojectcollaborationwithsweden/allsubjs_scored_dec.mat')$scr.sqrt.normUS[using]
orders <- readMat('/Users/alebedev/Downloads/redelusionpronenessprojectcollaborationwithsweden/stim_orders_for_learning.mat')
revs <- readMat('/Users/alebedev/Downloads/redelusionpronenessprojectcollaborationwithsweden/exp_rev_design.mat')$revs[using]

cbal <- unlist(EXPT[,,1]$counterbalancing, use.names=FALSE)[using]
vers <- unlist(EXPT[,,1]$version, use.names=FALSE)[using]
instr <- unlist(EXPT[,,1]$instructions, use.names=FALSE)[using]
gsr_sqrt <- gsr


rdat <- data.frame(trial=rep(c(1:80),length(using)), block=rep(rep(1:4,each=20),length(using)), stim=NA, shock=NA, gsr=NA, revtrial=0,subject=rep(1:length(using),each=80)) 

for (i in 1:length(using)){
    gsr_tmp <- as.numeric(c(unlist(gsr[[i]],use.names=FALSE)))
    revs_tmp <- as.numeric(unlist(revs[[i]],use.names=FALSE))
    rdat[rdat$subject==i, 'gsr'][1:length(gsr_tmp)] <- gsr_tmp
    #rdat[(which(rdat$subject==i)[1]-1 + (revs_tmp)),'revtrial']  <- 1
    #rdat[(which(rdat$subject==i)[1]-1 + (revs_tmp)+1),'revtrial']  <- 1
    rdat[(which(rdat$subject==i)[1]-1 + c(20,40,60)+1),'revtrial']  <- 1
    rdat[rdat$subject==i, 'instr'] <- instr[i]
    rdat[rdat$subject==i, 'counterbalancing']<- as.numeric(unlist(EXPT[,,1]$counterbalancing[i],use.names=FALSE))
    
    if (vers[i]=='A'){
      rdat[rdat$subject==i, 'stim'] <- orders$order.A[,1]+1
      rdat[rdat$subject==i, 'shock'] <- orders$order.A[,3]
      rdat[rdat$subject==i, 'order'] <- 'A'
    }
    else {
      rdat[rdat$subject==i, 'stim'] <- orders$order.B[,1]+1
      rdat[rdat$subject==i, 'shock'] <- orders$order.B[,3]
      rdat[rdat$subject==i, 'order'] <- 'B'
    }
}

rdat$gsr[which(rdat$gsr=='NaN')] <- 0
rdat$gsr[which(is.na(rdat$gsr))] <- 0

#rdat <- rdat[!rdat$subject==27,]
#rdat <- rdat[complete.cases(rdat),]
#rdat <- subset(rdat, rdat$instr=='Y')
rdat <- subset(rdat, rdat$instr=='Y')

subjList <- unique(rdat[,'subject'])  # list of subjects x blocks
numSubjs <- length(subjList)  # number of subjects
maxTrials <- max(rdat$trial)


Tsubj <- as.vector(rep(0, numSubjs))
response <- array(0, c(numSubjs, maxTrials) )
stimulus <- array(0, c(numSubjs, maxTrials) )
shock <- array(0, c(numSubjs, maxTrials) )
revtrial <- array(0, c(numSubjs, maxTrials) )


for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  Tsubj[i] <- sum(rdat$subject == curSubj)  # Tsubj[N]
  useTrials    <- Tsubj[i]
  tmp          <- subset(rdat, rdat$subject == curSubj)
  response[i, 1:useTrials]   <- tmp$gsr
  stimulus[i, 1:useTrials]   <- tmp$stim
  shock[i, 1:useTrials]   <- tmp$shock
  revtrial[i, 1:useTrials]   <- tmp$revtrial
}

Tsubj[11]<-78 # subject missing two last trials

dataList <- list(
  N             = numSubjs,
  T             = maxTrials,
  response      = response,
  stimulus      = stimulus,
  shock         = shock,
  Tsubj         = Tsubj,
  revtrial      = revtrial
)

adapt_delta   = 0.95
stepsize      = 1
max_treedepth = 10

### Run Model:
mysamples <- stan(file='/Users/alebedev/GitHub/ILDPII/stan_models/RW_instr_multipleSubj.stan',   
                  data=dataList, 
                  #                pars=parameters,
                  iter=3000, 
                  chains=4, 
                  thin=1,     
                  control = list(adapt_delta   = adapt_delta, 
                                 max_treedepth = max_treedepth, 
                                 stepsize      = stepsize) 
)


# Plots:
plot(mysamples)
pairs(mysamples)
traceplot(mysamples)



# 
dout <- summary(mysamples)
parms <- dout$summary[,c('mean', 'sd')]
df <- as.data.frame((parms[grep('^P_pr', rownames(parms)),'mean']))
df <- as.numeric(as.vector(df[,1]))
summary(glm(V1~V2, data=df))

