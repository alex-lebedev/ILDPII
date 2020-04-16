library(R.matlab)
library(rstan)
library(rstantools)


# Elife, 2016:
EXPT <- readMat('/Users/alebedev/Downloads/redelusionpronenessprojectcollaborationwithsweden/EXPT.mat')$EXPT
using <- EXPT[,,1]$using==1

gsr <- readMat('/Users/alebedev/Downloads/redelusionpronenessprojectcollaborationwithsweden/allsubjs_scored_dec.mat')$scr.sqrt.normUS
orders <- readMat('/Users/alebedev/Downloads/redelusionpronenessprojectcollaborationwithsweden/stim_orders_for_learning.mat')
revs <- readMat('/Users/alebedev/Downloads/redelusionpronenessprojectcollaborationwithsweden/exp_rev_design.mat')$revs

cbal <- unlist(EXPT[,,1]$counterbalancing, use.names=FALSE)
vers <- unlist(EXPT[,,1]$version, use.names=FALSE)
instr <- unlist(EXPT[,,1]$instructions, use.names=FALSE)
gsr_sqrt <- gsr


rdat <- data.frame(trial=rep(c(1:80),length(using)), block=rep(rep(1:4,each=20),length(using)), stim=NA, shock=NA, gsr=NA, revtrial=0,subject=rep(1:length(using),each=80)) 



for (i in c(1:71)[using]){
    gsr_tmp <- as.numeric(c(unlist(gsr[[i]],use.names=FALSE)))
    revs_tmp <- as.numeric(unlist(revs[[i]],use.names=FALSE))
    rdat[rdat$subject==i, 'gsr'][1:length(gsr_tmp)] <- gsr_tmp
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

rdat <- subset(rdat, is.element(rdat$subject,which(using)))

sALL_ <- rdat[,c('subject', 'trial', 'stim', 'gsr','shock', 'revtrial', 'instr')]
sALL_$order <- paste(rdat$order,rdat$counterbalancing, sep='')
colnames(sALL_) <- c("subject","trial","stimulus","response","shock","revtrial","instr","order"   )
sALL_eLife<-sALL_

save('sALL_eLife', file = '/Users/alebedev/GitHub/ILDPII/Pilot_2/sALL_eLife.rda')

# Sanity check:
instr1 <-subset(sALL_eLife$response, sALL_eLife$stimulus==1 & sALL_eLife$instr=='Y' & is.element(sALL_eLife$trial,1:20))
instr2 <-subset(sALL_eLife$response, sALL_eLife$stimulus==2 & sALL_eLife$instr=='Y' & is.element(sALL_eLife$trial,1:20))

uninstr1 <-subset(sALL_eLife$response, sALL_eLife$stimulus==1 & sALL_eLife$instr=='X' & is.element(sALL_eLife$trial,1:20))
uninstr2 <-subset(sALL_eLife$response, sALL_eLife$stimulus==2 & sALL_eLife$instr=='X' & is.element(sALL_eLife$trial,1:20))

