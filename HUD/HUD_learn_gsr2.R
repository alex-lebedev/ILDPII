
library(imputeTS)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

slist <- dir('/Users/alebedev/Documents/Projects/HUD/gsr2')
gsrDat <- data.frame(subject=NA, trial=rep(1:80, length(slist)), response=NA)



for (i in 1:length(slist)){
  tmp <- as.numeric(as.vector(read.table(paste0('/Users/alebedev/Documents/Projects/HUD/gsr2/',slist[i]),header = F)[,1]))
  #tmp[tmp<0]<-0
  #tmp <- scale(na_interpolation(tmp))
  tmp <- na_interpolation(tmp)
  #tmp <- tmp-mean(tmp, na.rm=T)
  tmp <- tmp-min(tmp,na.rm=T)
  gsrDat$subject[(80*i-79):(80*i)] <- rep(strsplit(strsplit(slist[i], 'sub-')[[1]][2], '.txt')[[1]],80)
  gsrDat$response[(80*i-79):(80*i)] <- as.numeric(tmp)
}

gsrDat$response <-gsrDat$response/sd(gsrDat$response, na.rm=T)
#gsrDat$response <- (gsrDat$response)^(1/2)

load('/Users/alebedev/Documents/Projects/HUD/pupillometry/HUD_RALT_pupillometry_df.rda')
tmp <- merge(gsrDat, HUD_RALT_pupillometry_df, by=c('subject', 'trial'), sort=F)
cor(tmp$response.x,tmp$response.y, use='complete.obs')


HUD_RALT_gsr_df <- merge(gsrDat, HUD_RALT_pupillometry_df[,c('subject', 'trial', 'stimulus', 'shock', 'revtrial','order', 'group')],
                         by=c('subject', 'trial'), sort=F)
t.test(HUD_RALT_gsr_df$response[HUD_RALT_gsr_df$shock==0],HUD_RALT_gsr_df$response[HUD_RALT_gsr_df$shock==1])

save('HUD_RALT_gsr_df', file='/Users/alebedev/Documents/Projects/HUD/gsr_output/HUD_RALT_gsr2_df.rda')


# Start here:

load('/Users/alebedev/Documents/Projects/HUD/gsr_output/HUD_RALT_gsr2_df.rda')


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

load('/Users/alebedev/Documents/Projects/HUD/gsr_output/HUD_RALT_gsr2_df.rda')
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



load('/Users/alebedev/Documents/Projects/HUD/gsr_output/HUD_RALT_gsr2_df.rda')
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


# Correlations:

load('/Users/alebedev/Documents/Projects/HUD/gsr_output/HUD_RALT_gsr2_df.rda')
HUD_df <- subset(HUD_RALT_gsr_df, HUD_RALT_gsr_df$group=='NP')
idsNP_gsr <- unique(HUD_df$subject)
HUD_df <- subset(HUD_RALT_gsr_df, HUD_RALT_gsr_df$group=='PP')
idsPP_gsr <- unique(HUD_df$subject)
load('/Users/alebedev/Documents/Projects/HUD/gsr_output/mysamplesHUD_PP.rda')
rhosPP_gsr<- rhos
alphaPP_gsr<- alpha
load('/Users/alebedev/Documents/Projects/HUD/gsr_output/mysamplesHUD_NP.rda')
rhosNP_gsr<- rhos
alphaNP_gsr<- alpha
gsr_df <- data.frame(subject=c(idsNP_gsr,idsPP_gsr), rho_gsr=c(rhosNP_gsr,rhosPP_gsr),aplpha_gsr=c(alphaNP_gsr,alphaPP_gsr))



load('/Users/alebedev/Documents/Projects/HUD/pupillometry/HUD_RALT_pupillometry_df.rda')
HUD_df <- subset(HUD_RALT_pupillometry_df, HUD_RALT_pupillometry_df$group=='NP')
idsNP_pup <- unique(HUD_df$subject)
HUD_df <- subset(HUD_RALT_pupillometry_df, HUD_RALT_pupillometry_df$group=='PP')
idsPP_pup <- unique(HUD_df$subject)
load('/Users/alebedev/Documents/Projects/HUD/pupillometry/mysamplesHUD_PP.rda')
rhosPP_pup<- rhos
alphaPP_pup<- alpha
load('/Users/alebedev/Documents/Projects/HUD/pupillometry/mysamplesHUD_NP.rda')
rhosNP_pup<- rhos
alphaNP_pup<- alpha
pup_df <- data.frame(subject=c(idsNP_pup,idsPP_pup), rho_pup=c(rhosNP_pup,rhosPP_pup),aplpha_pup=c(alphaNP_pup,alphaPP_pup))

gsr_df$group <- as.factor(c(rep('NP', length(idsNP_gsr)),rep('PP', length(idsPP_gsr))))
pup_df$group <- as.factor(c(rep('NP', length(idsNP_pup)),rep('PP', length(idsPP_pup))))

all_df <- merge(pup_df,gsr_df, by=c('subject', 'group'))



boxplot(rhosNP_pup, rhosPP_pup, col = c("#009999", "#CC6666"),outline=F, range = 10)
points(cbind(jitter(rep(1, length(rhosNP_pup)))), rhosNP_pup, pch=16, cex=3)
points(cbind(jitter(rep(2, length(rhosPP_pup)))), rhosPP_pup, pch=16, cex=3)


boxplot(rhosNP_gsr, rhosPP_gsr, col = c("#009999", "#CC6666"),outline=F, range = 10)
points(cbind(jitter(rep(1, length(rhosNP_gsr)))), rhosNP_gsr, pch=16, cex=3)
points(cbind(jitter(rep(2, length(rhosPP_gsr)))), rhosPP_gsr, pch=16, cex=3)


plot(all_df$rho_pup, all_df$rho_gsr, type='n')

points(all_df$rho_pup[all_df$group=='NP'], all_df$rho_gsr[all_df$group=='NP'], pch=16, cex=3, col="#009999")
points(all_df$rho_pup[all_df$group=='PP'], all_df$rho_gsr[all_df$group=='PP'], pch=16, cex=3, col="#CC6666")
abline(summary(glm(all_df$rho_gsr~all_df$rho_pup)), lwd=5)



d1 <- data.frame(rho=rhosNP_pup, group=1)
d2 <- data.frame(rho=rhosPP_pup, group=2)
dd <- rbind(d1,d2)
cohen.d(dd$rho,dd$group)



all_df$subject <- paste0('sub-', all_df$subject)
ddd <- merge(all_df, HUDMAIN_df, by='subject')


rho.glm <- glm(rho_gsr ~ PSY_freqprox, data=ddd)
summary(rho.glm)
beta(rho.glm)


