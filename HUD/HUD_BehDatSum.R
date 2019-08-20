# HUD_BehDatSum.R
# (Summarizing behavioural data)
# HUD: experimenal data preparation:

# RALT (subjective ratings):
raltList <- list.files('/Users/alebedev/Documents/Projects/HUD/performance/RALT/', '*.csv')

ralt <- data.frame(subject=rep(NA, length(raltList)), version=NA, language=NA,
                   FaceCSP1pre=NA, FaceCSP2pre=NA, FaceCSM1pre=NA,FaceCSM2pre=NA,
                   FaceCSP1post=NA, FaceCSP2post=NA, FaceCSM1post=NA,FaceCSM2post=NA)
                  
for (i in 1:length(raltList)){
  ralt$subject[i] <- paste('sub-',strsplit(raltList[i], "[[:punct:][:space:]]+")[[1]][5],sep='')
  ralt$version[i] <- strsplit(raltList[i], "[[:punct:][:space:]]+")[[1]][2]
  ralt$language[i] <- strsplit(raltList[i], "[[:punct:][:space:]]+")[[1]][3]
  tmp <- read.csv(paste('/Users/alebedev/Documents/Projects/HUD/performance/RALT/', raltList[i], sep=''))
  # Ratings:
  if (max(tmp$rating_pre.thisIndex, na.rm=T)==3){
    ralt$FaceCSP1pre[i] <- tmp$rating_scale.response[which(tmp$rating_pre.thisIndex==0)]
    ralt$FaceCSP2pre[i] <- tmp$rating_scale.response[which(tmp$rating_pre.thisIndex==2)]
    ralt$FaceCSM1pre[i] <- tmp$rating_scale.response[which(tmp$rating_pre.thisIndex==1)]
    ralt$FaceCSM2pre[i] <- tmp$rating_scale.response[which(tmp$rating_pre.thisIndex==3)]
    ralt$FaceCSP1post[i] <- tmp$rating_scale.response[which(tmp$rating_post.thisIndex==0)]
    ralt$FaceCSP2post[i] <- tmp$rating_scale.response[which(tmp$rating_post.thisIndex==2)]
    ralt$FaceCSM1post[i] <- tmp$rating_scale.response[which(tmp$rating_post.thisIndex==1)]
    ralt$FaceCSM2post[i] <- tmp$rating_scale.response[which(tmp$rating_post.thisIndex==3)]
  } else {
    ralt$FaceCSP1pre[i] <- tmp$rating_scale.response[which(tmp$rating_pre.thisIndex==0)]
    ralt$FaceCSP2pre[i] <- tmp$rating_scale.response[which(tmp$rating_pre.thisIndex==2)]
    ralt$FaceCSM1pre[i] <- tmp$rating_scale.response[which(tmp$rating_pre.thisIndex==1)]
    ralt$FaceCSP1post[i] <- tmp$rating_scale.response[which(tmp$rating_post.thisIndex==0)]
    ralt$FaceCSP2post[i] <- tmp$rating_scale.response[which(tmp$rating_post.thisIndex==2)]
    ralt$FaceCSM1post[i] <- tmp$rating_scale.response[which(tmp$rating_post.thisIndex==1)]
  }
}
ralt$raltCSPpre <- apply(ralt[,c('FaceCSP1pre','FaceCSP2pre')],1,mean,na.rm=T)
ralt$raltCSPpost <- apply(ralt[,c('FaceCSP1post','FaceCSP2post')],1,mean,na.rm=T)
ralt$raltCSMpre <- apply(ralt[,c('FaceCSM1pre','FaceCSM2pre')],1,mean,na.rm=T)
ralt$raltCSMpost <- apply(ralt[,c('FaceCSM1post','FaceCSM2post')],1,mean,na.rm=T)

# RALT (model-derived measures):
load('/Users/alebedev/Documents/Projects/HUD/pupillometry/mysamplesHUD_PP.rda')
mysamplesPP <- mysamplesHUD
rhosPP <- rhos
alphaPP <- alpha
evDeltaPP <- evDelta
load('/Users/alebedev/Documents/Projects/HUD/pupillometry/mysamplesHUD_NP.rda')
mysamplesNP <- mysamplesHUD
rhosNP <- rhos
alphaNP <- alpha
evDeltaNP <- evDelta
load('/Users/alebedev/Documents/Projects/HUD/pupillometry/HUD_RALT_pupillometry_df.rda')
tmp <- subset(HUD_RALT_pupillometry_df, HUD_RALT_pupillometry_df$trial==1)
raltLearn <- data.frame(subject=paste('sub-',tmp$subject, sep=''), version=tmp$order, group=tmp$group)
raltLearn$rhos[raltLearn$group=='NP'] <- rhosNP
raltLearn$rhos[raltLearn$group=='PP'] <- rhosPP
raltLearn$alpha[raltLearn$group=='NP'] <- alphaNP
raltLearn$alpha[raltLearn$group=='PP'] <- alphaPP
raltLearn$evDelta[raltLearn$group=='NP'] <- evDeltaNP
raltLearn$evDelta[raltLearn$group=='PP'] <- evDeltaPP
ralt <- merge(ralt, raltLearn, by=c('subject', 'version'))


# DOTS:
dotsList <- list.files('/Users/alebedev/Documents/Projects/HUD/performance/DOTS/', '*.csv')
dots <- data.frame(subject = rep(NA, length(dotsList)),
                     AccST = rep(NA, length(dotsList)), AccDYN = rep(NA, length(dotsList)),
                     AccSTfast= rep(NA, length(dotsList)), AccSTslow= rep(NA, length(dotsList)),
                     AccDYNfast= rep(NA, length(dotsList)), AccDYNslow= rep(NA, length(dotsList)),
                     rtST = rep(NA, length(dotsList)), rtDYN = rep(NA, length(dotsList)),
                     rtSTfast= rep(NA, length(dotsList)), rtSTslow= rep(NA, length(dotsList)),
                     rtDYNfast= rep(NA, length(dotsList)), rtDYNslow= rep(NA, length(dotsList))
)


for (i in 1:length(dotsList)){
  dots$subject[i] <- paste('sub-',strsplit(dotsList[i], "[[:punct:][:space:]]+")[[1]][3], sep='')
  tmp <- read.csv(paste('/Users/alebedev/Documents/Projects/HUD/performance/DOTS/', dotsList[i], sep=''))
  # Accuracies:
  dots$AccST[i] <- mean(tmp$resp_stat.corr, na.rm=T)
  dots$AccDYN[i] <- mean(tmp$resp_dyn.corr, na.rm=T)
  dots$AccSTslow[i] <- mean(tmp$resp_stat.corr[tmp$speed<0.001], na.rm=T)
  dots$AccDYNslow[i] <- mean(tmp$resp_dyn.corr[tmp$speed<0.001], na.rm=T)
  dots$AccSTfast[i] <- mean(tmp$resp_stat.corr[tmp$speed>=0.001], na.rm=T)
  dots$AccDYNfast[i] <- mean(tmp$resp_dyn.corr[tmp$speed>=0.001], na.rm=T)
  # RTs:
  dots$rtST[i] <- mean(tmp$resp_stat.rt, na.rm=T)
  dots$rtDYN[i] <- mean(tmp$resp_dyn.rt, na.rm=T)
  dots$rtSTslow[i] <- mean(tmp$resp_stat.rt[tmp$speed<0.001], na.rm=T)
  dots$rtDYNslow[i] <- mean(tmp$resp_dyn.rt[tmp$speed<0.001], na.rm=T)
  dots$rtSTfast[i] <- mean(tmp$resp_stat.rt[tmp$speed>=0.001], na.rm=T)
  dots$rtDYNfast[i] <- mean(tmp$resp_dyn.rt[tmp$speed>=0.001], na.rm=T)
}

# N-back:
# Prepare a list of files:
nbfolder <- '/Users/alebedev/Documents/Projects/HUD/performance/NBACK/'
nblist <- list.files(path=nbfolder, pattern="*.csv") 
nback <- matrix(NA,length(nblist), 3)
for (i in 1:dim(nback)[1]){
  subject <- paste('sub-',strsplit(nblist[i], "[[:punct:][:space:]]+")[[1]][3], sep='')
  subjF <- read.csv(paste(nbfolder,nblist[i], sep=''))
  # n1:
  subj_n1 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n1')
  sens1_n1 <- table(subj_n1$cr, subj_n1$resp.keys)['space','space']/table(subj_n1$cr)['space']
  spec1_n1 <- table(subj_n1$cr, subj_n1$resp.keys)['None','None']/table(subj_n1$cr)['None']
  # n2:
  subj_n2 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n2')
  sens1_n2 <- table(subj_n2$cr, subj_n2$resp.keys)['space','space']/table(subj_n2$cr)['space']
  spec1_n2 <- table(subj_n2$cr, subj_n2$resp.keys)['None','None']/table(subj_n2$cr)['None']
  # Overall accuracy:
  oa_n1 <- mean(c(sens1_n1,spec1_n1))
  oa_n2 <- mean(c(sens1_n2,spec1_n2))
  nback[i,] <-  c(subject, oa_n1,oa_n2)
}

nback <- as.data.frame(nback)
colnames(nback) <- c('subject','oa1', 'oa2')
nback$oa1 <- as.numeric(as.vector(nback$oa1))
nback$oa2 <- as.numeric(as.vector(nback$oa2))
nback$badNBperf <- nback$oa2<median(nback$oa2)

# BADE:
bade <- read.csv2('/Users/alebedev/Documents/Projects/HUD/performance/BADEscores.csv',stringsAsFactors=F)[,1:6]
bade[,c(3:6)] <- apply(bade[,c(3:6)],2,as.numeric)

# Merge experimental data:
EXPDAT_df <- merge(ralt, dots, by='subject', all=T)
EXPDAT_df <- merge(EXPDAT_df, nback, by='subject', all=T)
EXPDAT_df <- merge(EXPDAT_df, bade, by='subject', all=T)


# BEHAVIOURAL SOURCE DATA:
esids <- read.xlsx2('/Users/alebedev/Documents/Projects/HUD/visits/tested.xlsx', 1)
load('/Users/alebedev/Documents/Projects/HUD/HUD.rda')
esids$email <- gsub(" ","",tolower(esids$email))
esids$email <- gsub(";","",tolower(esids$email))
BEHSOURCE_df <- merge(SCRFU_df, esids, by='email')
BEHSOURCE_df <- BEHSOURCE_df[!duplicated(BEHSOURCE_df$ID.y),]
colnames(BEHSOURCE_df)[which(colnames(BEHSOURCE_df)=='ID.y')]<-'subject'
BEHSOURCE_df$ShockLevel <- as.numeric(as.vector(BEHSOURCE_df$ShockLevel))
BEHSOURCE_df <- merge(BEHSOURCE_df, CONSP_df[,c('email','group','CONS_public', 'CONS_polit',
                              'CONS_monit', 'CONS_connect', 'CONS_org', 'CONS_glob', 'CONS_prog',
                              'CONS_decisions', 'CONS_diseases', 'CONS_gov', 'ALC_freq', 'ALC_prox',
                              'TOB_freq', 'TOB_prox', 'MDMA_freq', 'MDMA_prox', 'CAN_freq', 'CAN_prox',
                              'STIM_freq', 'STIM_prox', 'OPI_freq', 'OPI_prox', 'PSY_freq', 'PSY_prox',
                              'ALC_freqprox', 'TOB_freqprox', 'MDMA_freqprox', 'CAN_freqprox',
                              'STIM_freqprox', 'OPI_freqprox','PSY_freqprox', 'CONS5', 'CONSALL')], by=c('email', 'group'))



HUDMAIN_df <- merge(EXPDAT_df, BEHSOURCE_df, by=c('subject'), all=T)


HUDMAIN_df$RTdiffBGD <- HUDMAIN_df$rtDYN-HUDMAIN_df$rtST
HUDMAIN_df$RTdiffST <- HUDMAIN_df$rtSTslow-HUDMAIN_df$rtSTfast
HUDMAIN_df$RTdiffDYN <- HUDMAIN_df$rtDYNslow-HUDMAIN_df$rtDYNfast



SCREEN_df$group <- NA
SCREEN_df$group[SCREEN_df$drug_psychedelics==0] <- 'NP'
SCREEN_df$group[SCREEN_df$drug_psychedelics==1] <- 'PP'


save(HUDMAIN_df, file='/Users/alebedev/Documents/Projects/HUD/HUDMAIN_df.rda')

SCREEN_df <- SCREEN_df[,-which(colnames(SCREEN_df)=='email')]
SCRFU_df <- SCRFU_df[,-which(colnames(SCRFU_df)=='email')]
CONSP_df <- CONSP_df[,-which(colnames(CONSP_df)=='email')]
HUDMAIN_df <- HUDMAIN_df[,-which(colnames(HUDMAIN_df)=='email')]

# Final data
save(SCREEN_df, SCRFU_df, HUDMAIN_df, CONSP_df, file='/Users/alebedev/Documents/Projects/HUD/HUD_anonymized.rda')


#d <- cor(HUDMAIN_df[,c(5:39,42:46, 50,76:111,114:143)], use='complete.obs')
#d['rhos',]


# CLEANING:
rm(list=ls())
load('/Users/alebedev/Documents/Projects/HUD/HUD_anonymized.rda')


# Outlier replacement:

# Clone source-files:
SCREEN_df_cleaned <- as.data.frame(SCREEN_df)
SCREEN_df_cleaned_NP <- subset(SCREEN_df_cleaned, SCREEN_df_cleaned$group=='NP')
SCREEN_df_cleaned_PP <- subset(SCREEN_df_cleaned, SCREEN_df_cleaned$group=='PP')






# NP:
outls <- matrix(F, dim(SCREEN_df_cleaned_NP)[1], dim(SCREEN_df_cleaned_NP)[2])
for (v in c(30:56,58:65)){
  Q13 <- quantile(SCREEN_df_cleaned_NP[,v], c(.25, .75), na.rm=T) 
  Upper = Q13[2] + (3 * (Q13[2] - Q13[1]))
  Lower = Q13[1]- (3 * (Q13[2] - Q13[1]))
  c(Upper, Lower)
  outs <- which(SCREEN_df_cleaned_NP[,v]<Lower | SCREEN_df_cleaned_NP[,v]>Upper)
  if (length(outs)==0){
  } else {
    outls[outs,v] <- T
  }
}


table(outls)
which(outls == T, arr.ind=TRUE)
names(SCREEN_df_cleaned_NP[which(outls == T, arr.ind=TRUE)[,2]])
SCREEN_df_cleaned_NP[outls] <- NA 
((table(outls)[2]-1)/sum(table(outls)))*100
# without 1 bianry-variable (not used in the analysis), it's 4 data-entries: 
# ((table(outls)[2]-1)/sum(table(outls)))*100 = 0.33%



# V2-Active:
outls <- matrix(F, dim(SCREEN_df_cleaned_PP)[1], dim(SCREEN_df_cleaned_PP)[2])
for (v in 3:dim(cogdat.v2.act)[2]){
  Q13 <- quantile(cogdat.v2.act[,v], c(.25, .75), na.rm=T) 
  Upper = Q13[2] + (3 * (Q13[2] - Q13[1]))
  Lower = Q13[1]- (3 * (Q13[2] - Q13[1]))
  c(Upper, Lower)
  outs <- which(cogdat.v2.act[,v]<Lower | cogdat.v2.act[,v]>Upper)
  if (length(outs)==0){
  } else {
    outls[outs,v] <- T
  }
}
table(outls)
which(outls == T, arr.ind=TRUE)
names(SCREEN_df_cleaned_PP[which(outls == T, arr.ind=TRUE)[,2]])
SCREEN_df_cleaned_PP[outls] <- NA 
((table(outls)[2])/sum(table(outls)))*100
# it's 2 data-entries: 
# ((table(outls)[2])/sum(table(outls)))*100 = 0.16%




