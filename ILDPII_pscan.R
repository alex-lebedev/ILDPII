#ILDPII_pscan.R


library(xlsx)
rm(list=ls())

# New Follow-up:
fdata <- read.xlsx2(paste('~/Downloads/pscan.xlsx',sep=''),2, stringsAsFactors=F)
scannedIDs <- read.xlsx2(paste('~/Downloads/ILDPII_scanning.xlsx',sep=''),1, stringsAsFactors=F)[,c('study.ID', 'email')]

fdata_df <- data.frame(study.ID=fdata$VAR00)


for (i in 1:length(fdata_df$study.ID)){
  # Select ith row:
  tmp <- subset(fdata[i,])
  fdata_df$CONS_public[i] = as.numeric(tmp$AF_1)
  fdata_df$CONS_polit[i]= as.numeric(tmp$AF_2)
  fdata_df$CONS_monit[i]= as.numeric(tmp$AF_3)
  fdata_df$CONS_connect[i]= as.numeric(tmp$AF_4)
  fdata_df$CONS_org[i]= as.numeric(tmp$AF_5)
  fdata_df$ALC_freq[i]= as.numeric(tmp$ALC1)
  fdata_df$ALC_prox[i]= as.numeric(tmp$ALC2)
  fdata_df$ALC_age[i]= tmp$VAR04
  fdata_df$TOB_freq[i]= as.numeric(tmp$TOB1)
  fdata_df$TOB_prox[i]= as.numeric(tmp$TOB2)
  fdata_df$TOB_age[i]= tmp$VAR07
  fdata_df$MDMA_freq[i]= as.numeric(tmp$MDMA1)
  fdata_df$MDMA_prox[i]= as.numeric(tmp$MDMA2)
  fdata_df$MDMA_age[i]= tmp$VAR10
  fdata_df$CAN_freq[i]= as.numeric(tmp$CAN1)
  fdata_df$CAN_prox[i]= as.numeric(tmp$CAN2)
  fdata_df$CAN_age[i]= tmp$VAR13
  fdata_df$STIM_freq[i]= as.numeric(tmp$STIM1)
  fdata_df$STIM_prox[i]= as.numeric(tmp$STIM2)
  fdata_df$STIM_age[i]= tmp$VAR16
  fdata_df$OPI_freq[i]= as.numeric(tmp$OPI1)
  fdata_df$OPI_prox[i]= as.numeric(tmp$OPI2)
  fdata_df$OPI_age[i]= tmp$VAR19
  fdata_df$PSY_freq[i]= as.numeric(tmp$PSY1)
  fdata_df$PSY_prox[i]= as.numeric(tmp$PSY2)
  fdata_df$PSY_age[i]= tmp$VAR22
  fdata_df$MLQ_presence[i] <- mean(as.numeric(tmp[,paste0('MLQ_',c(1,4,5,6))]), 7-as.numeric(tmp[,paste0('MLQ_',c(9))]))
  fdata_df$MLQ_search[i] <- mean(as.numeric(tmp[,paste0('MLQ_',c(2,3,7,8,10))]))
  fdata_df$SWLS[i] <- mean(as.numeric(tmp[,paste0('SWLS_',c(1:5))]))
  fdata_df$EBS_feel[i] <- mean(as.numeric(tmp[,paste0('EBS_feel_',c(1:4))]))
  fdata_df$EBS_evid[i] <- mean(as.numeric(tmp[,paste0('EBS_evid_',c(1:4))]))
  fdata_df$EBS_polit[i] <- mean(as.numeric(tmp[,paste0('EBS_polit_',c(1:4))]))
}

fdata_df$CONS5 <- apply(fdata_df[,c('CONS_public','CONS_polit', 'CONS_monit','CONS_connect', 'CONS_org')],1,mean)


fdata_df$ALC_prox[fdata_df$ALC_prox==1] <- 9
fdata_df$TOB_prox[fdata_df$TOB_prox==1] <- 9
fdata_df$CAN_prox[fdata_df$CAN_prox==1] <- 9
fdata_df$MDMA_prox[fdata_df$MDMA_prox==1] <- 9
fdata_df$STIM_prox[fdata_df$STIM_prox==1] <- 9
fdata_df$OPI_prox[fdata_df$OPI_prox==1] <- 9
fdata_df$PSY_prox[fdata_df$PSY_prox==1] <- 9
fdata_df[,c('ALC_prox', 'TOB_prox', 'CAN_prox','MDMA_prox','STIM_prox', 'OPI_prox', 'PSY_prox')] <- 
  10-fdata_df[,c('ALC_prox', 'TOB_prox', 'CAN_prox','MDMA_prox','STIM_prox', 'OPI_prox', 'PSY_prox')]

fdata_df$PSY_age <- as.numeric(fdata_df$PSY_age)
fdata_df$STIM_age <- as.numeric(fdata_df$STIM_age)
fdata_df$ALC_age <- as.numeric(fdata_df$ALC_age)
fdata_df$TOB_age <- as.numeric(fdata_df$TOB_age)
fdata_df$CAN_age <- as.numeric(fdata_df$CAN_age)
fdata_df$OPI_age <- as.numeric(fdata_df$OPI_age)
fdata_df$MDMA_age <- as.numeric(fdata_df$MDMA_age)
fdata_df$PSY_age[fdata_df$PSY_age>80] <- NA
fdata_df$STIM_age[fdata_df$STIM_age>80] <- NA
fdata_df$ALC_age[fdata_df$ALC_age>80] <- NA
fdata_df$TOB_age[fdata_df$TOB_age>80] <- NA
fdata_df$CAN_age[fdata_df$CAN_age>80] <- NA
fdata_df$OPI_age[fdata_df$OPI_age>80] <- NA
fdata_df$MDMA_age[fdata_df$MDMA_age>80] <- NA
PSCAN <- fdata_df
PSCAN$study.ID <- gsub(" ","",PSCAN$study.ID)
scannedIDs$study.ID <- gsub(" ","",scannedIDs$study.ID)
PSCAN <- merge(scannedIDs,PSCAN, by='study.ID')
PSCAN$email <- gsub(" ","",PSCAN$email)
save(PSCAN, file = '/Users/alebedev/Documents/Projects/ILDPII/PSCAN.rda')






