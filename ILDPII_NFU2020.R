######################
# New Follow-up data #
######################
# ILDP_NFU2020.R


library(xlsx)
library(stringi)
rm(list=ls())

# New Follow-up:
fdata <- read.xlsx2(paste('~/Downloads/NFU.xlsx',sep=''),2, stringsAsFactors=F)
fdata_df <- data.frame(email=fdata$VAR1, participation=fdata$VAR4, participation_comm=fdata$VAR4C, 
                       medsY1=fdata$VAR005, medsWhich=fdata$VAR005,
                       diagMDep=fdata$VAR003_1,	diagBP=fdata$VAR003_2,	diagScz=fdata$VAR003_3,
                       diagADHD=fdata$VAR003_4,	diagASD=fdata$VAR003_5, diagOCD=fdata$VAR003_6,
                       diagOther=fdata$VAR003_7,diagOtherWhich=fdata$VAR003C
)


for (i in 1:length(fdata_df$email)){
  # Select ith row:
  tmp <- subset(fdata[i,])
  fdata_df$O[i] <- mean(as.numeric(tmp[,paste0('VAR2_',c(7,8,9))]), na.rm=T)
  fdata_df$C[i] <- mean(as.numeric(tmp[,paste0('VAR2_',c(13,15))]), 7-as.numeric(tmp[,paste0('VAR2_',c(14))]))  
  fdata_df$E[i] <- mean(as.numeric(tmp[,paste0('VAR2_',c(4,5))]), 7-as.numeric(tmp[,paste0('VAR2_',c(6))]))  
  fdata_df$A[i] <- mean(as.numeric(tmp[,paste0('VAR2_',c(11,12))]), 7-as.numeric(tmp[,paste0('VAR2_',c(10))]))  
  fdata_df$N[i] <- mean(as.numeric(tmp[,paste0('VAR2_',c(1,2))]), 7-as.numeric(tmp[,paste0('VAR2_',c(3))]))  
  fdata_df$CONS_public[i] = as.numeric(tmp$AF_1)
  fdata_df$CONS_polit[i]= as.numeric(tmp$AF_2)
  fdata_df$CONS_monit[i]= as.numeric(tmp$AF_3)
  fdata_df$CONS_connect[i]= as.numeric(tmp$AF_4)
  fdata_df$CONS_org[i]= as.numeric(tmp$AF_5)
  fdata_df$ALC_freq[i]= as.numeric(tmp$ALC1)
  fdata_df$ALC_prox[i]= as.numeric(tmp$ALC2)
  fdata_df$ALC_age[i]= tmp$ALC3
  fdata_df$TOB_freq[i]= as.numeric(tmp$TOB1)
  fdata_df$TOB_prox[i]= as.numeric(tmp$TOB2)
  fdata_df$TOB_age[i]= tmp$TOB3
  fdata_df$MDMA_freq[i]= as.numeric(tmp$MDMA1)
  fdata_df$MDMA_prox[i]= as.numeric(tmp$MDMA2)
  fdata_df$MDMA_age[i]= tmp$MDMA3
  fdata_df$CAN_freq[i]= as.numeric(tmp$CAN1)
  fdata_df$CAN_prox[i]= as.numeric(tmp$CAN2)
  fdata_df$CAN_age[i]= tmp$CAN3
  fdata_df$STIM_freq[i]= as.numeric(tmp$STIM1)
  fdata_df$STIM_prox[i]= as.numeric(tmp$STIM2)
  fdata_df$STIM_age[i]= tmp$STIM3
  fdata_df$OPI_freq[i]= as.numeric(tmp$OPI1)
  fdata_df$OPI_prox[i]= as.numeric(tmp$OPI2)
  fdata_df$OPI_age[i]= tmp$OPI3
  fdata_df$PSY_freq[i]= as.numeric(tmp$PSY1)
  fdata_df$PSY_prox[i]= as.numeric(tmp$PSY2)
  fdata_df$PSY_age[i]= tmp$PSY3
  fdata_df$MLQ_presence[i] <- mean(as.numeric(tmp[,paste0('MLQ_',c(1,4,5,6))]), 7-as.numeric(tmp[,paste0('MLQ_',c(9))]))
  fdata_df$MLQ_search[i] <- mean(as.numeric(tmp[,paste0('MLQ_',c(2,3,7,8,10))]))
  fdata_df$SWLS[i] <- mean(as.numeric(tmp[,paste0('SWLS_',c(1:5))]))
  
  fdata_df$EBS_feel[i] <- mean(as.numeric(tmp[,paste0('EBS_feel_',c(1:4))]))
  fdata_df$EBS_evid[i] <- mean(as.numeric(tmp[,paste0('EBS_evid_',c(1:4))]))
  fdata_df$EBS_polit[i] <- mean(as.numeric(tmp[,paste0('EBS_polit_',c(1:4))]))
}

fdata_df$CONS5 <- apply(fdata_df[,c('CONS_public','CONS_polit', 'CONS_monit','CONS_connect', 'CONS_org')],1,mean)

fdata_df$email <- tolower(fdata_df$email)
fdata_df$email <- gsub(",",".",fdata_df$email)
fdata_df$email <- gsub(";",".",fdata_df$email)
fdata_df$email <- gsub(" ","",fdata_df$email)
fdata_df$email[stri_detect_fixed(as.vector(fdata_df$email), 'blank')] <- paste0('blankNFU',sample(1:length(fdata_df$email[stri_detect_fixed(as.vector(fdata_df$email), 'blank')])))
fdata_df <- fdata_df[!duplicated(fdata_df$email,fromLast = T),]
fdata_df$email[fdata_df$email=="ida.frossander@fi.se"] <- "ida.frossander@gmail.com"
fdata_df$email[fdata_df$email=="zuzana.sekakova@gmail.com"] <- "zuzana.sekajova@gmail.com"
fdata_df$email[fdata_df$email=="emeliesegwr@hotmail.se"] <- "emelieseger@hotmail.se" 

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

fdata_df$diagMDep<-as.vector(fdata_df$diagMDep)
fdata_df$diagBP<-as.vector(fdata_df$diagBP)
fdata_df$diagScz<-as.vector(fdata_df$diagScz)
fdata_df$diagADHD<-as.vector(fdata_df$diagADHD)
fdata_df$diagASD<-as.vector(fdata_df$diagASD)
fdata_df$diagOCD<-as.vector(fdata_df$diagOCD)
fdata_df$diagOther<-as.vector(fdata_df$diagOther)
fdata_df$diagOtherWhich<-as.vector(fdata_df$diagOtherWhich)

fdata_df$diagMDep[fdata_df$diagMDep=='999'] <- NA
fdata_df$diagBP[fdata_df$diagBP=='999'] <- NA
fdata_df$diagScz[fdata_df$diagScz=='999'] <- NA
fdata_df$diagADHD[fdata_df$diagADHD=='999'] <- NA
fdata_df$diagASD[fdata_df$diagASD=='999'] <- NA
fdata_df$diagOCD[fdata_df$diagOCD=='999'] <- NA
fdata_df$diagOther[fdata_df$diagOther=='999'] <- NA
fdata_df$diagOtherWhich[fdata_df$diagOtherWhich=='999'] <- NA
fdata_df$diagADHD[is.na(fdata_df$diagADHD)] <- 2
fdata_df$diagASD[is.na(fdata_df$diagASD)] <- 2
fdata_df$diagMDep[is.na(fdata_df$diagMDep)] <- 2
fdata_df$diagBP[is.na(fdata_df$diagBP)] <- 2
fdata_df$diagScz[is.na(fdata_df$diagScz)] <- 2
fdata_df$diagOCD[is.na(fdata_df$diagOCD)] <- 2
fdata_df$diagOther[is.na(fdata_df$diagOther)] <- 2

 


NFU <- fdata_df
save(NFU, file = '/Users/alebedev/Documents/Projects/ILDPII/NFU.rda')


