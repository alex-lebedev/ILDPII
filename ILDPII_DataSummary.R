# ILDP-II data summary:
rm(list=ls())

library(xlsx)
load('/Users/alebedev/Documents/Projects/HUD/HUD_final2020.rda')

# New Follow-up:
fdata <- read.xlsx2(paste('~/Downloads/NFU.xlsx',sep=''),2, stringsAsFactors=F)
fdata_df <- data.frame(email=fdata$VAR1, participation=fdata$VAR4, participation_comm=fdata$VAR4C, 
                       meds=fdata$VAR005, meds_which=fdata$VAR005,
                       md=fdata$VAR003_1,	bpd=fdata$VAR003_2,	sch=fdata$VAR003_3,
                       adhd=fdata$VAR003_4,	aut=fdata$VAR003_5, ocd=fdata$VAR003_6,
                       other=fdata$VAR003_7,other_which=fdata$VAR003C
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


fdata_df <- fdata_df[!duplicated(fdata_df$email),]

fdata_df$email[fdata_df$email=="ida.frossander@fi.se"] <- "ida.frossander@gmail.com"
fdata_df$email[fdata_df$email=="zuzana.sekakova@gmail.com"] <- "zuzana.sekajova@gmail.com"
fdata_df$email[fdata_df$email=="emeliesegwr@hotmail.se"] <- "emelieseger@hotmail.se" 




tmp <- merge(fdata_df, SCREEN_df, by='email')
#tmp <- merge(fdata_df, SCREEN_df, by='email', all=T)

cor(tmp[,c('SEPI_act','SEPI_demarc','SEPI_iden',
           'SEPI_thought', 'SEPI_consist','SEPI_vit',
           'SEPI_body','SWLS','MLQ_search', 'MLQ_presence')])


tmp$PSY_age <- as.numeric(tmp$PSY_age)
tmp$STIM_age <- as.numeric(tmp$STIM_age)
tmp$ALC_age <- as.numeric(tmp$ALC_age)
tmp$TOB_age <- as.numeric(tmp$TOB_age)
tmp$CAN_age <- as.numeric(tmp$CAN_age)
tmp$OPI_age <- as.numeric(tmp$OPI_age)
tmp$MDMA_age <- as.numeric(tmp$MDMA_age)

tmp$PSY_age[tmp$PSY_age>80] <- NA
tmp$STIM_age[tmp$STIM_age>80] <- NA
tmp$ALC_age[tmp$ALC_age>80] <- NA
tmp$TOB_age[tmp$TOB_age>80] <- NA
tmp$CAN_age[tmp$CAN_age>80] <- NA
tmp$OPI_age[tmp$OPI_age>80] <- NA
tmp$MDMA_age[tmp$MDMA_age>80] <- NA



cor(tmp$PSY_age, tmp$EBS_evid, use='complete.obs')
