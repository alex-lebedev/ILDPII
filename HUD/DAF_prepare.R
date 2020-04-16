# DAF_prepare.R
# Andres & Otilia:
######################


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

fdata_df <- cbind(fdata_df, fdata[,60:71])

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

# PSCAN:
# New Follow-up:
fdata_raw <- read.xlsx2(paste('~/Downloads/pscan.xlsx',sep=''),2, stringsAsFactors=F)
scannedIDs <- read.xlsx2(paste('~/Downloads/ILDPII_scanning.xlsx',sep=''),1, stringsAsFactors=F)[,c('study.ID', 'email')]

fdata_df <- data.frame(study.ID=fdata_raw$VAR00)


for (i in 1:length(fdata_df$study.ID)){
  # Select ith row:
  tmp <- subset(fdata_raw[i,])
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

fdata_df <- cbind(fdata_df, fdata_raw[,49:60])

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


############################################################
############################################################
############################################################
############################################################
############################################################
############################################################

# START MERGING:
# load HUD data:
load('/Users/alebedev/Documents/Projects/HUD/HUD_final_old2020.rda')
# load ILDPII screen data:
load('/Users/alebedev/Documents/Projects/ILDPII/ILDPII_screen.rda')
# load ILDPII new follow-up data:
load('/Users/alebedev/Documents/Projects/ILDPII/NFU.rda')
# load ILDPII *scanned* data:
load('/Users/alebedev/Documents/Projects/ILDPII/PSCAN.rda')

# Load HUDMAIN
load('/Users/alebedev/Documents/Projects/HUD/HUDMAIN_df.rda')






#load('/Users/alebedev/Documents/Projects/HUD/HUD_final_old2020.rda')
# load ILDPII screen data:
#load('/Users/alebedev/Documents/Projects/ILDPII/ILDPII_screen.rda')

# Screening:
colselSCR <- colnames(ILDPII_screen)[is.element(colnames(ILDPII_screen),colnames(SCREEN_df))]
ALLSCR <- rbind(SCREEN_df[,colselSCR], ILDPII_screen[,colselSCR])

set.seed(123)
ALLSCR$email[stri_detect_fixed(as.vector(ALLSCR$email), 'blank')] <- paste0('blankSCR',sample(1:length(ALLSCR$email[stri_detect_fixed(as.vector(ALLSCR$email), 'blank')])))
ALLSCR <- ALLSCR[!duplicated(ALLSCR$email,fromLast = T),]

# Apply transforms:
ALLSCR$PDI_totalLog <- log1p(ALLSCR$PDI_total)
ALLSCR$PDI_distLog <- log1p(ALLSCR$PDI_dist)
ALLSCR$PDI_convLog <- log1p(ALLSCR$PDI_conv)
ALLSCR$PDI_timeLog <- log1p(ALLSCR$PDI_time)
ALLSCR$OLIFE_totLog <- log1p(ALLSCR$OLIFE_tot)
ALLSCR$OLIFE_UELog <- log1p(ALLSCR$OLIFE_UE)
ALLSCR$OLIFE_CDLog <- log1p(ALLSCR$OLIFE_CD)
ALLSCR$OLIFE_IALog <- log1p(ALLSCR$OLIFE_IA)
ALLSCR$OLIFE_INLog <- log1p(ALLSCR$OLIFE_IN)
ALLSCR$raads_anyLog <- log1p(ALLSCR$raads_any)
ALLSCR$ASRSLog <- log1p(ALLSCR$ASRS)
ALLSCR$DP <- apply(scale(ALLSCR[,c('PDI_totalLog', 'OLIFE_totLog')]),1,mean)


# DIAGNOSES:
ALLSCR$diagMDep<-as.vector(ALLSCR$diagMDep)
ALLSCR$diagBP<-as.vector(ALLSCR$diagBP)
ALLSCR$diagScz<-as.vector(ALLSCR$diagScz)
ALLSCR$diagADHD<-as.vector(ALLSCR$diagADHD)
ALLSCR$diagASD<-as.vector(ALLSCR$diagASD)
ALLSCR$diagOCD<-as.vector(ALLSCR$diagOCD)
ALLSCR$diagOther<-as.vector(ALLSCR$diagOther)
ALLSCR$diagOtherWhich<-as.vector(ALLSCR$diagOtherWhich)
# ---- #
ALLSCR$diagMDep[ALLSCR$diagMDep=='999'] <- NA
ALLSCR$diagBP[ALLSCR$diagBP=='999'] <- NA
ALLSCR$diagScz[ALLSCR$diagScz=='999'] <- NA
ALLSCR$diagADHD[ALLSCR$diagADHD=='999'] <- NA
ALLSCR$diagASD[ALLSCR$diagASD=='999'] <- NA
ALLSCR$diagOCD[ALLSCR$diagOCD=='999'] <- NA
ALLSCR$diagOther[ALLSCR$diagOther=='999'] <- NA
ALLSCR$diagOtherWhich[ALLSCR$diagOtherWhich=='999'] <- NA
ALLSCR$diagADHD[is.na(ALLSCR$diagADHD)] <- 2
ALLSCR$diagASD[is.na(ALLSCR$diagASD)] <- 2
ALLSCR$diagMDep[is.na(ALLSCR$diagMDep)] <- 2
ALLSCR$diagBP[is.na(ALLSCR$diagBP)] <- 2
ALLSCR$diagScz[is.na(ALLSCR$diagScz)] <- 2
ALLSCR$diagOCD[is.na(ALLSCR$diagOCD)] <- 2
ALLSCR$diagOther[is.na(ALLSCR$diagOther)] <- 2
ALLSCR$PsychDiagAny <- 0
ALLSCR$PsychDiagAny[(ALLSCR$diagADHD==1 | ALLSCR$diagASD==1 | ALLSCR$diagMDep==1 |
                       ALLSCR$diagBP==1 | ALLSCR$diagScz==1 | ALLSCR$diagOCD==1
                     | ALLSCR$diagOther==1)] <- 1
# survey found out:
ALLSCR$surveyfoundout <- as.vector(ALLSCR$surveyfoundout)
ALLSCR$surveyfoundout <- tolower(ALLSCR$surveyfoundout)
ALLSCR$surveyfoundout <- gsub(",",".",ALLSCR$surveyfoundout)
ALLSCR$surveyfoundout <- gsub(";",".",ALLSCR$surveyfoundout)
ALLSCR$surveyfoundout <- gsub(" ","",ALLSCR$surveyfoundout)
ALLSCR$surveyfoundout[stri_detect_fixed(as.vector(ALLSCR$surveyfoundout), 'facebook')] <- 1
ALLSCR$surveyfoundout[stri_detect_fixed(as.vector(ALLSCR$surveyfoundout), 'alexanderlebedev')] <- 1
ALLSCR$surveyfoundout[stri_detect_fixed(as.vector(ALLSCR$surveyfoundout), 'sciencesw')] <- 1
ALLSCR$surveyfoundout[stri_detect_fixed(as.vector(ALLSCR$surveyfoundout), 'karolinska')] <- 2
ALLSCR$surveyfoundout[stri_detect_fixed(as.vector(ALLSCR$surveyfoundout), 'ki')] <- 2
ALLSCR$surveyfoundout[stri_detect_fixed(as.vector(ALLSCR$surveyfoundout), 'kanin')] <- 3
ALLSCR$surveyfoundout[stri_detect_fixed(as.vector(ALLSCR$surveyfoundout), 'forum')] <- 4
ALLSCR$surveyfoundout[stri_detect_fixed(as.vector(ALLSCR$surveyfoundout), 'magi')] <- 4
ALLSCR$surveyfoundout[stri_detect_fixed(as.vector(ALLSCR$surveyfoundout), 'reddit')] <- 4
ALLSCR$surveyfoundout[stri_detect_fixed(as.vector(ALLSCR$surveyfoundout), 'hppd')] <- 4
ALLSCR$surveyfoundout[stri_detect_fixed(as.vector(ALLSCR$surveyfoundout), 'cnv')] <- 4
ALLSCR$surveyfoundout[stri_detect_fixed(as.vector(ALLSCR$surveyfoundout), 'npv')] <- 4
ALLSCR$surveyfoundout[stri_detect_fixed(as.vector(ALLSCR$surveyfoundout), 'vetenskap')] <- 4
ALLSCR$surveyfoundout[stri_detect_fixed(as.vector(ALLSCR$surveyfoundout), 'psykedeliskfors')] <- 4
ALLSCR$surveyfoundout[stri_detect_fixed(as.vector(ALLSCR$surveyfoundout), 'narkotik')] <- 4
ALLSCR$surveyfoundout[stri_detect_fixed(as.vector(ALLSCR$surveyfoundout), 'grupp')] <- 1
ALLSCR$surveyfoundout[stri_detect_fixed(as.vector(ALLSCR$surveyfoundout), 'fb')] <- 1
ALLSCR$surveyfoundout[!is.element(ALLSCR$surveyfoundout, c(1:4))] <- 5
ALLSCR$surveyfoundout[ALLSCR$surveyfoundout==1] <- 'FB'
ALLSCR$surveyfoundout[ALLSCR$surveyfoundout==2 | ALLSCR$surveyfoundout==3] <- 'KI'
ALLSCR$surveyfoundout[ALLSCR$surveyfoundout==4] <- 'DRUGFORUM'
ALLSCR$surveyfoundout[ALLSCR$surveyfoundout==5] <- 'OTHER'
ALLSCR$surveyfoundout <- as.factor(ALLSCR$surveyfoundout)

ALLSCR$sex[ALLSCR$sex==1] <- 'man'
ALLSCR$sex[ALLSCR$sex==2] <- 'kvinna'
ALLSCR <- subset(ALLSCR,ALLSCR$sex!=3) # exclude missing
ALLSCR$sex <- as.factor(as.vector(ALLSCR$sex))
ALLSCR$email[ALLSCR$email=='malina.hansen@gmail.con'] <- 'malina.hansen@gmail.com'


exCols <- c('consentYes1', 'diagMDep', 'diagBP', 'diagScz', 'diagADHD','diagASD','diagOCD',
            'diagOther', 'diagOtherWhich', 'brainInjuryY1','medsY1', 'medsWhich'
)
ALLSCR <- ALLSCR[, !names(ALLSCR) %in% exCols] 



PSCAN_wDemogr <- merge(PSCAN, ALLSCR[c('email',colnames(ALLSCR)[!is.element(colnames(ALLSCR),colnames(PSCAN))])], by='email')

NFU_new <- subset(NFU, is.element(NFU$email,SCRFU_df$email)==F)
PSCAN_wDemogr_new <- subset(PSCAN_wDemogr, is.element(PSCAN_wDemogr$email,SCRFU_df$email)==F)

colselFU <- c("email", "CONS_public","CONS_polit","CONS_monit","CONS_connect",
              "CONS_org","ALC_freq","ALC_prox","TOB_freq","TOB_prox","MDMA_freq",    
              "MDMA_prox","CAN_freq","CAN_prox","STIM_freq","STIM_prox","OPI_freq",     
              "OPI_prox","PSY_freq","PSY_prox","CONS5")

# All conspiracy mentality:
ALLFU <- rbind(NFU_new[,colselFU],CONSP_df[,colselFU],PSCAN_wDemogr_new[,colselFU])
ALLFU$ALC_freqprox <- apply(scale(ALLFU[,c('ALC_freq', 'ALC_prox')]),1,mean)
ALLFU$TOB_freqprox <- apply(scale(ALLFU[,c('TOB_freq', 'TOB_prox')]),1,mean)
ALLFU$CAN_freqprox <- apply(scale(ALLFU[,c('CAN_freq', 'CAN_prox')]),1,mean)
ALLFU$MDMA_freqprox <- apply(scale(ALLFU[,c('MDMA_freq', 'MDMA_prox')]),1,mean)
ALLFU$STIM_freqprox <- apply(scale(ALLFU[,c('STIM_freq', 'STIM_prox')]),1,mean)
ALLFU$OPI_freqprox <- apply(scale(ALLFU[,c('OPI_freq', 'OPI_prox')]),1,mean)
ALLFU$PSY_freqprox <- apply(scale(ALLFU[,c('PSY_freq', 'PSY_prox')]),1,mean)

ALLSCR <- ALLSCR[!duplicated(ALLSCR$email,fromLast = T),]
ALLFU <- ALLFU[!duplicated(ALLFU$email,fromLast = T),]


ALLFU_wDemogr <- merge(ALLFU, ALLSCR[c('email',colnames(ALLSCR)[!is.element(colnames(ALLSCR),colnames(ALLFU))])], by='email' , all=T)
ALLFU_wDemogr <- ALLFU_wDemogr[!is.na(ALLFU_wDemogr$CONS5),]

extraEBS <- subset(PSCAN_wDemogr,is.element(PSCAN_wDemogr$email,NFU$email)==F)
colselEBS <- colnames(NFU)[is.element(colnames(NFU),colnames(extraEBS))]

ALLEBS <- rbind(NFU[,colselEBS],extraEBS[,colselEBS])
ALLEBS_wDemogr <- merge(ALLEBS, ALLSCR[,c('email',colnames(ALLSCR)[!is.element(colnames(ALLSCR),colselEBS)])], by='email' , all=T)
ALLEBS_wDemogr <- ALLEBS_wDemogr[!is.na(ALLEBS_wDemogr$EBS_feel),]

# Anonymize
ALLSCR <- ALLSCR[, !names(ALLSCR) %in% c('email')]
ALLFU_wDemogr <- ALLFU_wDemogr[, !names(ALLFU_wDemogr) %in% c('email')] 
ALLEBS_wDemogr <- ALLEBS_wDemogr[, !names(ALLEBS_wDemogr) %in% c('email')] 

save('ALLSCR','ALLFU_wDemogr','ALLEBS_wDemogr', file='/Users/alebedev/Documents/Projects/ILDPII/AndresOtilia_anonymized.rda')








