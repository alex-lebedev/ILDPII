############################
# Prepare final ILDPII set #
############################
# load libraries:
library(xlsx) 
library(stringi)
# Clear workspace:
rm(list=ls())

# Run ILDPII_NewScreen.R and ILDP_NFU2020.R
source('~/GitHub/ILDPII/HUD/HUD_complete_data_summary.R')
source('~/GitHub/ILDPII/ILDPII_NewScreen.R')
source('~/GitHub/ILDPII/ILDPII_NFU2020.R')
source('~/GitHub/ILDPII/ILDPII_pscan.R')


# START MERGING:
rm(list=ls())
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
ALLSCR$DP <- apply(scale(ALLSCR[,c('PDI_total', 'OLIFE_tot')]),1,mean)
ALLSCR$DP <- log1p(ALLSCR$DP-min(ALLSCR$DP))

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
ALLSCR <- ALLSCR[!duplicated(ALLSCR$email,fromLast = T),]
ALLFU <- ALLFU[!duplicated(ALLFU$email,fromLast = T),]

ALLFU_wDemogr <- merge(ALLFU, ALLSCR[c('email',colnames(ALLSCR)[!is.element(colnames(ALLSCR),colnames(ALLFU))])], by='email' , all=T)
ALLFU_wDemogr <- ALLFU_wDemogr[!is.na(ALLFU_wDemogr$CONS5),]

extraEBS <- subset(PSCAN_wDemogr,is.element(PSCAN_wDemogr$email,NFU$email)==F)
colselEBS <- colnames(NFU)[is.element(colnames(NFU),colnames(extraEBS))]
ALLEBS <- rbind(NFU[,colselEBS],extraEBS[,colselEBS])
ALLEBS_wDemogr <- merge(ALLEBS, ALLSCR[,c('email',colnames(ALLSCR)[!is.element(colnames(ALLSCR),colselEBS)])], by='email' , all=T)
ALLEBS_wDemogr <- ALLEBS_wDemogr[!is.na(ALLEBS_wDemogr$EBS_feel),]

tmp <- subset(ALLEBS_wDemogr, is.element(ALLEBS_wDemogr$email,ALLFU_wDemogr$email)==F)
ALLFU_wDemogr <- rbind(ALLFU_wDemogr[,colnames(ALLFU_wDemogr)], tmp[,colnames(ALLFU_wDemogr)])
ALLFU_wDemogr$ALC_freqprox <- apply(scale(ALLFU_wDemogr[,c('ALC_freq', 'ALC_prox')]),1,mean)
ALLFU_wDemogr$TOB_freqprox <- apply(scale(ALLFU_wDemogr[,c('TOB_freq', 'TOB_prox')]),1,mean)
ALLFU_wDemogr$CAN_freqprox <- apply(scale(ALLFU_wDemogr[,c('CAN_freq', 'CAN_prox')]),1,mean)
ALLFU_wDemogr$MDMA_freqprox <- apply(scale(ALLFU_wDemogr[,c('MDMA_freq', 'MDMA_prox')]),1,mean)
ALLFU_wDemogr$STIM_freqprox <- apply(scale(ALLFU_wDemogr[,c('STIM_freq', 'STIM_prox')]),1,mean)
ALLFU_wDemogr$OPI_freqprox <- apply(scale(ALLFU_wDemogr[,c('OPI_freq', 'OPI_prox')]),1,mean)
ALLFU_wDemogr$PSY_freqprox <- apply(scale(ALLFU_wDemogr[,c('PSY_freq', 'PSY_prox')]),1,mean)

HUDMAIN_df <- merge(HUDMAIN_df[, !names(HUDMAIN_df) %in% c('DP')] , ALLSCR[,c('email','DP',colnames(ALLSCR)[!is.element(colnames(ALLSCR),colnames(HUDMAIN_df))])], by='email' , all=T)
HUDMAIN_df <- HUDMAIN_df[!is.na(HUDMAIN_df$oa1),]

save('ALLSCR','ALLFU','ALLEBS','ALLEBS_wDemogr', 'ALLFU_wDemogr', 'HUDMAIN_df', 'PSCAN_wDemogr',file='/Users/alebedev/Documents/Projects/HUD/HUD_final_mergedApril2020.rda')



# Anonymize
ALLSCR <- ALLSCR[, !names(ALLSCR) %in% c('email')]
ALLFU <- ALLFU[, !names(ALLFU) %in% c('email')] 
ALLEBS <- ALLEBS[, !names(ALLEBS) %in% c('email')] 
ALLEBS_wDemogr <- ALLEBS_wDemogr[, !names(ALLEBS_wDemogr) %in% c('email')] 
ALLFU_wDemogr <- ALLFU_wDemogr[, !names(ALLFU_wDemogr) %in% c('email')] 
HUDMAIN_df <- HUDMAIN_df[, !names(HUDMAIN_df) %in% c('email')] 
PSCAN_wDemogr <- PSCAN_wDemogr[, !names(PSCAN_wDemogr) %in% c('email')] 

save('ALLSCR','ALLFU','ALLEBS','ALLEBS_wDemogr', 'ALLFU_wDemogr', 'HUDMAIN_df', 'PSCAN_wDemogr',file='/Users/alebedev/Documents/Projects/HUD/HUD_final_mergedApril2020_anonymized.rda')
