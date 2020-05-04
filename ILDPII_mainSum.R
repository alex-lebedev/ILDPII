############################
# Prepare final ILDPII set #
############################
# load libraries:
library(xlsx)
library(stringi)
# Clear workspace:
rm(list=ls())

# Run ILDPII_NewScreen.R and ILDP_NFU2020.R
source('~/GitHub/ILDPII/ILDPII_NewScreen.R')
source('~/GitHub/ILDPII/ILDPII_NFU2020.R')
source('~/GitHub/ILDPII/ILDPII_pscan.R')


# START MERGING:
rm(list=ls())
# load HUD data:
load('/Users/alebedev/Documents/Projects/HUD/HUD_final_March2020.rda')
# load ILDPII screen data:
load('/Users/alebedev/Documents/Projects/ILDPII/ILDPII_screen.rda')
# load ILDPII new follow-up data:
load('/Users/alebedev/Documents/Projects/ILDPII/NFU.rda')
# load ILDPII *scanned* data:
load('/Users/alebedev/Documents/Projects/ILDPII/PSCAN.rda')

# Screening:
ALLSCR <- rbind(ILDPII_screen[,colnames(ILDPII_screen)[is.element(colnames(ILDPII_screen),colnames(SCREEN_df))]],
                SCREEN_df[,colnames(ILDPII_screen)[is.element(colnames(ILDPII_screen),colnames(SCREEN_df))]])
set.seed(123)
ALLSCR$email[ALLSCR$email=='blankt@blankt.com'] <- paste0(ALLSCR$email[ALLSCR$email=='blankt@blankt.com'],
                                                          sample(1:length(ALLSCR$email[ALLSCR$email=='blankt@blankt.com'])))
ALLSCR$email[ALLSCR$email=='blank@blank.com'] <- paste0(ALLSCR$email[ALLSCR$email=='blank@blank.com'],
                                                          sample(1:length(ALLSCR$email[ALLSCR$email=='blank@blank.com'])))
ALLSCR$email[ALLSCR$email=='blankt@blankt.se'] <- paste0(ALLSCR$email[ALLSCR$email=='blankt@blankt.se'],
                                                        sample(1:length(ALLSCR$email[ALLSCR$email=='blankt@blankt.se'])))
ALLSCR <- ALLSCR[!duplicated(ALLSCR$email, fromLast=T),]
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
ALLSCR$sex[ALLSCR$sex==3] <- NA
ALLSCR$sex <- as.factor(as.vector(ALLSCR$sex))
ALLSCR$email[ALLSCR$email=='malina.hansen@gmail.con'] <- 'malina.hansen@gmail.com'

# All extended drug use + Big-5:
ALLEDU <- rbind(NFU[,colnames(NFU)[is.element(colnames(NFU),colnames(SCRFU_df))]],
                SCRFU_df[,colnames(NFU)[is.element(colnames(NFU),colnames(SCRFU_df))]])
ALLEDU$ALC_freqprox <- apply(scale(ALLEDU[,c('ALC_freq', 'ALC_prox')]),1,mean)
ALLEDU$TOB_freqprox <- apply(scale(ALLEDU[,c('TOB_freq', 'TOB_prox')]),1,mean)
ALLEDU$CAN_freqprox <- apply(scale(ALLEDU[,c('CAN_freq', 'CAN_prox')]),1,mean)
ALLEDU$MDMA_freqprox <- apply(scale(ALLEDU[,c('MDMA_freq', 'MDMA_prox')]),1,mean)
ALLEDU$STIM_freqprox <- apply(scale(ALLEDU[,c('STIM_freq', 'STIM_prox')]),1,mean)
ALLEDU$OPI_freqprox <- apply(scale(ALLEDU[,c('OPI_freq', 'OPI_prox')]),1,mean)
ALLEDU$PSY_freqprox <- apply(scale(ALLEDU[,c('PSY_freq', 'PSY_prox')]),1,mean)
# All conspiracy mentality:
ALLCMQ <- rbind(NFU[,colnames(NFU)[is.element(colnames(NFU),colnames(CONSP_df))]],
                CONSP_df[,colnames(NFU)[is.element(colnames(NFU),colnames(CONSP_df))]])
ALLCMQ$ALC_freqprox <- apply(scale(ALLCMQ[,c('ALC_freq', 'ALC_prox')]),1,mean)
ALLCMQ$TOB_freqprox <- apply(scale(ALLCMQ[,c('TOB_freq', 'TOB_prox')]),1,mean)
ALLCMQ$CAN_freqprox <- apply(scale(ALLCMQ[,c('CAN_freq', 'CAN_prox')]),1,mean)
ALLCMQ$MDMA_freqprox <- apply(scale(ALLCMQ[,c('MDMA_freq', 'MDMA_prox')]),1,mean)
ALLCMQ$STIM_freqprox <- apply(scale(ALLCMQ[,c('STIM_freq', 'STIM_prox')]),1,mean)
ALLCMQ$OPI_freqprox <- apply(scale(ALLCMQ[,c('OPI_freq', 'OPI_prox')]),1,mean)
ALLCMQ$PSY_freqprox <- apply(scale(ALLCMQ[,c('PSY_freq', 'PSY_prox')]),1,mean)


ALLSCR <- ALLSCR[!duplicated(ALLSCR$email),]
ALLCMQ <- ALLCMQ[!duplicated(ALLCMQ$email),]
ALLEDU <- ALLEDU[!duplicated(ALLEDU$email),]

ALLEDU_wDemogr <- merge(ALLEDU, ALLSCR[c('email',colnames(ALLSCR)[!is.element(colnames(ALLSCR),colnames(ALLEDU))])], by='email')
ALLCMQ_wDemogr <- merge(ALLCMQ, ALLSCR[c('email',colnames(ALLSCR)[!is.element(colnames(ALLSCR),colnames(ALLCMQ))])], by='email')
PSCAN_wDemogr <- merge(PSCAN, ALLSCR[c('email',colnames(ALLSCR)[!is.element(colnames(ALLSCR),colnames(PSCAN))])], by='email')

# Andres and Otilia:
extraEBS <- subset(PSCAN_wDemogr,is.element(PSCAN_wDemogr$email,NFU$email)==F)
ALLEBS <- rbind(NFU[,colnames(NFU)[is.element(colnames(NFU),colnames(extraEBS))]],
                        extraEBS[,colnames(NFU)[is.element(colnames(NFU),colnames(extraEBS))]])

ALLEBS_wDemogr <- merge(ALLEBS, ALLSCR[c('email',colnames(ALLSCR)[!is.element(colnames(ALLSCR),colnames(ALLEBS))])], by='email')
save('ALLSCR','ALLEDU','ALLCMQ','ALLEBS','ALLEBS_wDemogr', 'ALLEDU_wDemogr','ALLCMQ_wDemogr', 'HUDMAIN_df', 'PSCAN_wDemogr',file='/Users/alebedev/Documents/Projects/HUD/HUD_final_mergedMarch2020.rda')


# Anonymize
ALLSCR <- ALLSCR[, !names(ALLSCR) %in% c('email')]
ALLEDU_wDemogr <- ALLEDU_wDemogr[, !names(ALLEDU_wDemogr) %in% c('email')] 
ALLCMQ_wDemogr <- ALLCMQ_wDemogr[, !names(ALLCMQ_wDemogr) %in% c('email')] 
ALLEBS_wDemogr <- ALLEBS_wDemogr[, !names(ALLEBS_wDemogr) %in% c('email')] 
save('ALLEDU_wDemogr','ALLCMQ_wDemogr', 'ALLEBS','ALLEBS_wDemogr',file='/Users/alebedev/Documents/Projects/ILDPII/AndresOtilia_anonymized.rda')



# COVID_19:
cvp <- read.xlsx2(paste('~/Downloads/cvp.xlsx',sep=''),2, stringsAsFactors=F)
# only to those who hasn't responded
cvp$VAR1 <- tolower(cvp$VAR1)
cvp$VAR1 <- gsub(",",".",cvp$VAR1)
cvp$VAR1 <- gsub(";",".",cvp$VAR1)
cvp$VAR1 <- gsub(" ","",cvp$VAR1)

ALLSCR_sel <- subset(ALLSCR, is.element(ALLSCR$email,cvp$VAR1)==F)

fu_list <- ALLSCR_sel[,c('email', 'age','sex', 'mothertongue')]
write.csv2(fu_list, file = paste0('~/Downloads/covid19FUlist_', Sys.Date(),'.csv'))




# All conspiracy mentality:
summary(glm(CONS5~ALC_freq+TOB_freq+
              CAN_freq+MDMA_freq+STIM_freq+OPI_freq+PSY_freq, data=ALLCMQ))

tmp <- merge(ALLCMQ, ALLSCR, by='email')
summary(glm(DP~ALC_freq+age+sex+TOB_freq+
              CAN_freq+MDMA_freq+STIM_freq+OPI_freq+PSY_freq+PsychDiagAny+surveyfoundout, data=tmp))


summary(glm(EBS_evid~ALC_freq+TOB_freq+
              CAN_freq+MDMA_freq+STIM_freq+OPI_freq+PSY_freq, data=NFU))




# Prox:
summary(glm(CONS5~TOB_prox+ALC_prox+
              CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox+O+C+E+A+N, data=ALLCMQ))
summary(glm(EBS_evid~TOB_prox+ALC_prox+
              CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox+O+C+E+A+N, data=NFU))
tmp <- merge(NFU, ALLSCR, by='email')
summary(glm(CONS5~age+sex+TOB_prox+ALC_prox+
              CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox+O+C+E+A+N, data=tmp))

# Freq
summary(glm(CONS5~TOB_freq+ALC_freq+
              CAN_freq+MDMA_freq+STIM_freq+OPI_freq+PSY_freq+O+C+E+A+N, data=ALLCMQ))
summary(glm(EBS_evid~TOB_freq+ALC_freq+
              CAN_freq+MDMA_freq+STIM_freq+OPI_freq+PSY_freq+O+C+E+A+N, data=NFU))
tmp <- merge(NFU, ALLSCR, by='email')
summary(glm(CONS5~age+sex+TOB_freq+ALC_freq+
              CAN_freq+MDMA_freq+STIM_freq+OPI_freq+PSY_freq+O+C+E+A+N, data=tmp))


tmp <- merge(fdata_df, SCREEN_df, by='email', all=T)
tmp1 <- CONSP_df[,c("email","CONS_public" , "CONS_polit"  , "CONS_monit", 
                   "CONS_connect", "CONS_org",  
                   "ALC_freq"    , 
                   "ALC_prox",  "TOB_freq", "TOB_prox"    , "MDMA_freq",
                   "MDMA_prox"  , "CAN_freq"    , "CAN_prox"    , 
                   "STIM_freq", "STIM_prox", "OPI_freq"    , "OPI_prox",
                   "PSY_freq"    , "PSY_prox"    , "CONS5")]

tmp1$ALC_age <- NA
tmp1$TOB_age <- NA
tmp1$MDMA_age <- NA
tmp1$TOB_age <- NA
tmp1$CAN_age <- NA
tmp1$STIM_age <- NA
tmp1$OPI_age <- NA
tmp1$PSY_age <- NA
tmp1$EBS_feel <- NA
tmp1$EBS_evid <- NA
tmp1$EBS_polit <- NA

tmp2 <- tmp[,c("email","CONS_public" , "CONS_polit"  , "CONS_monit", 
               "CONS_connect", "CONS_org",  
               "ALC_freq"    , 
               "ALC_prox",  "TOB_freq", "TOB_prox"    , "MDMA_freq",
               "MDMA_prox"  , "CAN_freq"    , "CAN_prox"    , 
               "STIM_freq", "STIM_prox", "OPI_freq"    , "OPI_prox",
               "PSY_freq"    , "PSY_prox"    , "CONS5",
               "ALC_age", "TOB_age", "MDMA_age", "CAN_age", "STIM_age", "OPI_age", "PSY_age",
               "EBS_feel", "EBS_evid","EBS_polit")]
tmp2 <- tmp2[complete.cases(tmp2),]
DAF <- rbind(tmp1,tmp2)

scr1 <- subset(SCRFU_df, is.element(SCRFU_df$email, setdiff(SCRFU_df$email, SCREEN_df$email)))
ILDPII_screen$nonbingen <- 'no'
ILDPII_screen$nonbingen[ILDPII_screen$sex==3] <- 'yes'
allscr <- rbind(scr1[,c('email', 'sex', 'age', 'nonbingen', 'mothertongue','PDI_total','PDI_dist', 'PDI_time', 'PDI_conv', 'education', 'diagMDep','diagBP','diagScz','diagADHD','diagASD','diagOCD','diagOther',
                        'ASRS', 'OLIFE_tot', 
                        'OLIFE_UE','OLIFE_CD','OLIFE_IA','OLIFE_IN',
                        'raads_young','raads_now','raads_both', 'raads_any')],
                SCREEN_df[,c('email', 'sex', 'age', 'nonbingen', 'mothertongue','PDI_total','PDI_dist', 'PDI_time', 'PDI_conv', 'education', 'diagMDep','diagBP','diagScz','diagADHD','diagASD','diagOCD','diagOther',
                             'ASRS', 'OLIFE_tot', 
                             'OLIFE_UE','OLIFE_CD','OLIFE_IA','OLIFE_IN',
                             'raads_young','raads_now','raads_both', 'raads_any')],
                ILDPII_screen[,c('email', 'sex', 'age', 'nonbingen', 'mothertongue','PDI_total','PDI_dist', 'PDI_time', 'PDI_conv', 'education', 'diagMDep','diagBP','diagScz','diagADHD','diagASD','diagOCD','diagOther',
                                 'ASRS', 'OLIFE_tot', 
                                 'OLIFE_UE','OLIFE_CD','OLIFE_IA','OLIFE_IN',
                                 'raads_young','raads_now','raads_both', 'raads_any')]
)
rownames(allscr) <- c()
allscr <- allscr[!duplicated(allscr$email, fromLast=T),]

#allscr$email[allscr$email=="supermegaduperkurt@gmail.com"] <- "supermegaduperkurt@gmail.com"
ddd <- merge(DAF, allscr, by='email')

ddd$email <- sample(1:nrow(ddd))

save('DAF', file=paste0('~/Downloads/DAF_', Sys.Date(),'.rda'))
write.csv2(ddd, file = paste0('~/Downloads/DAF_', Sys.Date(),'.csv'))

ALLCMQ_wDemogr, ALLEDU_wDemogr, NFU, NFU_wDemogr  

ddd <- merge(DAF, allscr[,c()])






