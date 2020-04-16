# HUD: follow-up questionnaire:
library(stringi)
library(xlsx)
# Study ID-key:
esids <- read.xlsx2('/Users/alebedev/Documents/Projects/HUD/visits/tested.xlsx', 1, stringsAsFactors=F)
esids$email <- gsub(" ","",tolower(esids$email))
esids$email <- gsub(";","",tolower(esids$email))


# Follow-up data:
dtypes <- c('NP', 'PP', 'NP2', 'PP2')
for (n in 1:length(dtypes)){
  dtype<-dtypes[n]
  fdata <- read.xlsx2(paste('~/Downloads/',dtype, '.xlsx',sep=''),2, stringsAsFactors=F)

  if (dtype == 'PP'){
    fdata_df <- data.frame(email=fdata$VAR1, participation=fdata$VAR4)
    fdata$VAR2_8[1:57] <- NA
  } else if (dtype == 'NP') {
    fdata_df <- data.frame(email=fdata$VAR1, participation=fdata$VAR3)
    fdata$VAR2_8[1:47] <- NA
  } else if (dtype == 'PP2') {
    fdata_df <- data.frame(email=fdata$VAR1, participation=fdata$VAR4)
  } else if (dtype == 'NP2') {
    fdata_df <- data.frame(email=fdata$VAR1, participation=fdata$VAR4)
  }
  
  for (i in 1:length(fdata_df$email)){
    # Select ith row:
    tmp <- subset(fdata[i,])
    fdata_df$O[i] <- mean(as.numeric(tmp[,paste('VAR2_',c(7,8,9),sep='')]), na.rm=T)
    fdata_df$C[i] <- mean(as.numeric(tmp[,paste('VAR2_',c(13,15),sep='')]), 7-as.numeric(tmp[,paste('VAR2_',c(14),sep='')]))  
    fdata_df$E[i] <- mean(as.numeric(tmp[,paste('VAR2_',c(4,5),sep='')]), 7-as.numeric(tmp[,paste('VAR2_',c(6),sep='')]))  
    fdata_df$A[i] <- mean(as.numeric(tmp[,paste('VAR2_',c(11,12),sep='')]), 7-as.numeric(tmp[,paste('VAR2_',c(10),sep='')]))  
    fdata_df$N[i] <- mean(as.numeric(tmp[,paste('VAR2_',c(1,2),sep='')]), 7-as.numeric(tmp[,paste('VAR2_',c(3),sep='')]))  
    if (dtype == 'PP'){
      fdata_df$psyExEmo[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(1:3,5),sep='')]), na.rm=T)
      fdata_df$psySocial[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(6),sep='')]), na.rm=T)
      fdata_df$psyCritTh[i] <- mean(c(as.numeric(tmp[,paste('VAR3_',c(7),sep='')]), (-1)*as.numeric(tmp[,paste('VAR3_',c(10,21:24),sep='')])))
      fdata_df$psyCog[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(4, 8),sep='')]), na.rm=T)
      fdata_df$psyAnxDep[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(9,15),sep='')])*(-1), na.rm=T)
      fdata_df$psySelf[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(11,13:14),sep='')]), na.rm=T)
      fdata_df$psyPerc[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(16:17),sep='')]), na.rm=T)
      fdata_df$psySom[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(12,18),sep='')]), na.rm=T)
      fdata_df$psyAddict[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(19),sep='')]), na.rm=T)
      fdata_df$psySpi[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(20),sep='')]), na.rm=T)
      fdata_df$psyPolit[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(25),sep='')]), na.rm=T)
    } else if (dtype == 'PP2') {
      fdata_df$psyExEmo[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(1:3,5),sep='')]), na.rm=T)
      fdata_df$psySocial[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(6),sep='')]), na.rm=T)
      fdata_df$psyCritTh[i] <- mean(c(as.numeric(tmp[,paste('VAR3_',c(7),sep='')]), (-1)*as.numeric(tmp[,paste('VAR3_',c(10,21:24),sep='')])))
      fdata_df$psyCog[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(4, 8),sep='')]), na.rm=T)
      fdata_df$psyAnxDep[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(9,15),sep='')])*(-1), na.rm=T)
      fdata_df$psySelf[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(11,13:14),sep='')]), na.rm=T)
      fdata_df$psyPerc[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(16:17),sep='')]), na.rm=T)
      fdata_df$psySom[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(12,18),sep='')]), na.rm=T)
      fdata_df$psyAddict[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(19),sep='')]), na.rm=T)
      fdata_df$psySpi[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(20),sep='')]), na.rm=T)
      fdata_df$psyPolit[i] <- mean(as.numeric(tmp[,paste('VAR3_',c(25),sep='')]), na.rm=T)
      
      fdata_df$CONS_public[i] = as.numeric(tmp$AF_1)
      fdata_df$CONS_polit[i]= as.numeric(tmp$AF_2)
      fdata_df$CONS_monit[i]= as.numeric(tmp$AF_3)
      fdata_df$CONS_connect[i]= as.numeric(tmp$AF_4)
      fdata_df$CONS_org[i]= as.numeric(tmp$AF_5)
      fdata_df$CONS_glob[i]= as.numeric(tmp$AF_6)
      fdata_df$CONS_prog[i]= as.numeric(tmp$AF_7)
      fdata_df$CONS_decisions[i]=100- as.numeric(tmp$AF_8)
      fdata_df$CONS_diseases[i]= as.numeric(tmp$AF_9)
      fdata_df$CONS_health[i]= as.numeric(tmp$AF_10)
      fdata_df$CONS_gov[i]= as.numeric(tmp$AF_11)
      
      fdata_df$ALC_freq[i]= as.numeric(tmp$ALC1)
      fdata_df$ALC_prox[i]= as.numeric(tmp$ALC2)
      fdata_df$TOB_freq[i]= as.numeric(tmp$TOB1)
      fdata_df$TOB_prox[i]= as.numeric(tmp$TOB2)
      fdata_df$MDMA_freq[i]= as.numeric(tmp$MDMA1)
      fdata_df$MDMA_prox[i]= as.numeric(tmp$MDMA2)
      fdata_df$CAN_freq[i]= as.numeric(tmp$CAN1)
      fdata_df$CAN_prox[i]= as.numeric(tmp$CAN2)
      fdata_df$STIM_freq[i]= as.numeric(tmp$STIM1)
      fdata_df$STIM_prox[i]= as.numeric(tmp$STIM2)
      fdata_df$OPI_freq[i]= as.numeric(tmp$OPI1)
      fdata_df$OPI_prox[i]= as.numeric(tmp$OPI2)
      fdata_df$PSY_freq[i]= as.numeric(tmp$PSY1)
      fdata_df$PSY_prox[i]= as.numeric(tmp$PSY2)
      fdata_df$DESPSY[i] <- as.numeric(tmp$DP1)
      fdata_df$DESPSY_exp[i] <- as.numeric(tmp$DP2)
    } else if (dtype == 'NP2') {
      fdata_df$CONS_public[i] = as.numeric(tmp$AF_1)
      fdata_df$CONS_polit[i]= as.numeric(tmp$AF_2)
      fdata_df$CONS_monit[i]= as.numeric(tmp$AF_3)
      fdata_df$CONS_connect[i]= as.numeric(tmp$AF_4)
      fdata_df$CONS_org[i]= as.numeric(tmp$AF_5)
      fdata_df$CONS_glob[i]= as.numeric(tmp$AF_6)
      fdata_df$CONS_prog[i]= as.numeric(tmp$AF_7)
      fdata_df$CONS_decisions[i]=100- as.numeric(tmp$AF_8)
      fdata_df$CONS_diseases[i]= as.numeric(tmp$AF_9)
      fdata_df$CONS_health[i]= as.numeric(tmp$AF_10)
      fdata_df$CONS_gov[i]= as.numeric(tmp$AF_11)
      
      fdata_df$ALC_freq[i]= as.numeric(tmp$ALC1)
      fdata_df$ALC_prox[i]= as.numeric(tmp$ALC2)
      fdata_df$TOB_freq[i]= as.numeric(tmp$TOB1)
      fdata_df$TOB_prox[i]= as.numeric(tmp$TOB2)
      fdata_df$MDMA_freq[i]= as.numeric(tmp$MDMA1)
      fdata_df$MDMA_prox[i]= as.numeric(tmp$MDMA2)
      fdata_df$CAN_freq[i]= as.numeric(tmp$CAN1)
      fdata_df$CAN_prox[i]= as.numeric(tmp$CAN2)
      fdata_df$STIM_freq[i]= as.numeric(tmp$STIM1)
      fdata_df$STIM_prox[i]= as.numeric(tmp$STIM2)
      fdata_df$OPI_freq[i]= as.numeric(tmp$OPI1)
      fdata_df$OPI_prox[i]= as.numeric(tmp$OPI2)
      fdata_df$PSY_freq[i]= as.numeric(tmp$PSY1)
      fdata_df$PSY_prox[i]= as.numeric(tmp$PSY2)
    }
  }
  
  if (dtype == 'PP'){
    fdata_df_PP <- fdata_df
    fdata_df_PP$group <- 'PP'
  } else if (dtype == 'NP') {
    fdata_df_NP <- fdata_df
    fdata_df_NP$group <- 'NP'
  } else if (dtype == 'PP2') {
    fdata_df_PP2 <- fdata_df
    fdata_df_PP2$group <- 'PP'
  } else if (dtype == 'NP2') {
    fdata_df_NP2 <- fdata_df
    fdata_df_NP2$group <- 'NP'
  }
}





# Experimental Visit:
ES <- read.xlsx2('~/Downloads/EV.xlsx', 2, stringsAsFactors=F)
es_df <- data.frame(ID=ES$VAR00, CONS_public = as.numeric(ES$VAR01_1), CONS_polit=as.numeric(ES$VAR01_2),
                    CONS_monit=as.numeric(ES$VAR01_3),CONS_connect=as.numeric(ES$VAR01_4),
                    CONS_org=as.numeric(ES$VAR01_5), CONS_glob=as.numeric(ES$VAR01_6),
                    CONS_prog=as.numeric(ES$VAR01_7),CONS_decisions=100-as.numeric(ES$VAR01_8),
                    CONS_diseases=as.numeric(ES$VAR01_9),CONS_health=as.numeric(ES$VAR01_10),
                    CONS_gov=as.numeric(ES$VAR01_11),
                    
                    ALC_freq=as.numeric(ES$VAR02_1), ALC_prox=as.numeric(ES$VAR03_1),
                    TOB_freq=as.numeric(ES$VAR04_1), TOB_prox=as.numeric(ES$VAR05_1),
                    MDMA_freq=as.numeric(ES$VAR06_1), MDMA_prox=as.numeric(ES$VAR07_1),
                    CAN_freq=as.numeric(ES$VAR08_1), CAN_prox=as.numeric(ES$VAR09_1),
                    STIM_freq=as.numeric(ES$VAR10_1), STIM_prox=as.numeric(ES$VAR11_1),
                    OPI_freq=as.numeric(ES$VAR12_1), OPI_prox=as.numeric(ES$VAR13_1),
                    PSY_freq=as.numeric(ES$VAR14_1), PSY_prox=as.numeric(ES$VAR15_1),
                    
                    
                    STAI = ((5-apply(apply(ES[,paste('VAR16_', c(1,2,5,8,10,11,15,16,19,20), sep='')],1, as.numeric),2,mean))+
                    apply(apply(ES[,paste('VAR16_', c(3,4,6,7,9,12,13,14,17,18), sep='')],1, as.numeric),2,mean))/2
                    
)
es_df <- es_df[es_df$ID!='gg',]

fdata_df <- rbind(fdata_df_NP[,c('email','group', 'participation', 'O', 'C', 'E', 'A', 'N')],
                  fdata_df_PP[,c('email','group','participation', 'O', 'C', 'E', 'A', 'N')],
                  fdata_df_NP2[,c('email','group', 'participation', 'O', 'C', 'E', 'A', 'N')],
                  fdata_df_PP2[,c('email','group','participation', 'O', 'C', 'E', 'A', 'N')])

fdata_df$email <- gsub(" ","",fdata_df$email)
fdata_df$email <- gsub(",",".",fdata_df$email)
fdata_df$email <- gsub(";",".",fdata_df$email)
fdata_df$email <- tolower(fdata_df$email)
fdata_df$email[fdata_df$email=="supermegaduperkurt@icloud.com"] <- "supermegaduperkurt@gmail.com"

# Preprocess text:
df <- merge(fdata_df, screen_df, by = 'email')
ages <- df$age
df$age <- as.numeric(as.vector(df$age))
ages[which(is.na(df$age))]
df$age[which(is.na(df$age))] <- c(26,24,19)
df$mothertongue <- gsub(" ","",tolower(df$mothertongue))
df$mothertongue[is.element(df$mothertongue, c('swedish', 'estniska,svenska', 'venska(menbättrepåengelska)', 'swedish,russian', 
                                              'svenska,spanska,ryska,ukrainska', 'sve', 'albanska/svenska', 'russian,swedish',
                                              'scenska', 'svensa', 'svenska,arabiska', 'svenska,serbiska', 'svenska/spanska',
                                              'svenska/engelska', 'svenska+finska', 'svenskaochbengaliska','svenskaochturkiska',
                                              'svenska(menbättrepåengelska)',
                                              'svenskaochpolska','bengaliochsvenska'))] <- 'svenska'
df$somaticDS <- gsub(" ","",tolower(df$somaticDS))
df$somaticDS[is.element(df$somaticDS, c('no', 'nej.', 'no.', "", 'utmattningssyndrom','anxiety,panicattacks',
                                        'vetintevaddetbetyder', 'migrän', 'intevadjagvet','pollenallergi','vetintevaddetbetyder'))] <- 'nej'
df$sex <- gsub(" ","",tolower(df$sex))
df$sex[is.element(df$sex, c('femail',  "",'kvinna(intergender)', 'female', 'f', 'woman', 'kvinns'))] <- 'kvinna'
df$sex[is.element(df$sex, c('male',  "mam", 'man(biologiskt,menkanskeodefinieratmentalt)'))] <- 'man'
df$medsWhich <- gsub(" ","",tolower(df$medsWhich))
df$medsY1[is.element(df$medsWhich, c('oralcontraceptivepills.prionelle.',  "omeprazol,yaz",
                                     'Neovletta p-piller', 'omaprozol',
                                     'birthcontrolpill(maxim,everypillcontains0,030mgethinylestradioland2mgdienogest)',
                                     'birthcontrolpills(tomanagehormonalimbalancescausedbypolycysticovarysyndrome)',
                                     'birthcontrolpill',
                                     'ebastin20mg/dagunderpollensäsongen.', 'cbd-olja',
                                     'cannabis'))] <- 2
df$diagOtherWhich <- gsub(" ","",tolower(df$diagOtherWhich))
df$diagOther[is.element(df$diagOtherWhich, c('utmattningssyndrom,underutredningstarkmistankehfa', 
                                             'utmattningssyndrom','schizotypal', 'hsp', 'emotionelltinstabilpersonlighetsstörning',
                                             'gad', 'ejdiagnosticerad.', 'ångest', '-', ' ', '',
                                             'utmattningsdepression','utmattning', 'nej', 'gadpanikångest',
                                             'dystoni', 'depressionochutmattningssyndrom', 
                                             'anankastiskpersonlighetsstörning,ångest&panikattacker',
                                             'annanspecificeradpersonlighetsstörning,socialfobi'))] <- 2


df_all <- merge(es_df, esids[,c('ID','email', 'ShockLevel')],by='ID')
df_all$ShockLevel <- as.numeric(df_all$ShockLevel)
df_all <- merge(df_all, df, by='email')
df_all <- df_all[!duplicated(df_all$email,fromLast = T),]

df_allf <- merge(df_all, fdata_df_PP[,c('email','psyExEmo', 'psySocial','psyCritTh', 'psyCog', 'psyAnxDep','psySelf',
                                       'psyPerc', 'psySom', 'psyAddict', 'psySpi', 'psyPolit')], replace=T)

fdata2_df <- rbind(fdata_df_NP2[,c('email','group', 'participation', 'O', 'C', 'E', 'A', 'N',
                                   'CONS_public','CONS_polit', 'CONS_monit', 'CONS_connect', 'CONS_org','CONS_glob', 
                                   'CONS_prog' ,'CONS_decisions', 'CONS_diseases', 'CONS_health','CONS_gov','ALC_freq',
                                   'ALC_prox','TOB_freq' ,'TOB_prox','MDMA_freq', 'MDMA_prox', 'CAN_freq','CAN_prox','STIM_freq',
                                   'STIM_prox', 'OPI_freq','OPI_prox','PSY_freq','PSY_prox')],
                   fdata_df_PP2[,c('email','group', 'participation', 'O', 'C', 'E', 'A', 'N',
                                   'CONS_public','CONS_polit', 'CONS_monit', 'CONS_connect', 'CONS_org','CONS_glob', 
                                   'CONS_prog' ,'CONS_decisions', 'CONS_diseases', 'CONS_health','CONS_gov','ALC_freq',
                                   'ALC_prox','TOB_freq' ,'TOB_prox','MDMA_freq', 'MDMA_prox', 'CAN_freq','CAN_prox','STIM_freq',
                                   'STIM_prox', 'OPI_freq','OPI_prox','PSY_freq','PSY_prox')],
                   df_all[,c('email','group', 'participation', 'O', 'C', 'E', 'A', 'N',
                                   'CONS_public','CONS_polit', 'CONS_monit', 'CONS_connect', 'CONS_org','CONS_glob', 
                                   'CONS_prog' ,'CONS_decisions', 'CONS_diseases', 'CONS_health','CONS_gov','ALC_freq',
                                   'ALC_prox','TOB_freq' ,'TOB_prox','MDMA_freq', 'MDMA_prox', 'CAN_freq','CAN_prox','STIM_freq',
                                   'STIM_prox', 'OPI_freq','OPI_prox','PSY_freq','PSY_prox')])

fdata2_df$CONS5 <- apply(fdata2_df[,c('CONS_public', 'CONS_polit', 'CONS_monit', 'CONS_connect', 'CONS_org')],1,mean)
fdata2_df$CONSALL <- apply(fdata2_df[,c('CONS_public', 'CONS_polit', 'CONS_monit', 'CONS_connect', 'CONS_org',
                                        'CONS_glob','CONS_prog','CONS_diseases', 'CONS_health', 'CONS_gov', 'CONS_decisions')],1,mean)



SCREEN_df <- screen_df
# Apply transforms:
SCREEN_df$PDI_totalLog <- log1p(SCREEN_df$PDI_total)
SCREEN_df$PDI_distLog <- log1p(SCREEN_df$PDI_dist)
SCREEN_df$PDI_convLog <- log1p(SCREEN_df$PDI_conv)
SCREEN_df$PDI_timeLog <- log1p(SCREEN_df$PDI_time)
SCREEN_df$OLIFE_totLog <- log1p(SCREEN_df$OLIFE_tot)
SCREEN_df$OLIFE_UELog <- log1p(SCREEN_df$OLIFE_UE)
SCREEN_df$OLIFE_CDLog <- log1p(SCREEN_df$OLIFE_CD)
SCREEN_df$OLIFE_IALog <- log1p(SCREEN_df$OLIFE_IA)
SCREEN_df$OLIFE_INLog <- log1p(SCREEN_df$OLIFE_IN)
SCREEN_df$raads_anyLog <- log1p(SCREEN_df$raads_any)
SCREEN_df$ASRSLog <- log1p(SCREEN_df$ASRS)

SCREEN_df$DP <- apply(scale(SCREEN_df[,c('PDI_totalLog', 'OLIFE_totLog')]),1,mean)


SCRFU_df <- df
CONSP_df <- fdata2_df
CONSP_df$ALC_prox[CONSP_df$ALC_prox==1] <- 9
#CONSP_df$ALC_prox <- max(CONSP_df$ALC_prox)-(CONSP_df$ALC_prox)
CONSP_df$TOB_prox[CONSP_df$TOB_prox==1] <- 9
#CONSP_df$TOB_prox <- max(CONSP_df$TOB_prox)-(CONSP_df$TOB_prox)
CONSP_df$CAN_prox[CONSP_df$CAN_prox==1] <- 9
#CONSP_df$CAN_prox <- max(CONSP_df$CAN_prox)-(CONSP_df$CAN_prox)
CONSP_df$MDMA_prox[CONSP_df$MDMA_prox==1] <- 9
#CONSP_df$MDMA_prox <- max(CONSP_df$MDMA_prox)-(CONSP_df$MDMA_prox)
CONSP_df$STIM_prox[CONSP_df$STIM_prox==1] <- 9
#CONSP_df$STIM_prox <- max(CONSP_df$STIM_prox)-(CONSP_df$STIM_prox)
CONSP_df$OPI_prox[CONSP_df$OPI_prox==1] <- 9
#CONSP_df$OPI_prox <- max(CONSP_df$OPI_prox)-(CONSP_df$OPI_prox)
CONSP_df$PSY_prox[CONSP_df$PSY_prox==1] <- 9
#CONSP_df$PSY_prox <- max(CONSP_df$PSY_prox)-(CONSP_df$PSY_prox)


SCREEN_df$email[SCREEN_df$email=='supermegaduperkurt@icloud.com'] <- 'supermegaduperkurt@gmail.com'


SCREEN_df$diagMDep[is.na(SCREEN_df$diagMDep)]<-2
SCREEN_df$diagBP[is.na(SCREEN_df$diagBP)]<-2
SCREEN_df$diagScz[is.na(SCREEN_df$diagScz)]<-2
SCREEN_df$diagADHD[is.na(SCREEN_df$diagADHD)]<-2
SCREEN_df$diagASD[is.na(SCREEN_df$diagASD)]<-2
SCREEN_df$diagOCD[is.na(SCREEN_df$diagOCD)]<-2
SCREEN_df$diagOther[is.na(SCREEN_df$diagOther)]<-2
SCREEN_df$brainInjuryY1[is.na(SCREEN_df$brainInjuryY1)]<-2


CONSP_df$email <- gsub(" ","",tolower(CONSP_df$email))
CONSP_df <- CONSP_df[!duplicated(CONSP_df$email, fromLast = T),]
tmp1 <- merge(CONSP_df, SCREEN_df[,c('email','sex', 'age', 'diagMDep', 'diagBP', 'diagScz', 'diagADHD', 'diagASD','diagOCD',
                                    'diagOther', 'brainInjuryY1', 'DP', 'OLIFE_totLog', 'PDI_totalLog', 'SEPI_tot','SEPI_tot_drug')], by.x ='email', all=F)



tmp2 <- CONSP_df[is.element(CONSP_df$email,setdiff(CONSP_df$email, tmp1$email)),]

tmp2[,c('sex', 'age', 'diagMDep', 'diagBP', 'diagScz', 'diagADHD', 'diagASD','diagOCD',
        'diagOther', 'brainInjuryY1', 'DP', 'OLIFE_totLog', 'PDI_totalLog','SEPI_tot','SEPI_tot_drug')] <- NA


CONSP_df <- rbind(tmp1, tmp2)
CONSP_df <- CONSP_df[!duplicated(CONSP_df$email),]
CONSP_df$sex <- as.factor(CONSP_df$sex)





CONSP_df[,c('ALC_prox', 'TOB_prox', 'CAN_prox','MDMA_prox','STIM_prox', 'OPI_prox', 'PSY_prox')] <- 
  10-CONSP_df[,c('ALC_prox', 'TOB_prox', 'CAN_prox','MDMA_prox','STIM_prox', 'OPI_prox', 'PSY_prox')]

CONSP_df$ALC_freqprox <- apply(scale(CONSP_df[,c('ALC_freq', 'ALC_prox')]),1,mean)
CONSP_df$TOB_freqprox <- apply(scale(CONSP_df[,c('TOB_freq', 'TOB_prox')]),1,mean)
CONSP_df$CAN_freqprox <- apply(scale(CONSP_df[,c('CAN_freq', 'CAN_prox')]),1,mean)
CONSP_df$MDMA_freqprox <- apply(scale(CONSP_df[,c('MDMA_freq', 'MDMA_prox')]),1,mean)
CONSP_df$STIM_freqprox <- apply(scale(CONSP_df[,c('STIM_freq', 'STIM_prox')]),1,mean)
CONSP_df$OPI_freqprox <- apply(scale(CONSP_df[,c('OPI_freq', 'OPI_prox')]),1,mean)
CONSP_df$PSY_freqprox <- apply(scale(CONSP_df[,c('PSY_freq', 'PSY_prox')]),1,mean)


save('SCREEN_df','SCRFU_df','CONSP_df' ,file='/Users/alebedev/Documents/Projects/HUD/HUD.rda')


#CONSP <- CONSP_df[,2:dim(CONSP_df)[2]] 
#set.seed(123)
#CONSP$scrID <- paste('scr-',sample(1:dim(CONSP_df)[1]), sep='')

#save('CONSP' ,file='/Users/alebedev/Documents/Projects/HUD/CONSP.rda')




# PLOTTING:
v='CONS5'
t.test(CONSP_df[CONSP_df$group=='NP',v],CONSP_df[CONSP_df$group=='PP',v])



library(reghelper)

beta(glm(CONS5~ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox, data=CONSP_df))
beta(glm(CONS5~ALC_freq+TOB_freq+CAN_freq+MDMA_freq+STIM_freq+OPI_freq+PSY_freq, data=CONSP_df))
beta(glm(CONS5~ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox+
           ALC_freq+TOB_freq+CAN_freq+MDMA_freq+STIM_freq+OPI_freq+PSY_freq, data=CONSP_df))

beta(glm(CONS5~ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox+age+sex+O+C+E+A+N, data=CONSP_df))


# Radar Plots:
library(radarchart)
dfp <- t(aggregate(fdata_df[,c('O','C', 'E','A', 'N')],by=list(fdata_df$group), FUN=mean))
dfp <- data.frame(labels = c('O','C', 'E','A', 'N'), NonUsers=round(as.numeric(dfp[2:6,1]),2),
                  Users=round(as.numeric(dfp[2:6,2]),2))
chartJSRadar(scores=dfp, labelSize=30, scaleStartValue=3, maxScale=5.5,
             main = paste('BIG 5 and Psychedelic Drug Use (', 'n1 = ', table(fdata_df$group)['NP'],', n2 = ',
                          table(fdata_df$group)['PP'],')',sep=''),
             height=700, colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)

v='O'
t.test(fdata_df[fdata_df$group=='PP',v],fdata_df[fdata_df$group=='NP',v])

dfp <- t(aggregate(fdata2_df[,c('CONS_public','CONS_polit', 'CONS_monit','CONS_connect', 'CONS_org')],by=list(fdata2_df$group), FUN=mean))
dfp <- data.frame(labels = c('Misinformation','Politics', 'Monitoring','Hidden Connections', 'Secret Organisations'), NonUsers=round(as.numeric(dfp[2:6,1]),2),
                  Users=round(as.numeric(dfp[2:6,2]),2))
chartJSRadar(scores=dfp, labelSize=20, scaleStartValue=15,height=700,
             main = paste('Consipracy Mentality and Psychedelic Drug Use (', 'n1 = ', table(fdata_df$group)['NP'],', n2 = ',
                          table(fdata_df$group)['PP'],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)
v='CONS_public'
t.test(fdata2_df[fdata2_df$group=='NP',v],fdata2_df[fdata2_df$group=='PP',v])


screen_df$group<-NA
screen_df$group[screen_df$drug_psychedelics==1] <- 'PP'
screen_df$group[screen_df$drug_psychedelics==0] <- 'NP'


# O-LIFE:
dfp <- t(aggregate(screen_df[,c('OLIFE_UE','OLIFE_CD','OLIFE_IA', 'OLIFE_IN')],by=list(screen_df$group), FUN=mean))
dfp <- data.frame(labels = c('Unusual Experiences','Cognitive Disorganization',
                             'Introvertive Anhedonia', 'Impulsive Nonconformity'),
                  NonUsers=round(as.numeric(dfp[2:5,1]),2),
                  Users=round(as.numeric(dfp[2:5,2]),2))
chartJSRadar(scores=dfp, labelSize=20, scaleStartValue=2.5,height=700,
             main = paste('Schizotypy (O-LIFE) and Psychedelic Drug Use (', 'n1 = ', table(screen_df$group)['NP'],', n2 = ',
                          table(screen_df$group)['PP'],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)

v='OLIFE_UE'
t.test(screen_df[screen_df$group=='NP',v],screen_df[screen_df$group=='PP',v])


# PDI:
screen_df[,c('PDI_conv','PDI_dist','PDI_time')][is.na(screen_df[,c('PDI_conv','PDI_dist','PDI_time')])] <- 0
dfp <- t(aggregate(screen_df[,c('PDI_conv','PDI_dist','PDI_time')],by=list(screen_df$group), FUN=mean))
dfp <- data.frame(labels = c('Conviction','Disturbance','Preoccupation'),
                  NonUsers=round(as.numeric(dfp[2:4,1]),2),
                  Users=round(as.numeric(dfp[2:4,2]),2))
chartJSRadar(scores=dfp, labelSize=20, scaleStartValue=1.6,height=700,
             main = paste('Delusion-proneness (PDI) and Psychedelic Drug Use (', 'n = ', dim(screen_df)[1],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)

v='PDI_conv'
t.test(screen_df[screen_df$group=='NP',v],screen_df[screen_df$group=='PP',v])


# SEPI (endo):
dfp <- t(aggregate(screen_df[,c('SEPI_iden','SEPI_demarc','SEPI_consist','SEPI_act','SEPI_vit','SEPI_body','SEPI_thought', 'SEPI_overcomp')],by=list(screen_df$group), FUN=mean))
dfp <- data.frame(labels = c('Identity','Demarcation','Consistency','Activity',
                             'Vitality', 'Body', 'Thinking Process', 'Grandeur'),
                  NonUsers=round(as.numeric(dfp[2:dim(dfp)[1],1]),2),
                  Users=round(as.numeric(dfp[2:dim(dfp)[1],2]),2))
chartJSRadar(scores=dfp, labelSize=20, height=700,scaleStartValue=0.1,
             main = paste('Ich-Störungen (EPI) and Psychedelic Drug Use (', 'n1 = ', table(screen_df$group)['NP'],', n2 = ',
                          table(screen_df$group)['PP'],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)

v='SEPI_demarc'
t.test(screen_df[screen_df$group=='NP',v],screen_df[screen_df$group=='PP',v])

# SEPI (drug):
dfp <- t(aggregate(screen_df[,c('SEPI_iden_drug','SEPI_demarc_drug','SEPI_consist_drug','SEPI_act_drug','SEPI_vit_drug','SEPI_body_drug','SEPI_thought_drug', 'SEPI_overcomp_drug')],by=list(screen_df$group), FUN=mean))
dfp <- data.frame(labels = c('Identity','Demarcation','Consistency','Activity',
                             'Vitality', 'Body', 'Thinking Process', 'Grandeur'),
                  NonUsers=round(as.numeric(dfp[2:dim(dfp)[1],1]),2),
                  Users=round(as.numeric(dfp[2:dim(dfp)[1],2]),2))
chartJSRadar(scores=dfp, labelSize=20, height=700,
             main = paste('Ich-Störungen (EPI) and Psychedelic Drug Use (', 'n1 = ', table(screen_df$group)['NP'],', n2 = ',
                          table(screen_df$group)['PP'],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)


# SEPI: ENDO vs DRUG
#sscreen_df <- screen_df[screen_df$drug_psychedelics==1,]
sscreen_df <- screen_df
sepi <- cbind(apply(sscreen_df[,c('SEPI_iden','SEPI_demarc','SEPI_consist','SEPI_act','SEPI_vit','SEPI_body','SEPI_thought', 'SEPI_overcomp')],2,mean),
              apply(sscreen_df[,c('SEPI_iden_drug','SEPI_demarc_drug','SEPI_consist_drug','SEPI_act_drug','SEPI_vit_drug','SEPI_body_drug','SEPI_thought_drug', 'SEPI_overcomp_drug')],2,mean))

sepi <- data.frame(labels = c('Identity','Demarcation','Consistency','Activity',
                             'Vitality', 'Body', 'Thinking Process', 'Grandeur'),
                  ENDO=round(as.numeric(sepi[1:dim(sepi)[1],1]),2),
                  DRUG=round(as.numeric(sepi[1:dim(sepi)[1],2]),2))
chartJSRadar(scores=sepi, labelSize=20, height=700,scaleStartValue=0.2,
             main = paste('Ich-Störungen (EPI): Endogenous vs Drug-Induced (', 'n = ', dim(sscreen_df)[1],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)

t.test(sscreen_df$SEPI_body,sscreen_df$SEPI_body_drug)



library(ggplot2)

myDataM <- apply(fdata_df_PP[,c("psyAddict", "psyCritTh", "psySom", "psyPerc", "psyPolit", "psySocial",
                                "psyCog", "psyAnxDep", "psySelf","psySpi")],2,mean)


myDataSD <- apply(fdata_df_PP[,c("psyAddict", "psyCritTh", "psySom", "psyPerc", "psyPolit", "psySocial",
                                 "psyCog", "psyAnxDep", "psySelf","psySpi")],2,sd)


myData <- as.data.frame(cbind(myDataM, myDataSD))
colnames(myData) <- c('mean', 'sd')
myData$names <- rownames(myData)


myData$Position <- factor(myData$names, levels=myData$names)

myData$se <- myData$sd / sqrt(dim(fdata_df_PP)[1])


rownames(myData) <- c(1:dim(myData)[1])

myData$cols <- myData$mean>0

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = myData$mean + myData$se,
              ymin = myData$mean - myData$se)

myData <- myData[order(myData$mean),]

p <- ggplot(data = myData, aes(x = Position, y = mean, fill = cols))

p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank())




###### SUMMARY and MAILINGS:

# Task-type assignment:
vis <- as.data.frame(read.xlsx2('/Users/alebedev/Documents/Projects/HUD/visits/W1-2019-01-30.xlsx',1))
vis$email <- gsub(" ","",tolower(vis$email))
df1$email <- gsub(" ","",tolower(df1$email))

df_vis <- merge(vis, df1, by='email')

# Check dimensions:
dim(df_vis)[1]; dim(vis)[1]
setdiff(vis$email, df_vis$email)

# if ok:
df_vis$task[which(df_vis$group=='PP')] <- sample(c('iA1_SWE','iA2_SWE', 'iB1_SWE', 'iB2_SWE'), length(which(df_vis$group=='PP')), replace = T)
df_vis$task[which(df_vis$group=='NP')] <- sample(c('iA1_SWE','iA2_SWE', 'iB1_SWE', 'iB2_SWE'), length(which(df_vis$group=='NP')), replace = T)
df_vis <- df_vis[,c('email', 'studyID', 'task','group')]
df_vis <- df_vis[order(df_vis$studyID),]


df_vis_w1 <- df_vis
write.xlsx(df_vis_w1,
           file=paste('/Users/alebedev/Documents/Projects/HUD/visits/W1-randomized-',
                      Sys.Date(),'.xlsx', sep = ''), row.names = F)


# Adding rolls:
sample(c('iA1_SWE','iA2_SWE', 'iB1_SWE', 'iB2_SWE'),10, replace=T)


sample(c('iA1_SWE','iA2_SWE', 'iB1_SWE', 'iB2_SWE'),2)



#

df12 <- subset(df, (df$participation!=3) & (df$diagBP==2 | is.na(df$diagBP))  & (df$diagOCD==2 | is.na(df$diagOCD)) &
                 (df$diagScz==2 | is.na(df$diagScz))  & (df$diagASD==2 | is.na(df$diagASD)) & 
                 (df$diagADHD==2 | is.na(df$diagADHD)) & (df$diagMDep==2 | is.na(df$diagMDep)) &
                 (df$diagOther==2 | is.na(df$diagOther)) &
                 df$mothertongue=='svenska' & df$somaticDS=='nej' & df$medsY1==2 & 
                 (df$age>=18 & df$age <=35) & (df$brainInjuryY1==2 | is.na(df$brainInjuryY1)))




#df12 <- df12[-which(df12$diagOtherWhich=='väntarpåutredningavadhd,autismochocd.psykologharnämntdepressionmeningenfastdiagnos.'),]


#df12 <- df12[!duplicated(df12$email),]
#write.csv(df12[,c('email', 'age', 'sex', 'group')], file=paste('~/Downloads/', 'HUD_invite_SWE_', Sys.Date(),'.csv',sep=''), quote = F, row.names = F)


#df1[,18:27]

rand1 <- read.xlsx2('/Users/alebedev/Documents/Projects/HUD/visits/tested.xlsx',1)

rand1$email <- gsub(" ","",rand1$email)
rand1$email <- gsub(",",".",rand1$email)
rand1$email <- tolower(rand1$email)
rand1 <- rand1[!duplicated(rand1$email),]



dfNew <- subset(df12, is.element(df12$email, rand1$email)!=T)
dfNew <- subset(dfNew, (dfNew$group=='PP' & dfNew$sex!='man') | (dfNew$group=='NP' & dfNew$sex=='man'))
write.csv(dfNew[,c('email', 'age', 'sex', 'group')], file=paste('~/Downloads/', 'HUD_invite_SWE_', Sys.Date(),'.csv',sep=''), quote = F, row.names = F)

# Tested
dfTested <- subset(df, is.element(df$email, rand1$email))
dfTested <- dfTested[!duplicated(dfTested$email),]

t.test(dfTested$age[dfTested$group=='PP'],dfTested$age[dfTested$group=='NP'])
table(dfTested$sex[dfTested$group=='NP'])/sum(table(dfTested$sex[dfTested$group=='NP']))
table(dfTested$sex[dfTested$group=='PP'])/sum(table(dfTested$sex[dfTested$group=='PP']))
write.csv(dfTested[,c('email', 'age', 'sex', 'group')], file=paste('/Users/alebedev/Documents/Projects/HUD/visits/', 'tested-', Sys.Date(),'.csv',sep=''), quote = F, row.names = F)


###
# 
followUpMailings <- subset(screen_df[,c('email', 'drug_psychedelics','age')], is.element(screen_df$email, fdata_df$email)!=T)

followUpMailings <- subset(followUpMailings[,c('email', 'drug_psychedelics','age')], is.element(followUpMailings$email, CONSP_df$email)!=T)
followUpMailings <- subset(followUpMailings[,c('email', 'drug_psychedelics','age')], is.element(followUpMailings$email, esids$email)!=T)


followUpMailings$group <- NA
followUpMailings$group[followUpMailings$drug_psychedelics==0]<-'NP'
followUpMailings$group[followUpMailings$drug_psychedelics==1]<-'PP'


write.csv(followUpMailings[,c('email', 'group','age')], file=paste('~/Downloads/', 'FU_invite_', Sys.Date(),'.csv',sep=''), quote = F, row.names = F)

