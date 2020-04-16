# HUD_complete_data_summary.R

############################
### I. Initial screening ###
############################
# Clean workspace:
rm(list=ls())
# Load libraries:
library(xlsx)
library(rstan)
library(stringi)

screen_raw <- read.xlsx2('/Users/alebedev/Downloads/Export.xlsx',2, stringsAsFactors=F)

screen_raw[screen_raw==999] <- NA
screen_df <- data.frame(ID = screen_raw$ID, consentYes1 = screen_raw$VAR000, username = screen_raw$VAR001,email=screen_raw$VAR213,  
            age=screen_raw$VAR211,sex=screen_raw$VAR212, education=screen_raw$VAR214, occupation=screen_raw$VAR215, 
            mothertongue=screen_raw$VAR216, surveyfoundout=screen_raw$VAR217,
             somaticDS = screen_raw$VAR002,
             diagMDep = screen_raw$VAR003_1,diagBP = screen_raw$VAR003_2, diagScz = screen_raw$VAR003_3,
             diagADHD = screen_raw$VAR003_4,diagASD = screen_raw$VAR003_5, diagOCD = screen_raw$VAR003_6,
             diagOther = screen_raw$VAR003_7, diagOtherWhich = screen_raw$VAR003C, brainInjuryY1 = screen_raw$VAR004,
             medsY1 = screen_raw$VAR005, medsWhich = screen_raw$VAR005C,
             drug_alc = screen_raw$VAR101_1,drug_tobacco = screen_raw$VAR101_2, drug_mdma = screen_raw$VAR101_3,
             drug_cannabis = screen_raw$VAR101_4, drug_stim = screen_raw$VAR101_5, drug_opi = screen_raw$VAR101_6,
             drug_psychedelics = screen_raw$VAR101_7, drug_none = screen_raw$VAR101_8)


screen_df$drug_alc <- as.factor(2-as.numeric(screen_df$drug_alc))
screen_df$drug_tobacco <- as.factor(2-as.numeric(screen_df$drug_tobacco))
screen_df$drug_mdma <- as.factor(2-as.numeric(screen_df$drug_mdma))
screen_df$drug_cannabis <- as.factor(2-as.numeric(screen_df$drug_cannabis))
screen_df$drug_stim <- as.factor(2-as.numeric(screen_df$drug_stim))
screen_df$drug_opi <- as.factor(2-as.numeric(screen_df$drug_opi))
screen_df$drug_psychedelics <- as.factor(2-as.numeric(screen_df$drug_psychedelics))
screen_df$drug_none <- as.factor(2-as.numeric(screen_df$drug_none))



for (i in 1:length(screen_raw$ID)){
  # Select ith row:
  tmp <- subset(screen_raw[i,])
  
  # PDI:
  screen_df$PDI_total[i] <- sum(tmp[,paste('VAR', c('006','008','010', '012', '014', '016', '018', '020', '022',
                                           '024', '026', '028', '030', '032', '034', '036', '038', '040', '042',
                                           '044', '046', '048', '050', '052', '054', '056'), sep='')]==1)
  
  screen_df$PDI_dist[i] <- mean(as.numeric(as.vector(tmp[,paste('VAR', c('007','009','011', '013', '015', '017', '019', '021', '023',
                                                                         '025', '027', '029', '031', '033', '035', '037', '039', '041', '043',
                                                                         '045', '047', '049', '051', '053', '055', '057'),'_1', sep='')])), na.rm=T)
  
  screen_df$PDI_time[i] <- mean(as.numeric(as.vector(tmp[,paste('VAR', c('007','009','011', '013', '015', '017', '019', '021', '023',
                                                                         '025', '027', '029', '031', '033', '035', '037', '039', '041', '043',
                                                                         '045', '047', '049', '051', '053', '055', '057'),'_2', sep='')])), na.rm=T)
  
  screen_df$PDI_conv[i] <- mean(as.numeric(as.vector(tmp[,paste('VAR', c('007','009','011', '013', '015', '017', '019', '021', '023',
                                                                         '025', '027', '029', '031', '033', '035', '037', '039', '041', '043',
                                                                         '045', '047', '049', '051', '053', '055', '057'),'_3', sep='')])), na.rm=T)

  
  # O-LIFE:
  screen_df$OLIFE_UE[i] <- sum(tmp[,paste('VAR', c('058', '059', '060', '061', '062', '063', '064', '065', '066',
                                                   '067', '068', '069'), sep='')]==1)
  
  screen_df$OLIFE_CD[i] <- sum(tmp[,paste('VAR', c('070', '071', '072', '073', '074', '075',
                                                   '076', '077', '078', '079', '080'), sep='')]==1)
  
  screen_df$OLIFE_IA[i] <- sum(tmp[,paste('VAR', c('081', '082','086', '089', '090'), sep='')]==1) +
                            sum(tmp[,paste('VAR', c('083', '084','085', '087', '088'), sep='')]==2)
  
  screen_df$OLIFE_IN[i] <- sum(tmp[,paste('VAR', c('092', '093', '095', '097', '098', '099','100'), sep='')]==1) +
                            sum(tmp[,paste('VAR', c('091', '094', '096'), sep='')]==2)
  
  
  # SEPI:
  iden <- tmp[171:185]
  screen_df$SEPI_iden[i] <- sum(iden[,seq(1,length(iden),3)][iden[(seq(1,length(iden),3)+1)]!=1]==1)
  screen_df$SEPI_iden_drug[i] <- sum(iden[,seq(1,length(iden),3)][iden[(seq(1,length(iden),3)+1)]==1]==1)

  
  demarc <- tmp[186:197]
  screen_df$SEPI_demarc[i] <- sum(demarc[,seq(1,length(demarc),3)][demarc[(seq(1,length(demarc),3)+1)]!=1]==1)
  screen_df$SEPI_demarc_drug[i] <- sum(demarc[,seq(1,length(demarc),3)][demarc[(seq(1,length(demarc),3)+1)]==1]==1)
  
  consist <- tmp[198:212]
  screen_df$SEPI_consist[i] <- sum(consist[,seq(1,length(consist),3)][consist[(seq(1,length(consist),3)+1)]!=1]==1)
  screen_df$SEPI_consist_drug[i] <- sum(consist[,seq(1,length(consist),3)][consist[(seq(1,length(consist),3)+1)]==1]==1)
  
  act <- tmp[213:224]
  screen_df$SEPI_act[i] <- sum(act[,seq(1,length(act),3)][act[(seq(1,length(act),3)+1)]!=1]==1)
  screen_df$SEPI_act_drug[i] <- sum(act[,seq(1,length(act),3)][act[(seq(1,length(act),3)+1)]==1]==1)
  
  vit <- tmp[225:239]
  screen_df$SEPI_vit[i] <- sum(vit[,seq(1,length(vit),3)][vit[(seq(1,length(vit),3)+1)]!=1]==1)
  screen_df$SEPI_vit_drug[i] <- sum(vit[,seq(1,length(vit),3)][vit[(seq(1,length(vit),3)+1)]==1]==1)
  
  overcomp <- tmp[,240:257]
  screen_df$SEPI_overcomp[i] <- sum(overcomp[,seq(1,length(overcomp),3)][overcomp[(seq(1,length(overcomp),3)+1)]!=1]==1)
  screen_df$SEPI_overcomp_drug[i] <- sum(overcomp[,seq(1,length(overcomp),3)][overcomp[(seq(1,length(overcomp),3)+1)]==1]==1)
  
  body <- tmp[258:287]
  screen_df$SEPI_body[i] <- sum(body[,seq(1,length(body),3)][body[(seq(1,length(body),3)+1)]!=1]==1)
  screen_df$SEPI_body_drug[i] <- sum(body[,seq(1,length(body),3)][body[(seq(1,length(body),3)+1)]==1]==1)
  
  thought <- tmp[288:308]
  screen_df$SEPI_thought[i] <- sum(thought[,seq(1,length(thought),3)][thought[(seq(1,length(thought),3)+1)]!=1]==1)
  screen_df$SEPI_thought_drug[i] <- sum(thought[,seq(1,length(thought),3)][thought[(seq(1,length(thought),3)+1)]==1]==1)
  
  psychomot <- tmp[309:329]
  screen_df$SEPI_psychomot[i] <- sum(psychomot[,seq(1,length(psychomot),3)][psychomot[(seq(1,length(psychomot),3)+1)]!=1]==1)
  screen_df$SEPI_psychomot_drug[i] <- sum(psychomot[,seq(1,length(psychomot),3)][psychomot[(seq(1,length(psychomot),3)+1)]==1]==1)

  # ASRS:
  screen_df$ASRS[i] <- sum(as.numeric(as.vector(tmp[,paste('VAR208', 1:18, sep='_')])), na.rm=T)
  
  # Handeness:
  screen_df$hand[i] <- mean(as.numeric(as.vector(tmp[,paste('VAR209', 1:10, sep='_')])), na.rm=T)
  
  # RAADS:
  raads <- tmp[358:372]
  screen_df$raads_young[i] <- sum(raads[c(1:5,7:15)]==1)-sum(raads[6]==1)
  screen_df$raads_now[i] <- sum(raads[c(1:5,7:15)]==2)-sum(raads[6]==2)
  screen_df$raads_both[i] <- sum(raads[c(1:5,7:15)]==3)-sum(raads[6]==3)
  screen_df$raads_any[i] <- sum(raads[c(1:5,7:15)]!=0)-sum(raads[6]!=0)
  }

#screen_df <- screen_df[-141,] # + mb username: NCA4618

screen_df$SEPI_tot <- apply(screen_df[,c('SEPI_iden', 'SEPI_demarc', 'SEPI_consist',
                      'SEPI_act', 'SEPI_vit', 'SEPI_overcomp', 'SEPI_body', 'SEPI_thought')],1,sum)
screen_df$SEPI_tot_drug <- apply(screen_df[,c('SEPI_iden_drug', 'SEPI_demarc_drug', 'SEPI_consist_drug',
                                         'SEPI_act_drug', 'SEPI_vit_drug', 'SEPI_overcomp_drug', 'SEPI_body_drug', 'SEPI_thought_drug')],1,sum)
screen_df$OLIFE_tot <- screen_df$OLIFE_UE+screen_df$OLIFE_CD+screen_df$OLIFE_IA+screen_df$OLIFE_IN
screen_df$email <- tolower(screen_df$email)
screen_df$email <- gsub(",",".",screen_df$email)
screen_df$email <- gsub(";",".",screen_df$email)
screen_df$email <- gsub(" ","",screen_df$email)
screen_df$email[stri_detect_fixed(as.vector(screen_df$email), 'blank')] <- paste0('blankHUD',sample(1:length(screen_df$email[stri_detect_fixed(as.vector(screen_df$email), 'blank')])))
screen_df <- screen_df[!duplicated(screen_df$email,fromLast = T),]


screen_df$sex <- gsub(" ","",tolower(screen_df$sex))
screen_df$nonbingen <- 'no'
screen_df$nonbingen[is.element(screen_df$sex, c('kvinna(intergender)','icke-binär(föddikvinnligtkodadkropp)','nb','man(biologiskt,menkanskeodefinieratmentalt)',
                                          'sermigsomickebinärmenärföddkvinna,harintegjortnågrakönskorrigerandeoperationerutantarbaratestosteronenl.läkareordinationer'))] <- 'yes'

screen_df$sex[is.element(screen_df$sex, c('femail',  "",'kvinna(intergender)', 'kvinnor',
            'female', 'f', 'woman', 'kvinns', 'kvinba', 'icke-binär(föddikvinnligtkodadkropp)',
            'sermigsomickebinärmenärföddkvinna,harintegjortnågrakönskorrigerandeoperationerutantarbaratestosteronenl.läkareordinationer'))] <- 'kvinna'
screen_df$sex[is.element(screen_df$sex, c('male', 'man.', 'm','mam', 'kille','man(biologiskt,menkanskeodefinieratmentalt)'))] <- 'man'
screen_df$sex[!is.element(screen_df$sex, c('kvinna', 'man'))] <- NA

screen_df$age <- tolower(screen_df$age)
screen_df$age <- gsub(",",".",screen_df$age)
screen_df$age <- gsub(";",".",screen_df$age)
screen_df$age <- gsub(" ","",screen_df$age)
screen_df$age[is.element(screen_df$age,c('24.','19år', '26(1992)', 'nilas.alakoski@gmail.com', '24å4', '18år'))] <- c(19,26,NA,24,24, 18,18)
screen_df$age <- as.numeric(screen_df$age)

# Select subset of those without psychiatric disorders:
screen_df_sel <- screen_df[which((screen_df$diagADHD==2 | is.na(screen_df$diagADHD))  &
                                   (screen_df$diagASD==2 | is.na(screen_df$diagASD)) &
                                   (screen_df$diagMDep==2 | is.na(screen_df$diagMDep)) &
                                   (screen_df$diagBP==2 | is.na(screen_df$diagBP)) &
                                   (screen_df$diagScz==2 | is.na(screen_df$diagScz)) &
                                   (screen_df$diagOCD==2 | is.na(screen_df$diagOCD)) &
                                   (screen_df$diagOther==2 | is.na(screen_df$diagOther))),]

screen_df_selSCH <- screen_df[which((screen_df$diagADHD==1)  |
                                    (screen_df$diagASD==1) |
                                    (screen_df$diagBP==1) |
                                    (screen_df$diagScz==1)),]





###############################
### II. Follow-up screening ###
###############################
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
      fdata_df$psyVisual[i] <- as.numeric(tmp[,'VAR3_16'])
      fdata_df$psyAuditory[i] <- as.numeric(tmp[,'VAR3_17'])
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
      fdata_df$psyVisual[i] <- as.numeric(tmp[,'VAR3_16'])
      fdata_df$psyAuditory[i] <- as.numeric(tmp[,'VAR3_17'])
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

fdata_df_NP$psyVisual <- NA
fdata_df_NP$psyAuditory <- NA
fdata_df_NP2$psyVisual <- NA
fdata_df_NP2$psyAuditory <- NA

fdata_df <- rbind(fdata_df_NP[,c('email','group', 'participation', 'O', 'C', 'E', 'A', 'N', 'psyVisual', 'psyAuditory')],
                  fdata_df_PP[,c('email','group','participation', 'O', 'C', 'E', 'A', 'N', 'psyVisual', 'psyAuditory')],
                  fdata_df_NP2[,c('email','group', 'participation', 'O', 'C', 'E', 'A', 'N','psyVisual', 'psyAuditory')],
                  fdata_df_PP2[,c('email','group','participation', 'O', 'C', 'E', 'A', 'N','psyVisual', 'psyAuditory')])




fdata_df$email <- gsub(" ","",fdata_df$email)
fdata_df$email <- gsub(",",".",fdata_df$email)
fdata_df$email <- gsub(";",".",fdata_df$email)
fdata_df$email <- tolower(fdata_df$email)
fdata_df$email[fdata_df$email=="supermegaduperkurt@icloud.com"] <- "supermegaduperkurt@gmail.com"

# Preprocess text:
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

df <- merge(fdata_df, SCREEN_df, by = 'email')
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
df_all$email[stri_detect_fixed(as.vector(df_all$email), 'blank')] <- paste0('blankT',sample(1:length(df_all$email[stri_detect_fixed(as.vector(df_all$email), 'blank')])))
df_all <- df_all[!duplicated(df_all$email,fromLast = T),]

df_allf <- merge(df_all, fdata_df_PP[,c('email','psyExEmo', 'psySocial','psyCritTh', 'psyCog', 'psyAnxDep','psySelf',
                                        'psyPerc', 'psySom', 'psyAddict', 'psySpi', 'psyPolit','psyVisual', 'psyAuditory')], replace=T)

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





SCRFU_df <- df
CONSP_df <- fdata2_df

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
CONSP_df$email[stri_detect_fixed(as.vector(CONSP_df$email), 'blank')] <- paste0('blankCONSP1',sample(1:length(CONSP_df$email[stri_detect_fixed(as.vector(CONSP_df$email), 'blank')])))
CONSP_df <- CONSP_df[!duplicated(CONSP_df$email,fromLast = T),]
tmp1 <- merge(CONSP_df, SCREEN_df[,c('email','sex', 'age', 'diagMDep', 'diagBP', 'diagScz', 'diagADHD', 'diagASD','diagOCD',
                                     'diagOther', 'brainInjuryY1', 'DP', 'OLIFE_totLog', 'PDI_totalLog', 'SEPI_tot','SEPI_tot_drug', 'ASRSLog','raads_anyLog')], by.x ='email', all=F)


tmp2 <- CONSP_df[is.element(CONSP_df$email,setdiff(CONSP_df$email, tmp1$email)),]
tmp2[,c('sex', 'age', 'diagMDep', 'diagBP', 'diagScz', 'diagADHD', 'diagASD','diagOCD',
        'diagOther', 'brainInjuryY1', 'DP', 'OLIFE_totLog', 'PDI_totalLog','SEPI_tot','SEPI_tot_drug', 'ASRSLog','raads_anyLog')] <- NA

CONSP_df <- rbind(tmp1, tmp2)
CONSP_df$email[stri_detect_fixed(as.vector(CONSP_df$email), 'blank')] <- paste0('blankCONSP2',sample(1:length(CONSP_df$email[stri_detect_fixed(as.vector(CONSP_df$email), 'blank')])))
CONSP_df <- CONSP_df[!duplicated(CONSP_df$email,fromLast = T),]
CONSP_df$sex <- as.factor(CONSP_df$sex)

CONSP_df$ALC_prox[CONSP_df$ALC_prox==1] <- 9
CONSP_df$TOB_prox[CONSP_df$TOB_prox==1] <- 9
CONSP_df$CAN_prox[CONSP_df$CAN_prox==1] <- 9
CONSP_df$MDMA_prox[CONSP_df$MDMA_prox==1] <- 9
CONSP_df$STIM_prox[CONSP_df$STIM_prox==1] <- 9
CONSP_df$OPI_prox[CONSP_df$OPI_prox==1] <- 9
CONSP_df$PSY_prox[CONSP_df$PSY_prox==1] <- 9
CONSP_df[,c('ALC_prox', 'TOB_prox', 'CAN_prox','MDMA_prox','STIM_prox', 'OPI_prox', 'PSY_prox')] <- 
  10-CONSP_df[,c('ALC_prox', 'TOB_prox', 'CAN_prox','MDMA_prox','STIM_prox', 'OPI_prox', 'PSY_prox')]
CONSP_df$ALC_freqprox <- apply(scale(CONSP_df[,c('ALC_freq', 'ALC_prox')]),1,mean)
CONSP_df$TOB_freqprox <- apply(scale(CONSP_df[,c('TOB_freq', 'TOB_prox')]),1,mean)
CONSP_df$CAN_freqprox <- apply(scale(CONSP_df[,c('CAN_freq', 'CAN_prox')]),1,mean)
CONSP_df$MDMA_freqprox <- apply(scale(CONSP_df[,c('MDMA_freq', 'MDMA_prox')]),1,mean)
CONSP_df$STIM_freqprox <- apply(scale(CONSP_df[,c('STIM_freq', 'STIM_prox')]),1,mean)
CONSP_df$OPI_freqprox <- apply(scale(CONSP_df[,c('OPI_freq', 'OPI_prox')]),1,mean)
CONSP_df$PSY_freqprox <- apply(scale(CONSP_df[,c('PSY_freq', 'PSY_prox')]),1,mean)

SCREEN_df$PsychDiagAny <- 1
SCRFU_df$PsychDiagAny <- 1
CONSP_df$PsychDiagAny <- 1

SCREEN_df$PsychDiagAny[is.element(SCREEN_df$email, screen_df_sel$email)]<-0
SCRFU_df$PsychDiagAny[is.element(SCRFU_df$email, screen_df_sel$email)]<-0
CONSP_df$PsychDiagAny[is.element(CONSP_df$email, screen_df_sel$email)]<-0
save('SCREEN_df','SCRFU_df','CONSP_df' ,file='/Users/alebedev/Documents/Projects/HUD/HUD.rda')

#############################
### III. Experimental arm ###
#############################

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

SCRFU_df <- merge(SCRFU_df, CONSP_df[,c('email',
            'ALC_freqprox','TOB_freqprox', 'CAN_freqprox','MDMA_freqprox','STIM_freqprox','OPI_freqprox','PSY_freqprox',
            'ALC_freq','TOB_freq', 'CAN_freq','MDMA_freq','STIM_freq','OPI_freq','PSY_freq',
            'ALC_prox','TOB_prox', 'CAN_prox','MDMA_prox','STIM_prox','OPI_prox','PSY_prox')],
            by='email', all=T)
SCRFU_df <- SCRFU_df[!duplicated(SCRFU_df$email, fromLast=T),]



SCREEN_df$raads_anyLog[which(SCREEN_df$raads_anyLog==-Inf)] <- 0
SCRFU_df$raads_anyLog[which(SCRFU_df$raads_anyLog==-Inf)] <- 0
CONSP_df$raads_anyLog[which(CONSP_df$raads_anyLog==-Inf)] <- 0
HUDMAIN_df$raads_anyLog[which(HUDMAIN_df$raads_anyLog==-Inf)] <- 0


# Clean education:
SCREEN_df$education<-tolower(SCREEN_df$education)
SCREEN_df$education <- gsub(" ","",SCREEN_df$education)
SCREEN_df$education <- gsub(";","",SCREEN_df$education)
SCREEN_df$education <- gsub(",",".",SCREEN_df$education)
SCREEN_df$education <- gsub("ingen","0",SCREEN_df$education)
SCREEN_df$education <- gsub("-","0",SCREEN_df$education)
SCREEN_df$education <- as.numeric(SCREEN_df$education)

# Final data:
save(SCREEN_df, SCRFU_df, HUDMAIN_df, CONSP_df, file='/Users/alebedev/Documents/Projects/HUD/HUD_final_old2020.rda')

