# ILDPII_NewScreen.R
 
############################
### I. New screening ###
############################
# Clean workspace:
rm(list=ls())
# Load libraries:
library(xlsx)
library(rstan)

screen_raw <- read.xlsx2('/Users/alebedev/Downloads/newScr.xlsx',2, stringsAsFactors=F)

screen_raw[screen_raw==999] <- NA
screen_df <- data.frame(ID = screen_raw$ID, consentYes1 = screen_raw$VAR000, email=screen_raw$VAR213,  
                        age=screen_raw$VAR211,sex=screen_raw$VAR212, education=screen_raw$VAR214, occupation=screen_raw$VAR215, 
                        mothertongue=screen_raw$VAR216, surveyfoundout=screen_raw$VAR217,
                        somaticDS = screen_raw$VAR002,
                        diagMDep = screen_raw$VAR003_1,diagBP = screen_raw$VAR003_2, diagScz = screen_raw$VAR003_3,
                        diagADHD = screen_raw$VAR003_4,diagASD = screen_raw$VAR003_5, diagOCD = screen_raw$VAR003_6,
                        diagOther = screen_raw$VAR003_7, diagOtherWhich = screen_raw$VAR003C, brainInjuryY1 = screen_raw$VAR004,
                        medsY1 = screen_raw$VAR005, medsWhich = screen_raw$VAR005C,
                        ChTraum = screen_raw$ChTraum_1,
                        ChTraumSex = screen_raw$ChTraum_2_1, ChTraumPhys = screen_raw$ChTraum_2_2, ChTraumNeglect = screen_raw$ChTraum_2_3, 
                        ChTraumBull = screen_raw$ChTraum_2_4, ChTraumHousehold = screen_raw$ChTraum_2_5,ChTraumOther = screen_raw$ChTraum_2_6,
                        drug_alc = screen_raw$VAR101_1,drug_tobacco = screen_raw$VAR101_2, drug_mdma = screen_raw$VAR101_3,
                        drug_cannabis = screen_raw$VAR101_4, drug_stim = screen_raw$VAR101_5, drug_opi = screen_raw$VAR101_6,
                        drug_psychedelics = screen_raw$VAR101_7, drug_none = screen_raw$VAR101_8, participation=screen_raw$VAR4)


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
  screen_df$PDI_total[i] <- sum(tmp[,paste0('VAR', c('006','008','010', '012', '014', '016', '018', '020', '022',
                                                    '024', '026', '028', '030', '032', '034', '036', '038', '040', '042',
                                                    '044', '046', '048', '050', '052', '054', '056'))]==1)
  
  screen_df$PDI_dist[i] <- mean(as.numeric(as.vector(tmp[,paste0('VAR', c('007','009','011', '013', '015', '017', '019', '021', '023',
                                                                         '025', '027', '029', '031', '033', '035', '037', '039', '041', '043',
                                                                         '045', '047', '049', '051', '053', '055', '057'),'_1')])), na.rm=T)
  
  screen_df$PDI_time[i] <- mean(as.numeric(as.vector(tmp[,paste0('VAR', c('007','009','011', '013', '015', '017', '019', '021', '023',
                                                                         '025', '027', '029', '031', '033', '035', '037', '039', '041', '043',
                                                                         '045', '047', '049', '051', '053', '055', '057'),'_2')])), na.rm=T)
  
  screen_df$PDI_conv[i] <- mean(as.numeric(as.vector(tmp[,paste0('VAR', c('007','009','011', '013', '015', '017', '019', '021', '023',
                                                                         '025', '027', '029', '031', '033', '035', '037', '039', '041', '043',
                                                                         '045', '047', '049', '051', '053', '055', '057'),'_3')])), na.rm=T)
  
  
  # O-LIFE:
  screen_df$OLIFE_UE[i] <- sum(tmp[,paste0('VAR', c('058', '059', '060', '061', '062', '063', '064', '065', '066',
                                                   '067', '068', '069'))]==1)
  
  screen_df$OLIFE_CD[i] <- sum(tmp[,paste0('VAR', c('070', '071', '072', '073', '074', '075',
                                                   '076', '077', '078', '079', '080'))]==1)
  
  screen_df$OLIFE_IA[i] <- sum(tmp[,paste0('VAR', c('081', '082','086', '089', '090'))]==1) +
    sum(tmp[,paste0('VAR', c('083', '084','085', '087', '088'))]==2)
  
  screen_df$OLIFE_IN[i] <- sum(tmp[,paste0('VAR', c('092', '093', '095', '097', '098', '099','100'))]==1) +
    sum(tmp[,paste0('VAR', c('091', '094', '096'))]==2)
  
  
  # SEPI:
  iden <- tmp[,paste0('VAR', c('102', '103', '103C','104', '105', '105C',
                               '106', '107', '107C','108', '109', '109C','110', '111', '111C'))]
  screen_df$SEPI_iden[i] <- sum(iden[,seq(1,length(iden),3)][iden[(seq(1,length(iden),3)+1)]!=1]==1)
  screen_df$SEPI_iden_drug[i] <- sum(iden[,seq(1,length(iden),3)][iden[(seq(1,length(iden),3)+1)]==1]==1)
  
  
  demarc <- tmp[,paste0('VAR', c('112', '113', '113C','114', '115', '115C',
                               '116', '117', '117C','118', '119', '119C'))]
  screen_df$SEPI_demarc[i] <- sum(demarc[,seq(1,length(demarc),3)][demarc[(seq(1,length(demarc),3)+1)]!=1]==1)
  screen_df$SEPI_demarc_drug[i] <- sum(demarc[,seq(1,length(demarc),3)][demarc[(seq(1,length(demarc),3)+1)]==1]==1)
  
  
  consist <- tmp[,paste0('VAR', c('120', '121', '121C','122', '123', '123C',
                               '124', '125', '125C','126', '127', '127C','128', '129', '129C'))]
  screen_df$SEPI_consist[i] <- sum(consist[,seq(1,length(consist),3)][consist[(seq(1,length(consist),3)+1)]!=1]==1)
  screen_df$SEPI_consist_drug[i] <- sum(consist[,seq(1,length(consist),3)][consist[(seq(1,length(consist),3)+1)]==1]==1)
  
  
  act <- tmp[,paste0('VAR', c('130', '131', '131C','132', '133', '133C',
                                  '134', '135', '135C','136', '137', '137C'))]
  screen_df$SEPI_act[i] <- sum(act[,seq(1,length(act),3)][act[(seq(1,length(act),3)+1)]!=1]==1)
  screen_df$SEPI_act_drug[i] <- sum(act[,seq(1,length(act),3)][act[(seq(1,length(act),3)+1)]==1]==1)
  
  
  vit <- tmp[,paste0('VAR', c('138', '139', '139C','140', '141', '141C',
                    '142', '143', '143C','144', '145', '145C','146', '147', '147C'))]
  screen_df$SEPI_vit[i] <- sum(vit[,seq(1,length(vit),3)][vit[(seq(1,length(vit),3)+1)]!=1]==1)
  screen_df$SEPI_vit_drug[i] <- sum(vit[,seq(1,length(vit),3)][vit[(seq(1,length(vit),3)+1)]==1]==1)
  
  
  overcomp <- tmp[,paste0('VAR', c('148', '149', '149C','150', '151', '151C',
                              '152', '153', '153C','154', '155', '155C',
                              '156', '157', '157C', '158', '159', '159C'))]
  screen_df$SEPI_overcomp[i] <- sum(overcomp[,seq(1,length(overcomp),3)][overcomp[(seq(1,length(overcomp),3)+1)]!=1]==1)
  screen_df$SEPI_overcomp_drug[i] <- sum(overcomp[,seq(1,length(overcomp),3)][overcomp[(seq(1,length(overcomp),3)+1)]==1]==1)
  
  
  body <- tmp[,paste0('VAR', c('160', '161', '161C','162', '163', '163C',
                              '164', '165', '165C','166', '167', '167C',
                              '168', '169', '169C','170', '171', '171C',
                              '172', '173', '173C','174', '175', '175C',
                              '176', '177', '177C','178', '179', '179C'))]
  screen_df$SEPI_body[i] <- sum(body[,seq(1,length(body),3)][body[(seq(1,length(body),3)+1)]!=1]==1)
  screen_df$SEPI_body_drug[i] <- sum(body[,seq(1,length(body),3)][body[(seq(1,length(body),3)+1)]==1]==1)
  
  
  thought <- tmp[,paste0('VAR', c('180', '181', '181C','182', '183', '183C',
                               '184', '185', '185C','186', '187', '187C',
                               '188', '189', '189C','190', '191', '191C',
                               '192', '193', '193C'))]
  screen_df$SEPI_thought[i] <- sum(thought[,seq(1,length(thought),3)][thought[(seq(1,length(thought),3)+1)]!=1]==1)
  screen_df$SEPI_thought_drug[i] <- sum(thought[,seq(1,length(thought),3)][thought[(seq(1,length(thought),3)+1)]==1]==1)
  
  psychomot <- tmp[,paste0('VAR', c('194', '195', '195C','196', '197', '197C',
                                  '198', '199', '199C','200', '201', '201C',
                                  '202', '203', '203C','204', '205', '205C',
                                  '206', '207', '207C'))]
  screen_df$SEPI_psychomot[i] <- sum(psychomot[,seq(1,length(psychomot),3)][psychomot[(seq(1,length(psychomot),3)+1)]!=1]==1)
  screen_df$SEPI_psychomot_drug[i] <- sum(psychomot[,seq(1,length(psychomot),3)][psychomot[(seq(1,length(psychomot),3)+1)]==1]==1)
  
  # ASRS:
  screen_df$ASRS[i] <- sum(as.numeric(as.vector(tmp[,paste0('VAR208_', 1:18)])), na.rm=T)
  
  screen_df$MLQ_presence[i] <- mean(as.numeric(tmp[,paste0('MLQ_',c(1,4,5,6))]), 7-as.numeric(tmp[,paste0('MLQ_',c(9))]))
  screen_df$MLQ_search[i] <- mean(as.numeric(tmp[,paste0('MLQ_',c(2,3,7,8,10))]))
  screen_df$SWLS[i] <- mean(as.numeric(tmp[,paste0('SWLS_',c(1:5))]))
  
  # RAADS:
  raads <- tmp[,paste0('VAR210_', 1:15)]
  
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
screen_df$email[stri_detect_fixed(as.vector(screen_df$email), 'blank')] <- paste0('blankNS',sample(1:length(screen_df$email[stri_detect_fixed(as.vector(screen_df$email), 'blank')])))
screen_df <- screen_df[!duplicated(screen_df$email,fromLast = T),]


ILDPII_screen <- screen_df
ILDPII_screen$age <- tolower(ILDPII_screen$age)
ILDPII_screen$age <- gsub(",",".",ILDPII_screen$age)
ILDPII_screen$age <- gsub(";",".",ILDPII_screen$age)
ILDPII_screen$age <- gsub(" ","",ILDPII_screen$age)
ILDPII_screen$age <- gsub("år","",ILDPII_screen$age)
ILDPII_screen$age[ILDPII_screen$age=='19fyller2026:efebruari'] <- 19.5
ILDPII_screen$age[ILDPII_screen$age=='299'] <- 29
ILDPII_screen$age[ILDPII_screen$age=='född1964'] <- 56
ILDPII_screen$age[ILDPII_screen$age=='22.23inovember'] <- 22.5
ILDPII_screen$age <- as.numeric(ILDPII_screen$age)

save(ILDPII_screen, file = '/Users/alebedev/Documents/Projects/ILDPII/ILDPII_screen.rda')










