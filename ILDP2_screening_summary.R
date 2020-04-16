# ILDP2_screening_summary.R

library(xlsx)

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

screen_df$DP <- apply(scale(screen_df[,c('PDI_total', 'OLIFE_tot')]),1,mean)
#screen_df$DP <- apply(log1p(scale(screen_df[,c('PDI_total', 'OLIFE_tot')])+2),1,mean)
#screen_df$DP <- apply(cbind(scale(log1p(screen_df$PDI_total)),scale(screen_df$OLIFE_tot)),1,mean)

screen_df$email <- tolower(screen_df$email)
screen_df$email <- gsub(",",".",screen_df$email)
screen_df$email <- gsub(";",".",screen_df$email)
screen_df$email <- gsub(" ","",screen_df$email)
screen_df <- screen_df[!duplicated(screen_df$email),]


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
screen_df$age[is.element(screen_df$age,c('24.','19år', '26(1992)', 'nilas.alakoski@gmail.com', '24å4', '18år'))] <- c(24,19,26, NA, 24, 18)
screen_df$age <- as.numeric(screen_df$age)

screen_df$DP <- screen_df$DP-min(screen_df$DP)
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



# screen_df_sel <- screen_df_sel[as.numeric(screen_df_sel$diagOtherWhich)==1,]
t.test(screen_df$DP[screen_df$drug_psychedelics==0],screen_df$DP[screen_df$drug_psychedelics==1])
boxplot(screen_df$DP[screen_df$drug_psychedelics==0],screen_df$DP[screen_df$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(screen_df$drug_psychedelics==0)[2])), screen_df$DP[screen_df$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(screen_df$drug_psychedelics==1)[2])), screen_df$DP[screen_df$drug_psychedelics==1]), pch=16)



t.test(screen_df_sel$DP[screen_df_sel$drug_psychedelics==0],screen_df_sel$DP[screen_df_sel$drug_psychedelics==1])

finalGLM <- glm(DP ~ drug_psychedelics+drug_opi+drug_mdma+drug_alc+drug_cannabis+drug_tobacco+drug_stim+sex+age, data=screen_df)
summary(finalGLM)

finalGLM <- glm(DP ~ drug_psychedelics+drug_opi+drug_mdma+drug_alc+drug_cannabis+drug_tobacco+drug_stim+sex+age, data=screen_df_sel)
summary(finalGLM)



save('screen_df', 'screen_df_sel', file='/Users/alebedev/Documents/Projects/HUD/screen_df.rda')











### Prepare data for follow-up survey:

responders <- data.frame('FN' = screen_df$drug_psychedelics, 'Last name' = screen_df$mothertongue, 'E-mail' = screen_df$email)
responders$First.name[responders$FN==0] <- 'NP'
responders$First.name[responders$FN==1] <- 'PP'

responders <- responders[,c('First.name', 'Last.name', 'E.mail')]

responders$Last.name <- gsub(",","/",responders$Last.name)



write.csv(responders,file=paste('~/Downloads/', Sys.Date(), '_ALL_responders_1-',dim(responders)[1],'.csv', sep=''), quote = F, row.names = F)

write.csv(responders[responders$First.name=='NP',],file=paste('~/Downloads/', Sys.Date(), '_NP_responders_1-',dim(responders)[1],'.csv', sep=''), quote = F, row.names = F)
write.csv(responders[responders$First.name=='PP',],file=paste('~/Downloads/', Sys.Date(), '_PP_responders_1-',dim(responders)[1],'.csv', sep=''), quote = F, row.names = F)


#screen_df$age <- as.numeric(as.vector(screen_df$age))

save(screen_df, file='/Users/alebedev/GitHub/ILDPII/HUD/screening/HUD_sreen1.rda')
screen_df <- screen_df[-which(screen_df$DP==max(screen_df$DP)),]

#finalGLM <- glm(screen_df$DP ~ screen_df$drug_psychedelics+screen_df$drug_cannabis+screen_df$drug_mdma+screen_df$drug_opi+screen_df$drug_alc+screen_df$drug_tobacco+screen_df$drug_stim)
#summary(finalGLM)

mylogit <- glm(screen_df$drug_psychedelics ~ screen_df$DP, family = "binomial")
exp(cbind(OR = coef(mylogit), confint(mylogit)))
summary(mylogit)


