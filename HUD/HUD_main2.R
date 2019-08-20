# Laod libraries:
library(reghelper)
library(ggplot2)
library(ggstance)
library(radarchart)

# Load data:
load('/Users/alebedev/Documents/Projects/HUD/HUD.rda')
load('/Users/alebedev/Documents/Projects/HUD/HUDMAIN_df.rda')

# Two-sample t-test: 
t.test(SCREEN_DF$DP[SCREEN_DF$drug_psychedelics==0],SCREEN_DF$DP[SCREEN_DF$drug_psychedelics==1])

# Plot:
boxplot(SCREEN_DF$DP[SCREEN_DF$drug_psychedelics==0],SCREEN_DF$DP[SCREEN_DF$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_DF$drug_psychedelics==0)[2])), SCREEN_DF$DP[SCREEN_DF$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_DF$drug_psychedelics==1)[2])), SCREEN_DF$DP[SCREEN_DF$drug_psychedelics==1]), pch=16)

dots <- subset(dots, dots$Acc>=0.2)

# Fit GLM:
fit.alldrugs <- glm(DP ~ drug_psychedelics+drug_opi+drug_mdma+drug_alc+drug_cannabis+drug_tobacco+drug_stim, data=SCREEN_DF)
summary(fit.alldrugs)
beta(fit.alldrugs) # standardized coeffs

# With demographics:
fit.full <- glm(DP ~ drug_psychedelics+drug_opi+drug_mdma+drug_alc+drug_cannabis+drug_tobacco+drug_stim+sex+age, data=SCREEN_DF)
summary(fit.full)
plot_coefs(fit.full, scale = TRUE)


# BADE:
fit <- (glm(EII2~age+sex+ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+PSY_prox+
              ALC_freq+TOB_freq+CAN_freq+MDMA_freq+STIM_freq+PSY_freq, data=HUDMAIN_df))
plot_coefs(fit, scale = TRUE)

summary(fit)


# (Below are the plots for SEPI. Keep in mind that the distributuions are not transformed to meet normality criteria):
boxplot(SCREEN_DF$SEPI_tot_drug[SCREEN_DF$drug_psychedelics==0],SCREEN_DF$SEPI_tot_drug[SCREEN_DF$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_DF$drug_psychedelics==0)[2])), SCREEN_DF$SEPI_tot_drug[SCREEN_DF$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_DF$drug_psychedelics==1)[2])), SCREEN_DF$SEPI_tot_drug[SCREEN_DF$drug_psychedelics==1]), pch=16)

boxplot(SCREEN_DF$SEPI_tot[SCREEN_DF$drug_psychedelics==0],SCREEN_DF$SEPI_tot[SCREEN_DF$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_DF$drug_psychedelics==0)[2])), SCREEN_DF$SEPI_tot[SCREEN_DF$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_DF$drug_psychedelics==1)[2])), SCREEN_DF$SEPI_tot[SCREEN_DF$drug_psychedelics==1]), pch=16)


boxplot(SCREEN_DF$SEPI_tot_drug[SCREEN_DF$drug_psychedelics==0],SCREEN_DF$SEPI_tot_drug[SCREEN_DF$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_DF$drug_psychedelics==0)[2])), SCREEN_DF$SEPI_tot_drug[SCREEN_DF$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_DF$drug_psychedelics==1)[2])), SCREEN_DF$SEPI_tot_drug[SCREEN_DF$drug_psychedelics==1]), pch=16)

# Radar plots:
dfp <- t(aggregate(SCRFU_df[,c('O','C', 'E','A', 'N')],by=list(SCRFU_df$group), FUN=mean))
dfp <- data.frame(labels = c('O','C', 'E','A', 'N'), NonUsers=round(as.numeric(dfp[2:6,1]),2),
                  Users=round(as.numeric(dfp[2:6,2]),2))
chartJSRadar(scores=dfp, labelSize=30, scaleStartValue=3, maxScale=5.5,
             main = paste('BIG 5 and Psychedelic Drug Use (', 'n1 = ', table(SCRFU_df$group)['NP'],', n2 = ',
                          table(SCRFU_df$group)['PP'],')',sep=''),
             height=700, colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)

v='O'
t.test(SCRFU_df[SCRFU_df$group=='PP',v],SCRFU_df[SCRFU_df$group=='NP',v])

dfp <- t(aggregate(CONSP_df[,c('CONS_public','CONS_polit', 'CONS_monit','CONS_connect', 'CONS_org')],by=list(CONSP_df$group), FUN=mean))
dfp <- data.frame(labels = c('Misinformation','Politics', 'Monitoring','Hidden Connections', 'Secret Organisations'), NonUsers=round(as.numeric(dfp[2:6,1]),2),
                  Users=round(as.numeric(dfp[2:6,2]),2))
chartJSRadar(scores=dfp, labelSize=20, scaleStartValue=15,height=700,
             main = paste('Consipracy Mentality and Psychedelic Drug Use (', 'n1 = ', table(SCRFU_df$group)['NP'],', n2 = ',
                          table(SCRFU_df$group)['PP'],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)
v='CONS_public'
t.test(CONSP_df[CONSP_df$group=='NP',v],CONSP_df[CONSP_df$group=='PP',v])

SCREEN_DF$group<-NA
SCREEN_DF$group[SCREEN_DF$drug_psychedelics==1] <- 'PP'
SCREEN_DF$group[SCREEN_DF$drug_psychedelics==0] <- 'NP'


# O-LIFE:
dfp <- t(aggregate(SCREEN_DF[,c('OLIFE_UE','OLIFE_CD','OLIFE_IA', 'OLIFE_IN')],by=list(SCREEN_DF$group), FUN=mean))
dfp <- data.frame(labels = c('Unusual Experiences','Cognitive Disorganization',
                             'Introvertive Anhedonia', 'Impulsive Nonconformity'),
                  NonUsers=round(as.numeric(dfp[2:5,1]),2),
                  Users=round(as.numeric(dfp[2:5,2]),2))
chartJSRadar(scores=dfp, labelSize=20, scaleStartValue=2.5,height=700,
             main = paste('Schizotypy (O-LIFE) and Psychedelic Drug Use (', 'n1 = ', table(SCREEN_DF$group)['NP'],', n2 = ',
                          table(SCREEN_DF$group)['PP'],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)

v='OLIFE_UE'
t.test(SCREEN_DF[SCREEN_DF$group=='NP',v],SCREEN_DF[SCREEN_DF$group=='PP',v])


# PDI:
SCREEN_DF[,c('PDI_conv','PDI_dist','PDI_time')][is.na(SCREEN_DF[,c('PDI_conv','PDI_dist','PDI_time')])] <- 0
dfp <- t(aggregate(SCREEN_DF[,c('PDI_conv','PDI_dist','PDI_time')],by=list(SCREEN_DF$group), FUN=mean))
dfp <- data.frame(labels = c('Conviction','Disturbance','Preoccupation'),
                  NonUsers=round(as.numeric(dfp[2:4,1]),2),
                  Users=round(as.numeric(dfp[2:4,2]),2))
chartJSRadar(scores=dfp, labelSize=20, scaleStartValue=1.6,height=700,
             main = paste('Delusion-proneness (PDI) and Psychedelic Drug Use (', 'n = ', dim(SCREEN_DF)[1],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)

v='PDI_conv'
t.test(SCREEN_DF[SCREEN_DF$group=='NP',v],SCREEN_DF[SCREEN_DF$group=='PP',v])

# SEPI (endo):
dfp <- t(aggregate(SCREEN_DF[,c('SEPI_iden','SEPI_demarc','SEPI_consist','SEPI_act','SEPI_vit','SEPI_body','SEPI_thought', 'SEPI_overcomp')],by=list(SCREEN_DF$group), FUN=mean))
dfp <- data.frame(labels = c('Identity','Demarcation','Consistency','Activity',
                             'Vitality', 'Body', 'Thinking Process', 'Grandeur'),
                  NonUsers=round(as.numeric(dfp[2:dim(dfp)[1],1]),2),
                  Users=round(as.numeric(dfp[2:dim(dfp)[1],2]),2))
chartJSRadar(scores=dfp, labelSize=20, height=700,scaleStartValue=0.1,
             main = paste('Ich-Störungen (EPI) and Psychedelic Drug Use (', 'n1 = ', table(SCREEN_DF$group)['NP'],', n2 = ',
                          table(SCREEN_DF$group)['PP'],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)

v='SEPI_demarc'
t.test(SCREEN_DF[SCREEN_DF$group=='NP',v],SCREEN_DF[SCREEN_DF$group=='PP',v])

# SEPI (drug):
dfp <- t(aggregate(SCREEN_DF[,c('SEPI_iden_drug','SEPI_demarc_drug','SEPI_consist_drug','SEPI_act_drug','SEPI_vit_drug','SEPI_body_drug','SEPI_thought_drug', 'SEPI_overcomp_drug')],by=list(SCREEN_DF$group), FUN=mean))
dfp <- data.frame(labels = c('Identity','Demarcation','Consistency','Activity',
                             'Vitality', 'Body', 'Thinking Process', 'Grandeur'),
                  NonUsers=round(as.numeric(dfp[2:dim(dfp)[1],1]),2),
                  Users=round(as.numeric(dfp[2:dim(dfp)[1],2]),2))
chartJSRadar(scores=dfp, labelSize=20, height=700,
             main = paste('Ich-Störungen (EPI) and Psychedelic Drug Use (', 'n1 = ', table(SCREEN_DF$group)['NP'],', n2 = ',
                          table(SCREEN_DF$group)['PP'],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)


# SEPI: ENDO vs DRUG
sSCREEN_DF <- SCREEN_DF
sepi <- cbind(apply(sSCREEN_DF[,c('SEPI_iden','SEPI_demarc','SEPI_consist','SEPI_act','SEPI_vit','SEPI_body','SEPI_thought', 'SEPI_overcomp')],2,mean),
              apply(sSCREEN_DF[,c('SEPI_iden_drug','SEPI_demarc_drug','SEPI_consist_drug','SEPI_act_drug','SEPI_vit_drug','SEPI_body_drug','SEPI_thought_drug', 'SEPI_overcomp_drug')],2,mean))

sepi <- data.frame(labels = c('Identity','Demarcation','Consistency','Activity',
                              'Vitality', 'Body', 'Thinking Process', 'Grandeur'),
                   ENDO=round(as.numeric(sepi[1:dim(sepi)[1],1]),2),
                   DRUG=round(as.numeric(sepi[1:dim(sepi)[1],2]),2))
chartJSRadar(scores=sepi, labelSize=20, height=700,scaleStartValue=0.2,
             main = paste('Ich-Störungen (EPI): Endogenous vs Drug-Induced (', 'n = ', dim(sSCREEN_DF)[1],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)

t.test(sSCREEN_DF$SEPI_body,sSCREEN_DF$SEPI_body_drug)