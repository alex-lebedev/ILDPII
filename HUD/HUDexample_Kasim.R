# clear workspace:
rm(list=ls())

# Load libraries:
library(reghelper)
library(ggplot2)
library(ggstance)
library(radarchart)

# Load data:
load('/Users/alebedev/Documents/Projects/HUD/HUD_anonymized.rda')
# Selecting subsamples of those without any psychiatric diagnoses:
SCREEN_df_noPsych <- subset(SCREEN_df, SCREEN_df$PsychDiagAny==0)
CONSP_df_noPsych <- subset(CONSP_df, CONSP_df$PsychDiagAny==0)

#########################
### Group comparisons ###
#########################

### Whole sample:
# T-test: 
t.test(SCREEN_df$DP[SCREEN_df$drug_psychedelics==0],SCREEN_df$DP[SCREEN_df$drug_psychedelics==1])
# Plot:
boxplot(SCREEN_df$DP[SCREEN_df$drug_psychedelics==0],SCREEN_df$DP[SCREEN_df$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_df$drug_psychedelics==0)[2])), SCREEN_df$DP[SCREEN_df$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_df$drug_psychedelics==1)[2])), SCREEN_df$DP[SCREEN_df$drug_psychedelics==1]), pch=16)


# Two-sample t-test: 
t.test(SCREEN_df_noPsych$DP[SCREEN_df_noPsych$drug_psychedelics==0],SCREEN_df_noPsych$DP[SCREEN_df_noPsych$drug_psychedelics==1])
# Plot:
boxplot(SCREEN_df_noPsych$DP[SCREEN_df_noPsych$drug_psychedelics==0],SCREEN_df_noPsych$DP[SCREEN_df_noPsych$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_df_noPsych$drug_psychedelics==0)[2])), SCREEN_df_noPsych$DP[SCREEN_df_noPsych$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_df_noPsych$drug_psychedelics==1)[2])), SCREEN_df_noPsych$DP[SCREEN_df_noPsych$drug_psychedelics==1]), pch=16)

# DP:
# DP-GLM1 (whole sample):
fit.alldrugs <- glm(DP ~ age+sex+drug_psychedelics+drug_opi+drug_mdma+drug_alc+drug_cannabis+drug_tobacco+drug_stim, data=SCREEN_df)
plot_coefs(fit.alldrugs, scale = TRUE)+xlim(limits = c(-1, 1))
# DP-GLM2 (no psychiatric background):
fit.alldrugs <- glm(DP ~ age+sex+drug_psychedelics+drug_opi+drug_mdma+drug_alc+drug_cannabis+drug_tobacco+drug_stim, data=SCREEN_df_noPsych)
plot_coefs(fit.alldrugs, scale = TRUE)+xlim(limits = c(-1, 1))


###############################
####  Continious variables ####
### (overall drug exposure) ###
###############################
# GLM1 (whole sample):
fit.alldrugs <- glm(DP ~ age+sex+ALC_freqprox+TOB_freqprox+CAN_freqprox+MDMA_freqprox+STIM_freqprox+OPI_freqprox+PSY_freqprox, data=CONSP_df)
plot_coefs(fit.alldrugs, scale = TRUE)+xlim(limits = c(-1, 1))
# GLM2 (no psychiatric background):
fit.alldrugs <- glm(DP ~ age+sex+ALC_freqprox+TOB_freqprox+CAN_freqprox+MDMA_freqprox+STIM_freqprox+OPI_freqprox+PSY_freqprox, data=CONSP_df_noPsych)
plot_coefs(fit.alldrugs, scale = TRUE)+xlim(limits = c(-1, 1))





# BADE:
fit <- (glm(EII2~age+sex+ALC_freqprox+TOB_freqprox+CAN_freqprox+MDMA_freqprox+STIM_freqprox+PSY_freqprox, data=HUDMAIN_df))
plot_coefs(fit, scale = TRUE)



# (Below are the plots for SEPI. Keep in mind that the distributuions are not transformed to meet normality criteria):
boxplot(SCREEN_df$SEPI_tot_drug[SCREEN_df$drug_psychedelics==0],SCREEN_df$SEPI_tot_drug[SCREEN_df$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_df$drug_psychedelics==0)[2])), SCREEN_df$SEPI_tot_drug[SCREEN_df$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_df$drug_psychedelics==1)[2])), SCREEN_df$SEPI_tot_drug[SCREEN_df$drug_psychedelics==1]), pch=16)

boxplot(SCREEN_df$SEPI_tot[SCREEN_df$drug_psychedelics==0],SCREEN_df$SEPI_tot[SCREEN_df$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_df$drug_psychedelics==0)[2])), SCREEN_df$SEPI_tot[SCREEN_df$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_df$drug_psychedelics==1)[2])), SCREEN_df$SEPI_tot[SCREEN_df$drug_psychedelics==1]), pch=16)


boxplot(SCREEN_df$SEPI_tot_drug[SCREEN_df$drug_psychedelics==0],SCREEN_df$SEPI_tot_drug[SCREEN_df$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_df$drug_psychedelics==0)[2])), SCREEN_df$SEPI_tot_drug[SCREEN_df$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_df$drug_psychedelics==1)[2])), SCREEN_df$SEPI_tot_drug[SCREEN_df$drug_psychedelics==1]), pch=16)

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

SCREEN_df$group<-NA
SCREEN_df$group[SCREEN_df$drug_psychedelics==1] <- 'PP'
SCREEN_df$group[SCREEN_df$drug_psychedelics==0] <- 'NP'


# O-LIFE:
dfp <- t(aggregate(SCREEN_df[,c('OLIFE_UE','OLIFE_CD','OLIFE_IA', 'OLIFE_IN')],by=list(SCREEN_df$group), FUN=mean))
dfp <- data.frame(labels = c('Unusual Experiences','Cognitive Disorganization',
                             'Introvertive Anhedonia', 'Impulsive Nonconformity'),
                  NonUsers=round(as.numeric(dfp[2:5,1]),2),
                  Users=round(as.numeric(dfp[2:5,2]),2))
chartJSRadar(scores=dfp, labelSize=20, scaleStartValue=2.5,height=700,
             main = paste('Schizotypy (O-LIFE) and Psychedelic Drug Use (', 'n1 = ', table(SCREEN_df$group)['NP'],', n2 = ',
                          table(SCREEN_df$group)['PP'],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)

v='OLIFE_UE'
t.test(SCREEN_df[SCREEN_df$group=='NP',v],SCREEN_df[SCREEN_df$group=='PP',v])


# PDI:
SCREEN_df[,c('PDI_conv','PDI_dist','PDI_time')][is.na(SCREEN_df[,c('PDI_conv','PDI_dist','PDI_time')])] <- 0
dfp <- t(aggregate(SCREEN_df[,c('PDI_conv','PDI_dist','PDI_time')],by=list(SCREEN_df$group), FUN=mean))
dfp <- data.frame(labels = c('Conviction','Disturbance','Preoccupation'),
                  NonUsers=round(as.numeric(dfp[2:4,1]),2),
                  Users=round(as.numeric(dfp[2:4,2]),2))
chartJSRadar(scores=dfp, labelSize=20, scaleStartValue=1.6,height=700,
             main = paste('Delusion-proneness (PDI) and Psychedelic Drug Use (', 'n = ', dim(SCREEN_df)[1],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)

v='PDI_conv'
t.test(SCREEN_df[SCREEN_df$group=='NP',v],SCREEN_df[SCREEN_df$group=='PP',v])

# SEPI (endo):
dfp <- t(aggregate(SCREEN_df[,c('SEPI_iden','SEPI_demarc','SEPI_consist','SEPI_act','SEPI_vit','SEPI_body','SEPI_thought', 'SEPI_overcomp')],by=list(SCREEN_df$group), FUN=mean))
dfp <- data.frame(labels = c('Identity','Demarcation','Consistency','Activity',
                             'Vitality', 'Body', 'Thinking Process', 'Grandeur'),
                  NonUsers=round(as.numeric(dfp[2:dim(dfp)[1],1]),2),
                  Users=round(as.numeric(dfp[2:dim(dfp)[1],2]),2))
chartJSRadar(scores=dfp, labelSize=20, height=700,scaleStartValue=0.1,
             main = paste('Ich-Störungen (EPI) and Psychedelic Drug Use (', 'n1 = ', table(SCREEN_df$group)['NP'],', n2 = ',
                          table(SCREEN_df$group)['PP'],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)

v='SEPI_demarc'
t.test(SCREEN_df[SCREEN_df$group=='NP',v],SCREEN_df[SCREEN_df$group=='PP',v])

# SEPI (drug):
dfp <- t(aggregate(SCREEN_df[,c('SEPI_iden_drug','SEPI_demarc_drug','SEPI_consist_drug','SEPI_act_drug','SEPI_vit_drug','SEPI_body_drug','SEPI_thought_drug', 'SEPI_overcomp_drug')],by=list(SCREEN_df$group), FUN=mean))
dfp <- data.frame(labels = c('Identity','Demarcation','Consistency','Activity',
                             'Vitality', 'Body', 'Thinking Process', 'Grandeur'),
                  NonUsers=round(as.numeric(dfp[2:dim(dfp)[1],1]),2),
                  Users=round(as.numeric(dfp[2:dim(dfp)[1],2]),2))
chartJSRadar(scores=dfp, labelSize=20, height=700,
             main = paste('Ich-Störungen (EPI) and Psychedelic Drug Use (', 'n1 = ', table(SCREEN_df$group)['NP'],', n2 = ',
                          table(SCREEN_df$group)['PP'],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)


##################
### SUPPLEMENT ###
##################

# Interaction with Openness:
load('/Users/alebedev/Documents/Projects/HUD/HUD.rda')
tmp <- merge(SCREEN_df, SCRFU_df[,c('email', 'O')], by='email', all=T)
summary((glm(DP~group*O, data=tmp)))




