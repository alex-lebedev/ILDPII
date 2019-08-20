# HUD: main analysis
library(reghelper)
library(ggplot2)
library(ggstance)

load('/Users/alebedev/Documents/Projects/HUD/HUD_anonymized.rda')


HUDMAIN_df$AccSTdiff<-HUDMAIN_df$AccSTfast-HUDMAIN_df$AccSTslow
HUDMAIN_df$AccDYNdiff<-HUDMAIN_df$AccDYNfast-HUDMAIN_df$AccDYNslow


fit <- glm(DP ~ drug_psychedelics+drug_opi+drug_mdma+drug_alc+drug_cannabis+drug_tobacco+drug_stim+sex+age, data=SCREEN_df)
summary(fit)
plot_coefs(fit, scale = TRUE)


beta(glm(rhos~PSY_prox, data=HUDMAIN_df))
beta(glm(rhos~PSY_freq, data=HUDMAIN_df))
#beta(glm(rhos~age+sex+ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+PSY_prox, data=HUDMAIN_df))
#beta(glm(rhos~age+sex+ALC_freq+TOB_freq+CAN_freq+MDMA_freq+STIM_freq+PSY_freq, data=HUDMAIN_df))
#beta(glm(rhos~age+sex+ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+PSY_prox+O+C+E+A+N, data=HUDMAIN_df))
#fit <- (glm(rhos~age+sex+ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+PSY_prox+
#           ALC_freq+TOB_freq+CAN_freq+MDMA_freq+STIM_freq+PSY_freq, data=HUDMAIN_df))
fit <- (glm(rhos~age+sex+ALC_freqprox+TOB_freqprox+CAN_freqprox+MDMA_freqprox+STIM_freqprox+PSY_freqprox, data=HUDMAIN_df))

plot_coefs(fit, scale = TRUE)


fit <- (glm(EII2~age+sex+ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+PSY_prox+
           ALC_freq+TOB_freq+CAN_freq+MDMA_freq+STIM_freq+PSY_freq, data=HUDMAIN_df))
plot_coefs(fit, scale = TRUE)


boxplot(SCREEN_DF$DP[SCREEN_DF$drug_psychedelics==0],SCREEN_DF$DP[SCREEN_DF$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_DF$drug_psychedelics==0)[2])), SCREEN_DF$DP[SCREEN_DF$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_DF$drug_psychedelics==1)[2])), SCREEN_DF$DP[SCREEN_DF$drug_psychedelics==1]), pch=16)



boxplot(SCREEN_DF$SEPI_tot_drug[SCREEN_DF$drug_psychedelics==0],SCREEN_DF$SEPI_tot_drug[SCREEN_DF$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_DF$drug_psychedelics==0)[2])), SCREEN_DF$SEPI_tot_drug[SCREEN_DF$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_DF$drug_psychedelics==1)[2])), SCREEN_DF$SEPI_tot_drug[SCREEN_DF$drug_psychedelics==1]), pch=16)

boxplot(SCREEN_DF$SEPI_tot[SCREEN_DF$drug_psychedelics==0],SCREEN_DF$SEPI_tot[SCREEN_DF$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_DF$drug_psychedelics==0)[2])), SCREEN_DF$SEPI_tot[SCREEN_DF$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_DF$drug_psychedelics==1)[2])), SCREEN_DF$SEPI_tot[SCREEN_DF$drug_psychedelics==1]), pch=16)


boxplot(SCREEN_DF$SEPI_tot_drug[SCREEN_DF$drug_psychedelics==0],SCREEN_DF$SEPI_tot_drug[SCREEN_DF$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_DF$drug_psychedelics==0)[2])), SCREEN_DF$SEPI_tot_drug[SCREEN_DF$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_DF$drug_psychedelics==1)[2])), SCREEN_DF$SEPI_tot_drug[SCREEN_DF$drug_psychedelics==1]), pch=16)


boxplot(SCREEN_DF$ASRS[SCREEN_DF$drug_psychedelics==0],SCREEN_DF$ASRS[SCREEN_DF$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_DF$drug_psychedelics==0)[2])), SCREEN_DF$ASRS[SCREEN_DF$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_DF$drug_psychedelics==1)[2])), SCREEN_DF$ASRS[SCREEN_DF$drug_psychedelics==1]), pch=16)

boxplot(SCREEN_DF$raads_any[SCREEN_DF$drug_psychedelics==0],SCREEN_DF$raads_any[SCREEN_DF$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_DF$drug_psychedelics==0)[2])), SCREEN_DF$raads_any[SCREEN_DF$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_DF$drug_psychedelics==1)[2])), SCREEN_DF$raads_any[SCREEN_DF$drug_psychedelics==1]), pch=16)


boxplot(CONSP_df$CONS5[CONSP_df$group=='NP'],CONSP_df$CONS5[CONSP_df$group=='PP'])
points(cbind(jitter(rep(1, table(CONSP_df$group=='NP')[2])), CONSP_df$CONS5[CONSP_df$group=='NP']), pch=16)
points(cbind(jitter(rep(2, table(CONSP_df$group=='PP')[2])), CONSP_df$CONS5[CONSP_df$group=='PP']), pch=16)
t.test(CONSP_df$CONS5[CONSP_df$group=='NP'],CONSP_df$CONS5[CONSP_df$group=='PP'], 'less')



boxplot(HUDMAIN_df$rho[HUDMAIN_df$group=='NP'],HUDMAIN_df$rho[HUDMAIN_df$group=='PP'])
points(cbind(jitter(rep(1, table(HUDMAIN_df$group=='NP')[2])), HUDMAIN_df$rho[HUDMAIN_df$group=='NP']), pch=16)
points(cbind(jitter(rep(2, table(HUDMAIN_df$group=='PP')[2])), HUDMAIN_df$rho[HUDMAIN_df$group=='PP']), pch=16)
t.test(HUDMAIN_df$rho[HUDMAIN_df$group=='NP'],HUDMAIN_df$rho[HUDMAIN_df$group=='PP'], 'less')


fit <- (glm(CONS5~age+sex+ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox+
                         ALC_freq+TOB_freq+CAN_freq+MDMA_freq+STIM_freq+OPI_freq+PSY_freq, data=CONSP_df))

plot_coefs(fit, scale = TRUE)

# CONSPIRACY MENTALITY:
beta(glm(CONS5~PSY_prox, data=CONSP_df))
beta(glm(CONS5~PSY_freq, data=CONSP_df))
#beta(glm(CONS5~ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox, data=CONSP_df))
#beta(glm(CONS5~ALC_freq+TOB_freq+CAN_freq+MDMA_freq+STIM_freq+OPI_freq+PSY_freq, data=CONSP_df))
#beta(glm(CONS5~ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox+
#           ALC_freq+TOB_freq+CAN_freq+MDMA_freq+STIM_freq+OPI_freq+PSY_freq, data=CONSP_df))
#beta(glm(CONS5~ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox+age+sex+O+C+E+A+N, data=CONSP_df))

beta(glm(CONS5~age+sex+ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox+
           ALC_freq+TOB_freq+CAN_freq+MDMA_freq+STIM_freq+OPI_freq+PSY_freq, data=CONSP_df))







cor.test((HUDMAIN_df$rhos)^3,sqrt(HUDMAIN_df$PSY_prox), method='pearson')
cor.test((HUDMAIN_df$rhos)^3,sqrt(HUDMAIN_df$PSY_freq), method='pearson')


cor.test(HUDMAIN_df$rhos,HUDMAIN_df$PSY_freq, method='spearman')

cor(HUDMAIN_df$rhos,HUDMAIN_df$PSY_prox, method='spearman')
cor.test(HUDMAIN_df$rhos,HUDMAIN_df$PSY_prox, method='spearman')




# Plot
var = 'rhos'
drug = 'PSY'
prox <- c('Never', '>5yrs ago', '<5yrs ago', '<1yr', '<1month', '<1week','<3days','<24hr')
freq <- c('Never', '1-3 times', '4-10 times', '11-50 times', '>50 times')
tmp <- HUDMAIN_df[,c(var, paste(drug, '_prox', sep=''),paste(drug, '_freq', sep=''))]
colnames(tmp) <- c('var', 'prox', 'freq')
for (i in 1:dim(tmp)[1]){
  tmp$proxnames[i] <- prox[tmp[i,2]]
  tmp$freqnames[i] <- prox[tmp[i,3]]
}

numProx <- as.numeric(names(table(tmp$prox)))
numFreq <- as.numeric(names(table(tmp$freq)))
boxplot(var~prox, data=tmp, names=prox[numProx], col='grey',
        ylab='Effect of Instructed knowledge', xlab='Psychedelics: last time used')
points(var~jitter(prox), data=tmp, names=prox[numProx], pch=16, cex=3)


boxplot(rhos~group, data=HUDMAIN_df, col='grey'); points(rhos~jitter(as.numeric(group)), data=HUDMAIN_df, pch=16, cex=3)

#boxplot(var~freq, data=tmp, names=freq[numFreq])

# Alpha
boxplot(HUDMAIN_df$alpha[HUDMAIN_df$PSY_freq==1],HUDMAIN_df$alpha[HUDMAIN_df$PSY_freq==2],HUDMAIN_df$alpha[HUDMAIN_df$PSY_freq==3],HUDMAIN_df$alpha[HUDMAIN_df$PSY_freq==4],HUDMAIN_df$alpha[HUDMAIN_df$PSY_freq==5])
boxplot(HUDMAIN_df$alpha[HUDMAIN_df$PSY_prox==1],HUDMAIN_df$alpha[HUDMAIN_df$PSY_prox==2],HUDMAIN_df$alpha[HUDMAIN_df$PSY_prox==3],HUDMAIN_df$alpha[HUDMAIN_df$PSY_prox==4],HUDMAIN_df$alpha[HUDMAIN_df$PSY_prox==5])

# evDelta
boxplot(HUDMAIN_df$evDelta[HUDMAIN_df$PSY_freq==1],HUDMAIN_df$evDelta[HUDMAIN_df$PSY_freq==2],HUDMAIN_df$evDelta[HUDMAIN_df$PSY_freq==3],HUDMAIN_df$evDelta[HUDMAIN_df$PSY_freq==4],HUDMAIN_df$evDelta[HUDMAIN_df$PSY_freq==5])
boxplot(HUDMAIN_df$evDelta[HUDMAIN_df$PSY_prox==1],HUDMAIN_df$evDelta[HUDMAIN_df$PSY_prox==2],HUDMAIN_df$evDelta[HUDMAIN_df$PSY_prox==3],HUDMAIN_df$evDelta[HUDMAIN_df$PSY_prox==4],HUDMAIN_df$evDelta[HUDMAIN_df$PSY_prox==5])



# Radar Plots:
library(radarchart)
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
#sSCREEN_DF <- SCREEN_DF[SCREEN_DF$drug_psychedelics==1,]
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






library(ggplot2)

myDataM <- apply(SCRFU_df_PP[,c("psyAddict", "psyCritTh", "psySom", "psyPerc", "psyPolit", "psySocial",
                                "psyCog", "psyAnxDep", "psySelf","psySpi")],2,mean)


myDataSD <- apply(SCRFU_df_PP[,c("psyAddict", "psyCritTh", "psySom", "psyPerc", "psyPolit", "psySocial",
                                 "psyCog", "psyAnxDep", "psySelf","psySpi")],2,sd)


myData <- as.data.frame(cbind(myDataM, myDataSD))
colnames(myData) <- c('mean', 'sd')
myData$names <- rownames(myData)


myData$Position <- factor(myData$names, levels=myData$names)

myData$se <- myData$sd / sqrt(dim(SCRFU_df_PP)[1])


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


