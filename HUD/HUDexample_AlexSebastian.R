# Clear workspace:
rm(list=ls())
# Load libraries:
library(psych)
library(radarchart)
library(gtools)
library(corrplot)

# Load data:
load('/Users/alebedev/Documents/Projects/HUD/HUD_anonymized.rda')

# Main model:
fitCONS5 <- (glm(CONS5~age+sex+ALC_freqprox+TOB_freqprox+CAN_freqprox+MDMA_freqprox+STIM_freqprox+OPI_freqprox+PSY_prox, data=CONSP_df))

# Summary:
beta(fitCONS5)

# Plot standardized regression coefficients:
plot_coefs(fitCONS5, scale = TRUE)

# Correlation plot (relationships between exposure to different drugs):
corrplot(cor(CONSP_df[,c('ALC_freqprox','TOB_freqprox', 'CAN_freqprox',
                         'MDMA_freqprox', 'STIM_freqprox', 'OPI_freqprox', 'PSY_freqprox')]),
         col='black', method='number', type='lower')

# Correlation plot depicting relationships of CMQ-scores with different clinical scales:
# Keep only compelte data:
complete.dataset <- CONSP_df[,c('CONS5', 'OLIFE_totLog','PDI_totalLog', 'ASRSLog', 'raads_anyLog')]
complete.dataset <- complete.dataset[complete.cases(complete.dataset),]
# Corrplot:
corrplot(cor(complete.dataset), col='black', method='number', type='lower')

# Statistical tests for significance of the correlations with adjustment for multiple tests with Bonferroni):
corr.test(complete.dataset$CONS5, complete.dataset[,c('OLIFE_totLog','PDI_totalLog', 'ASRSLog', 'raads_anyLog')], adjust='bonferroni')


##################
### SUPPLEMENT ###
##################
# Follow-up (secondary) analyses, 1 model for each question: 
fitCONS_public <- (glm(CONS_public~age+sex+ALC_freqprox+TOB_freqprox+CAN_freqprox+MDMA_freqprox+STIM_freqprox+OPI_freqprox+PSY_prox, data=CONSP_df))
fitCONS_polit <- (glm(CONS_polit~age+sex+ALC_freqprox+TOB_freqprox+CAN_freqprox+MDMA_freqprox+STIM_freqprox+OPI_freqprox+PSY_prox, data=CONSP_df))
fitCONS_monit <- (glm(CONS_monit~age+sex+ALC_freqprox+TOB_freqprox+CAN_freqprox+MDMA_freqprox+STIM_freqprox+OPI_freqprox+PSY_prox, data=CONSP_df))
fitCONS_connect <- (glm(CONS_connect~age+sex+ALC_freqprox+TOB_freqprox+CAN_freqprox+MDMA_freqprox+STIM_freqprox+OPI_freqprox+PSY_prox, data=CONSP_df))
fitCONS_org <- (glm(CONS_org~age+sex+ALC_freqprox+TOB_freqprox+CAN_freqprox+MDMA_freqprox+STIM_freqprox+OPI_freqprox+PSY_prox, data=CONSP_df))
# Check summaries:
beta(fitCONS_public) # (the pattern is generally the same across all questions)


# Psychedelic-users VS Non-users (direct group comparison):
# Two-sample t-test:
t.test(CONSP_df$CONS5[CONSP_df$group=='NP'],CONSP_df$CONS5[CONSP_df$group=='PP'], 'less')
# Plot:
boxplot(CONSP_df$CONS5[CONSP_df$group=='NP'],CONSP_df$CONS5[CONSP_df$group=='PP'], col=c('#00BFC4','#F8766D'))
points(cbind(jitter(rep(1, table(CONSP_df$group=='NP')[2])), CONSP_df$CONS5[CONSP_df$group=='NP']), pch=16)
points(cbind(jitter(rep(2, table(CONSP_df$group=='PP')[2])), CONSP_df$CONS5[CONSP_df$group=='PP']), pch=16)

# Radar plots:
dfp <- t(aggregate(CONSP_df[,c('CONS_public','CONS_polit', 'CONS_monit','CONS_connect', 'CONS_org')],by=list(CONSP_df$group), FUN=mean))
dfp <- data.frame(labels = c('Misinformation','Politics', 'Monitoring','Hidden Connections', 'Secret Organisations'), NonUsers=round(as.numeric(dfp[2:6,1]),2),
                  Users=round(as.numeric(dfp[2:6,2]),2))
chartJSRadar(scores=dfp, labelSize=20, scaleStartValue=15,height=700,
             main = paste('Consipracy Mentality and Psychedelic Drug Use (', 'n1 = ', table(SCRFU_df$group)['NP'],', n2 = ',
                          table(SCRFU_df$group)['PP'],')',sep=''),
             colMatrix = cbind(c(54,145,109),c(202,66,16)),lineAlpha=5,polyAlpha=0.3)
             

# CMQ subscales:
v='CONS_org'
t.test(CONSP_df[CONSP_df$group=='NP',v],CONSP_df[CONSP_df$group=='PP',v])






