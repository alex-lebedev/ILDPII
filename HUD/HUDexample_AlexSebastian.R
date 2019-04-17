
# Install packages:
#install.packages('jtools')
#install.packages('broom')
#install.ackages('corrplot')

# Load libraries:
library(reghelper)
library(ggplot2)
library(ggstance)
library(radarchart)
library(gtools)
library(corrplot)

# Load data:
load('/Users/alebedev/Documents/Projects/HUD/HUD_anonymized.rda')


# Two-sample t-test:
t.test(CONSP_df$CONS5[CONSP_df$group=='NP'],CONSP_df$CONS5[CONSP_df$group=='PP'], 'less')
# Plot:
boxplot(CONSP_df$CONS5[CONSP_df$group=='NP'],CONSP_df$CONS5[CONSP_df$group=='PP'])
points(cbind(jitter(rep(1, table(CONSP_df$group=='NP')[2])), CONSP_df$CONS5[CONSP_df$group=='NP']), pch=16)
points(cbind(jitter(rep(2, table(CONSP_df$group=='PP')[2])), CONSP_df$CONS5[CONSP_df$group=='PP']), pch=16)

# Main analysis:
fitCONS5 <- (glm(CONS5~age+sex+ALC_freqprox+TOB_freqprox+CAN_freqprox+MDMA_freqprox+STIM_freqprox+OPI_freqprox+PSY_prox, data=CONSP_df))
plot_coefs(fitCONS5, scale = TRUE)

# Follow-up analyses (each question):
fitCONS_public <- (glm(CONS_public~age+sex+ALC_freqprox+TOB_freqprox+CAN_freqprox+MDMA_freqprox+STIM_freqprox+OPI_freqprox+PSY_prox, data=CONSP_df))
fitCONS_polit <- (glm(CONS_polit~age+sex+ALC_freqprox+TOB_freqprox+CAN_freqprox+MDMA_freqprox+STIM_freqprox+OPI_freqprox+PSY_prox, data=CONSP_df))
fitCONS_monit <- (glm(CONS_monit~age+sex+ALC_freqprox+TOB_freqprox+CAN_freqprox+MDMA_freqprox+STIM_freqprox+OPI_freqprox+PSY_prox, data=CONSP_df))
fitCONS_connect <- (glm(CONS_connect~age+sex+ALC_freqprox+TOB_freqprox+CAN_freqprox+MDMA_freqprox+STIM_freqprox+OPI_freqprox+PSY_prox, data=CONSP_df))
fitCONS_org <- (glm(CONS_org~age+sex+ALC_freqprox+TOB_freqprox+CAN_freqprox+MDMA_freqprox+STIM_freqprox+OPI_freqprox+PSY_prox, data=CONSP_df))

# Check summaries:
beta(fitCONS5)
beta(fitCONS_public)


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



# Correlation plots:
corrplot(cor(CONSP_df[,50:56]), col='black', method='number', type='lower')


