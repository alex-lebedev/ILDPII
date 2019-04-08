
# Install packages:
#install.packages('jtools')
#install.packages('broom', dependencies=T)

# Load libraries:
library(reghelper)
library(ggplot2)
library(ggstance)
library(radarchart)

# Load data:
load('/Users/alebedev/Documents/Projects/HUD/HUD.rda')
load('/Users/alebedev/Documents/Projects/HUD/HUDMAIN_df.rda')


# Two-sample t-test:
t.test(CONSP_df$CONS5[CONSP_df$group=='NP'],CONSP_df$CONS5[CONSP_df$group=='PP'], 'less')
# Plot:
boxplot(CONSP_df$CONS5[CONSP_df$group=='NP'],CONSP_df$CONS5[CONSP_df$group=='PP'])
points(cbind(jitter(rep(1, table(CONSP_df$group=='NP')[2])), CONSP_df$CONS5[CONSP_df$group=='NP']), pch=16)
points(cbind(jitter(rep(2, table(CONSP_df$group=='PP')[2])), CONSP_df$CONS5[CONSP_df$group=='PP']), pch=16)

fit <- (glm(CONS5~age+sex+ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox+
                         ALC_freq+TOB_freq+CAN_freq+MDMA_freq+STIM_freq+OPI_freq+PSY_freq, data=CONSP_df))

plot_coefs(fit, scale = TRUE)

# GLM
# standardized coefficients:
beta(glm(CONS5~PSY_prox, data=CONSP_df))
beta(glm(CONS5~PSY_freq, data=CONSP_df))


fit.full <- (glm(CONS5~age+sex+ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox+
                         ALC_freq+TOB_freq+CAN_freq+MDMA_freq+STIM_freq+OPI_freq+PSY_freq, data=CONSP_df))

plot_coefs(fit.full, scale = TRUE)


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
