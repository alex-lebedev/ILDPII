# clear workspace:
rm(list=ls())

# Load libraries:
library(reghelper)
library(ggplot2)
library(ggstance)
library(radarchart)
library(psych)
library(xlsx)
library(nlme)

# Load data:
load('/Users/alebedev/Documents/Projects/HUD/HUD_anonymized.rda')
##### Sampling bias:
SCRFU_df_Jesper <- read.xlsx2('~/Downloads/HUD_anonymized_cleaned.xlsx',2)

        
# Selecting subsamples of those without any psychiatric diagnoses:
SCREEN_df_noPsych <- subset(SCREEN_df, SCREEN_df$PsychDiagAny==0)
CONSP_df_noPsych <- subset(CONSP_df, CONSP_df$PsychDiagAny==0)

dd <- merge(SCRFU_df,SCRFU_df_Jesper[,c('ID', 'surveyfoundout')], by = 'ID')      
dd <- dd[!duplicated(dd$ID),]              
# Visual
dd$psyVisual[dd$psyVisual<0]<-0        
table(dd$psyVisual>0)/sum(table(dd$psyVisual>0))
table(dd$psyVisual==1)[2]/sum(table(dd$psyVisual>0))
table(dd$psyVisual==2)[2]/sum(table(dd$psyVisual>0))
table(dd$psyVisual==3)[2]/sum(table(dd$psyVisual>0))
pie(table(dd$psyVisual),labels =c(''), col=c('white', 'lightgrey','darkgrey','black'))
        
# Auditory:
dd$psyAuditory[dd$psyAuditory<0]<-0
table(dd$psyAuditory>0)/sum(table(dd$psyAuditory>0))
table(dd$psyAuditory==1)[2]/sum(table(dd$psyAuditory>0))
table(dd$psyAuditory==2)[2]/sum(table(dd$psyAuditory>0))
table(dd$psyAuditory==3)[2]/sum(table(dd$psyAuditory>0))
pie(table(dd$psyAuditory),labels =c(''), col=c('white', 'lightgrey','darkgrey','black'))        
#dd <- subset(dd, dd$surveyfoundout!='HPPD Support Group')         # then, do the same

        
# Designer drugs:
dtypes <- c('PP', 'PP2')
d1 <- read.xlsx2(paste('~/Downloads/',dtypes[1], '.xlsx',sep=''),2, stringsAsFactors=F)
d2 <- read.xlsx2(paste('~/Downloads/',dtypes[2], '.xlsx',sep=''),2, stringsAsFactors=F)        
dd1 <- data.frame(email=d1$VAR1, psyVisual = as.numeric(d1$VAR3_16), psyAuditory=as.numeric(d1$VAR3_17), desDrug1=NA,desDrug2=NA)
dd2 <- data.frame(email=d2$VAR1, psyVisual = as.numeric(d2$VAR3_16), psyAuditory=as.numeric(d2$VAR3_17), desDrug1=d2$DP1, desDrug2=d2$DP2)

ddrugs <- rbind(dd1,dd2)
ddrugs <- ddrugs[!duplicated(ddrugs$email),]        
ddrugs$psyVisual[ddrugs$psyVisual<0]<-0
ddrugs$psyAuditory[ddrugs$psyAuditory<0]<-0

# Proportions in relation to designer drug use:
        
table(ddrugs$psyVisual[ddrugs$desDrug1==1]>0)/sum(table(ddrugs$psyVisual[ddrugs$desDrug1==1]>0))
table(ddrugs$psyVisual[ddrugs$desDrug1==2]>0)/sum(table(ddrugs$psyVisual[ddrugs$desDrug1==2]>0))

table(ddrugs$psyAuditory[ddrugs$desDrug1==1]>0)/sum(table(ddrugs$psyAuditory[ddrugs$desDrug1==1]>0))                                                          
table(ddrugs$psyAuditory[ddrugs$desDrug1==2]>0)/sum(table(ddrugs$psyAuditory[ddrugs$desDrug1==2]>0))        

        
        
# DOTS:
# RT:
dots <- data.frame(subject = rep(HUDMAIN_df$subject,4), group = rep(HUDMAIN_df$group.y,4),
                   RT=c(HUDMAIN_df$rtSTfast,HUDMAIN_df$rtSTslow, HUDMAIN_df$rtDYNfast,HUDMAIN_df$rtDYNslow),
                   BGD = as.ordered(c(rep('static', dim(HUDMAIN_df)[1]*2),rep('dynamic', dim(HUDMAIN_df)[1]*2))),
                   SPEED = as.ordered(rep(c(rep('fast', dim(HUDMAIN_df)[1]),rep('slow', dim(HUDMAIN_df)[1])),2))
)

# Acc:
dots <- data.frame(subject = rep(HUDMAIN_df$subject,4), group = rep(HUDMAIN_df$group.y,4),
                   Acc=c(HUDMAIN_df$AccSTfast,HUDMAIN_df$AccSTslow, HUDMAIN_df$AccDYNfast,HUDMAIN_df$AccDYNslow),
                   BGD = as.ordered(c(rep('static', dim(HUDMAIN_df)[1]*2),rep('dynamic', dim(HUDMAIN_df)[1]*2))),
                   SPEED = as.ordered(rep(c(rep('fast', dim(HUDMAIN_df)[1]),rep('slow', dim(HUDMAIN_df)[1])),2))
)


        
 # Regular GLM:
summary(glm(RT~BGD*SPEED*group, data=dots))
summary(glm(Acc~BGD*SPEED*group, data=dots))


# MLE:
modME <- lme(RT~BGD*SPEED*group,data=dots, random=~1|subject)
summary(modME)
modME <- lme(Acc~BGD*SPEED*group,data=dots, random=~1|subject)
summary(modME)

dotsST <- subset(dots, dots$BGD=='static')
dotsDYN <- subset(dots, dots$BGD=='dynamic')

ggplot(dotsST, aes(x=SPEED, y=RT, fill=factor(group))) + 
  stat_summary(fun.y=mean, geom="bar",position=position_dodge(1)) + 
  stat_summary(fun.ymin=min,fun.ymax=max,geom="errorbar",
               color="grey40",position=position_dodge(1), width=.2)+ ylim(0, 3)
ggplot(dotsDYN, aes(x=SPEED, y=RT, fill=factor(group))) + 
  stat_summary(fun.y=mean, geom="bar",position=position_dodge(1)) + 
  stat_summary(fun.ymin=min,fun.ymax=max,geom="errorbar",
               color="grey40",position=position_dodge(1), width=.2)+ ylim(0, 3)


# RT:
boxplot(dots[(dots$BGD=='dynamic' & dots$SPEED=='slow' & dots$group=='NP'),'RT'],dots[(dots$BGD=='dynamic' & dots$SPEED=='slow' & dots$group=='PP'),'RT'],
        dots[(dots$BGD=='dynamic' & dots$SPEED=='fast' & dots$group=='NP'),'RT'],dots[(dots$BGD=='dynamic' & dots$SPEED=='fast' & dots$group=='PP'),'RT'], ylim=c(0,3))
points(jitter(cbind(1, dots[(dots$BGD=='dynamic' & dots$SPEED=='slow' & dots$group=='NP'),'RT']),5), col='#F8766D', pch=16, cex=1)
points(cbind(1, mean(dots[(dots$BGD=='dynamic' & dots$SPEED=='slow' & dots$group=='NP'),'RT'])),col='#F8766D', pch=16, cex=3)
points(jitter(cbind(2, dots[(dots$BGD=='dynamic' & dots$SPEED=='slow' & dots$group=='PP'),'RT']),5), col='#00BFC4', pch=16, cex=1)
points(cbind(2, mean(dots[(dots$BGD=='dynamic' & dots$SPEED=='slow' & dots$group=='PP'),'RT'])),col='#00BFC4', pch=16, cex=3)
points(jitter(cbind(3, dots[(dots$BGD=='dynamic' & dots$SPEED=='fast' & dots$group=='NP'),'RT']),5), col='#F8766D', pch=17, cex=1)
points(cbind(3, mean(dots[(dots$BGD=='dynamic' & dots$SPEED=='fast' & dots$group=='NP'),'RT'])),col='#F8766D', pch=17, cex=3)
points(jitter(cbind(4, dots[(dots$BGD=='dynamic' & dots$SPEED=='fast' & dots$group=='PP'),'RT']),5), col='#00BFC4', pch=17, cex=1)
points(cbind(4, mean(dots[(dots$BGD=='dynamic' & dots$SPEED=='fast' & dots$group=='PP'),'RT'])),col='#00BFC4', pch=17, cex=3)


boxplot(dots[(dots$BGD=='static' & dots$SPEED=='slow' & dots$group=='NP'),'RT'],dots[(dots$BGD=='static' & dots$SPEED=='slow' & dots$group=='PP'),'RT'],
        dots[(dots$BGD=='static' & dots$SPEED=='fast' & dots$group=='NP'),'RT'],dots[(dots$BGD=='static' & dots$SPEED=='fast' & dots$group=='PP'),'RT'], ylim=c(0,3))
points(jitter(cbind(1, dots[(dots$BGD=='static' & dots$SPEED=='slow' & dots$group=='NP'),'RT']),5), col='#F8766D', pch=16, cex=1)
points(cbind(1, mean(dots[(dots$BGD=='static' & dots$SPEED=='slow' & dots$group=='NP'),'RT'])),col='#F8766D', pch=16, cex=3)
points(jitter(cbind(2, dots[(dots$BGD=='static' & dots$SPEED=='slow' & dots$group=='PP'),'RT']),5), col='#00BFC4', pch=16, cex=1)
points(cbind(2, mean(dots[(dots$BGD=='static' & dots$SPEED=='slow' & dots$group=='PP'),'RT'])),col='#00BFC4', pch=16, cex=3)
points(jitter(cbind(3, dots[(dots$BGD=='static' & dots$SPEED=='fast' & dots$group=='NP'),'RT']),5), col='#F8766D', pch=17, cex=1)
points(cbind(3, mean(dots[(dots$BGD=='static' & dots$SPEED=='fast' & dots$group=='NP'),'RT'])),col='#F8766D', pch=17, cex=3)
points(jitter(cbind(4, dots[(dots$BGD=='static' & dots$SPEED=='fast' & dots$group=='PP'),'RT']),5), col='#00BFC4', pch=17, cex=1)
points(cbind(4, mean(dots[(dots$BGD=='static' & dots$SPEED=='fast' & dots$group=='PP'),'RT'])),col='#00BFC4', pch=17, cex=3)
# ACCURACIES:
boxplot(dots[(dots$BGD=='dynamic' & dots$SPEED=='slow' & dots$group=='NP'),'Acc'],dots[(dots$BGD=='dynamic' & dots$SPEED=='slow' & dots$group=='PP'),'Acc'],
        dots[(dots$BGD=='dynamic' & dots$SPEED=='fast' & dots$group=='NP'),'Acc'],dots[(dots$BGD=='dynamic' & dots$SPEED=='fast' & dots$group=='PP'),'Acc'], border='white',ylim=c(0.5,1))
points(jitter(cbind(1, dots[(dots$BGD=='dynamic' & dots$SPEED=='slow' & dots$group=='NP'),'Acc']),5), col='#F8766D', pch=16, cex=1)
points(cbind(1, median(dots[(dots$BGD=='dynamic' & dots$SPEED=='slow' & dots$group=='NP'),'Acc'])),col='#F8766D', pch=16, cex=3)
points(jitter(cbind(2, dots[(dots$BGD=='dynamic' & dots$SPEED=='slow' & dots$group=='PP'),'Acc']),5), col='#00BFC4', pch=16, cex=1)
points(cbind(2, median(dots[(dots$BGD=='dynamic' & dots$SPEED=='slow' & dots$group=='PP'),'Acc'])),col='#00BFC4', pch=16, cex=3)
points(jitter(cbind(3, dots[(dots$BGD=='dynamic' & dots$SPEED=='fast' & dots$group=='NP'),'Acc']),5), col='#F8766D', pch=17, cex=1)
points(cbind(3, median(dots[(dots$BGD=='dynamic' & dots$SPEED=='fast' & dots$group=='NP'),'Acc'])),col='#F8766D', pch=17, cex=3)
points(jitter(cbind(4, dots[(dots$BGD=='dynamic' & dots$SPEED=='fast' & dots$group=='PP'),'Acc']),5), col='#00BFC4', pch=17, cex=1)
points(cbind(4, median(dots[(dots$BGD=='dynamic' & dots$SPEED=='fast' & dots$group=='PP'),'Acc'])),col='#00BFC4', pch=17, cex=3)


boxplot(dots[(dots$BGD=='static' & dots$SPEED=='slow' & dots$group=='NP'),'Acc'],dots[(dots$BGD=='static' & dots$SPEED=='slow' & dots$group=='PP'),'Acc'],
        dots[(dots$BGD=='static' & dots$SPEED=='fast' & dots$group=='NP'),'Acc'],dots[(dots$BGD=='static' & dots$SPEED=='fast' & dots$group=='PP'),'Acc'], border='white',ylim=c(0.5,1))
points(jitter(cbind(1, dots[(dots$BGD=='static' & dots$SPEED=='slow' & dots$group=='NP'),'Acc']),5), col='#F8766D', pch=16, cex=1)
points(cbind(1, median(dots[(dots$BGD=='static' & dots$SPEED=='slow' & dots$group=='NP'),'Acc'])),col='#F8766D', pch=16, cex=3)
points(jitter(cbind(2, dots[(dots$BGD=='static' & dots$SPEED=='slow' & dots$group=='PP'),'Acc']),5), col='#00BFC4', pch=16, cex=1)
points(cbind(2, median(dots[(dots$BGD=='static' & dots$SPEED=='slow' & dots$group=='PP'),'Acc'])),col='#00BFC4', pch=16, cex=3)
points(jitter(cbind(3, dots[(dots$BGD=='static' & dots$SPEED=='fast' & dots$group=='NP'),'Acc']),5), col='#F8766D', pch=17, cex=1)
points(cbind(3, median(dots[(dots$BGD=='static' & dots$SPEED=='fast' & dots$group=='NP'),'Acc'])),col='#F8766D', pch=17, cex=3)
points(jitter(cbind(4, dots[(dots$BGD=='static' & dots$SPEED=='fast' & dots$group=='PP'),'Acc']),5), col='#00BFC4', pch=17, cex=1)
points(cbind(4, median(dots[(dots$BGD=='static' & dots$SPEED=='fast' & dots$group=='PP'),'Acc'])),col='#00BFC4', pch=17, cex=3)





#########################
### Group comparisons ###
#########################

### T-TESTS ###

# O-LIFE (Schizotypy):
t.test(SCREEN_df$OLIFE_totLog[SCREEN_df$drug_psychedelics==0],SCREEN_df$OLIFE_totLog[SCREEN_df$drug_psychedelics==1]) # whole sample
t.test(SCREEN_df_noPsych$OLIFE_totLog[SCREEN_df_noPsych$drug_psychedelics==0],SCREEN_df_noPsych$OLIFE_totLog[SCREEN_df_noPsych$drug_psychedelics==1]) # no psychiatric background

# ASRS (ADHD):
t.test(SCREEN_df$ASRSLog[SCREEN_df$drug_psychedelics==0],SCREEN_df$ASRSLog[SCREEN_df$drug_psychedelics==1]) # whole sample
t.test(SCREEN_df$ASRSLog[SCREEN_df_noPsych$drug_psychedelics==0],SCREEN_df_noPsych$ASRSLog[SCREEN_df_noPsych$drug_psychedelics==1]) # no psychiatric background

# RAADS (autism):
t.test(SCREEN_df$raads_anyLog[SCREEN_df$drug_psychedelics==0],SCREEN_df$raads_anyLog[SCREEN_df$drug_psychedelics==1]) # whole sample
t.test(SCREEN_df_noPsych$raads_anyLog[SCREEN_df_noPsych$drug_psychedelics==0],SCREEN_df_noPsych$raads_anyLog[SCREEN_df_noPsych$drug_psychedelics==1]) # no psychiatric background

### GLMs ###

# OLIFE-GLM1 (whole sample):
fit.alldrugs <- glm(OLIFE_totLog ~ age+sex+drug_psychedelics+drug_opi+drug_mdma+drug_alc+drug_cannabis+drug_tobacco+drug_stim, data=SCREEN_df)
plot_coefs(fit.alldrugs, scale = TRUE)+xlim(limits = c(-0.5, 0.5))
# OLIFE-GLM2 (no psychiatric background):
fit.alldrugs <- glm(OLIFE_totLog ~ age+sex+drug_psychedelics+drug_opi+drug_mdma+drug_alc+drug_cannabis+drug_tobacco+drug_stim, data=SCREEN_df_noPsych)
plot_coefs(fit.alldrugs, scale = TRUE)+xlim(limits = c(-0.5, 0.5))

# ASRSLog-GLM1 (whole sample):
fit.alldrugs <- glm(ASRSLog ~ age+sex+drug_psychedelics+drug_opi+drug_mdma+drug_alc+drug_cannabis+drug_tobacco+drug_stim, data=SCREEN_df)
plot_coefs(fit.alldrugs, scale = TRUE)+xlim(limits = c(-0.5, 0.5))
# ASRSLog-GLM2 (no psychiatric background):
fit.alldrugs <- glm(ASRSLog ~ age+sex+drug_psychedelics+drug_opi+drug_mdma+drug_alc+drug_cannabis+drug_tobacco+drug_stim, data=SCREEN_df_noPsych)
plot_coefs(fit.alldrugs, scale = TRUE)+xlim(limits = c(-0.5, 0.5))

# raads_anyLog:
# raads_anyLog-GLM1 (whole sample):
fit.alldrugs <- glm(raads_anyLog ~ age+sex+drug_psychedelics+drug_opi+drug_mdma+drug_alc+drug_cannabis+drug_tobacco+drug_stim, data=SCREEN_df)
plot_coefs(fit.alldrugs, scale = TRUE)+xlim(limits = c(-1, 1))
# raads_anyLog-GLM2 (no psychiatric background):
fit.alldrugs <- glm(raads_anyLog ~ age+sex+drug_psychedelics+drug_opi+drug_mdma+drug_alc+drug_cannabis+drug_tobacco+drug_stim, data=SCREEN_df_noPsych)
plot_coefs(fit.alldrugs, scale = TRUE)+xlim(limits = c(-1, 1))
fit <- glm(ASRS ~ drug_psychedelics+drug_opi+drug_mdma+drug_alc+drug_cannabis+drug_tobacco+drug_stim+sex+age, data=SCREEN_DF) 
summary(fit)
plot_coefs(fit, scale = TRUE)


#Psychopathology:
# Users only (Pearson)
cor(SCRFU_df[SCRFU_df$PSY_prox>1, c('PSY_freqprox', 'psyVisual', 'psyAuditory','raads_anyLog',  'ASRSLog','PDI_totalLog', 'OLIFE_totLog', 'SEPI_tot')], use='complete.obs');
# Whole sample (Spearman)
cor(SCRFU_df[, c('PSY_freqprox', 'raads_anyLog',  'ASRSLog','PDI_totalLog', 'OLIFE_totLog', 'SEPI_tot')], use='complete.obs', method='spearman');

########################
### Experimental Arm ###
########################

# Experimental arm:
fit <- (glm(rtDYNfast~age+sex+ALC_freqprox+TOB_freqprox+CAN_freqprox+MDMA_freqprox+STIM_freqprox+OPI_freqprox+PSY_prox, data=HUDMAIN_df))
summary(fit, scale = TRUE)








