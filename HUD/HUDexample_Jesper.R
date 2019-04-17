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


########################
### Experimental Arm ###
########################

# Experimental arm:
fit <- (glm(rtDYNfast~age+sex+ALC_freqprox+TOB_freqprox+CAN_freqprox+MDMA_freqprox+STIM_freqprox+OPI_freqprox+PSY_prox, data=HUDMAIN_df))
summary(fit, scale = TRUE)


