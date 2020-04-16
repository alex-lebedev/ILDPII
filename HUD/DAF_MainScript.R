# Otilia and Andreas
rm(list=ls())
load('/Users/alebedev/Documents/Projects/ILDPII/AndresOtilia_anonymized.rda')
  
fitDAF <- (glm(EBS_polit~age+sex+ALC_freq+TOB_freq+CAN_freq+MDMA_freq+STIM_freq+OPI_freq+PSY_freq, data=ALLEBS_wDemogr))
summary(fitDAF)
fitDAF <- (glm(EBS_polit~age+sex+ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox, data=ALLEBS_wDemogr))
summary(fitDAF)