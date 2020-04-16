# Otilia and Andreas
rm(list=ls())
load('/Users/alebedev/Documents/Projects/ILDPII/AndresOtilia_anonymized.rda')

# Initial checks:
# CONS5
fitDAF <- (glm(CONS5~PSY_prox, data=ALLFU_wDemogr))
summary(fitDAF)

fitDAF <- (glm(EBS_polit~PSY_prox, data=ALLEBS_wDemogr))
summary(fitDAF)



ALLEBS_wDemogr$education <- as.numeric(ALLEBS_wDemogr$education)
ALLFU_wDemogr$education <- as.numeric(ALLFU_wDemogr$education)

# These are the target (preregistered) models:

# CONS5
fitDAF <- (glm(CONS5~age+sex+ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox, data=ALLFU_wDemogr))
summary(fitDAF)

# EBS:
fitDAF <- (glm(EBS_polit~age+sex+ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox, data=ALLEBS_wDemogr))
summary(fitDAF)





fitDAF <- (glm(EBS_polit~age+sex+ALC_freq+TOB_freq+CAN_freq+MDMA_freq+STIM_freq+OPI_freq+PSY_freq, data=ALLEBS_wDemogr))
summary(fitDAF)
