# Alternative Beliefs in Drug Users: Fact and Fiction
# Psykologprogrammet, termin 6, 2020
# Students: Andrés Cabrera & Otilia Horntvedt 
# Supervisor: Alexander Lebedev

rm(list=ls())

library(dplyr)
library(ggpubr)
library(bestNormalize)

load('/Users/alebedev/Dropbox/AndresOtilia_anonymized.rda')

# Creating a binary factor defining psychedelic user group {False;True}
ALLFU_ocean_wDemogr$PSY_group <- ALLFU_ocean_wDemogr$PSY_prox > 1
ALLEBS_wDemogr$PSY_group <- ALLEBS_wDemogr$PSY_prox > 1

# Convert "education" variable to numeric:
ALLEBS_wDemogr$education <- as.numeric(ALLEBS_wDemogr$education)
ALLFU_ocean_wDemogr$education <- as.numeric(ALLFU_ocean_wDemogr$education)
########################
# Density and QQ-plots #
########################

# CONS5 (ok)
ggdensity(ALLFU_ocean_wDemogr$CONS5, fill = "lightgray")
ggqqplot(ALLFU_ocean_wDemogr$CONS5)
# EBS-feel (ok)
ggdensity(ALLEBS_wDemogr$EBS_feel, fill = "lightgray")
ggqqplot(ALLEBS_wDemogr$EBS_feel)
# EBS-evid (non-normal)
ggdensity(ALLEBS_wDemogr$EBS_evid, fill = "lightgray")
ggqqplot(ALLEBS_wDemogr$EBS_evid)
# EBS-polit (non-normal)
ggdensity(ALLEBS_wDemogr$EBS_polit, fill = "lightgray")
ggqqplot(ALLEBS_wDemogr$EBS_polit)

####################
# Apply transforms #
####################
# EBS_evid: choose best normalization method (orderNorm):
bestNormalize(ALLEBS_wDemogr$EBS_evid)$chosen_transform
ALLEBS_wDemogr$EBS_evid_transformed <- bestNormalize(ALLEBS_wDemogr$EBS_evid)$x.t
# EBS_polit: choose best normalization method (Standardized I):
bestNormalize(ALLEBS_wDemogr$EBS_polit)$chosen_transform
ALLEBS_wDemogr$EBS_polit_transformed <- bestNormalize(ALLEBS_wDemogr$EBS_polit)$x.t



############
# ANALYSIS #
############

#########
# CONS5 #
#########

fitCONS5 <- (lm(CONS5~PSY_prox, data=ALLFU_ocean_wDemogr))
summary(fitCONS5)
# The effect is still there even if we regress-out demographics and user/non-user group effects:
fitCONS5 <- (lm(CONS5~age+sex+PSY_group+PSY_prox, data=ALLFU_ocean_wDemogr))
summary(fitCONS5)
# With adjustments for other drugs:
fitCONS5.adj <- (lm(CONS5~age+sex+ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox, data=ALLFU_ocean_wDemogr))
summary(fitCONS5.adj)

#######
# EBS #
#######

# EBS-polit (no adjustments):
fitEBSpolit <- (lm(EBS_polit_transformed~PSY_prox, data=ALLEBS_wDemogr))
summary(fitEBSpolit)
# The effect is still there even if we regress-out demographics and user/non-user group effects:
fitEBSpolit <- (lm(EBS_polit_transformed~age+sex+PSY_group+PSY_prox, data=ALLEBS_wDemogr))
summary(fitEBSpolit)
# EBS-polit (with adjustments for other drugs):
fitEBSpolit.adj <- (lm(EBS_polit_transformed~age+sex+ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox, data=ALLEBS_wDemogr))
summary(fitEBSpolit.adj)


# EBS-evid (no adjustments):
fitEBSevid <- (lm(EBS_evid_transformed~PSY_prox, data=ALLEBS_wDemogr))
summary(fitEBSevid) # non-significant
# EBS-evid (with adjustments):
#fitEBSevid.adj <- (lm(EBS_evid_transformed~age+sex+ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox, data=ALLEBS_wDemogr))
#summary(fitEBSevid.adj)

# EBS-feel (no adjustments):
fitEBSfeel <- (lm(EBS_feel~PSY_prox, data=ALLEBS_wDemogr))
summary(fitEBSfeel) # non-significant
# EBS-feel (with adjustments):
#fitEBSfeel.adj <- (lm(EBS_feel~age+sex+ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox, data=ALLEBS_wDemogr))
#summary(fitEBSfeel.adj)


