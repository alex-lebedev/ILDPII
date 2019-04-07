# Laod libraries:
library(reghelper)
library(ggplot2)
library(ggstance)

# Load data:
load('/Users/alebedev/Documents/Projects/HUD/HUD.rda')
load('/Users/alebedev/Documents/Projects/HUD/HUDMAIN_df.rda')

# Two-sample t-test: 
t.test(SCREEN_DF$DP[SCREEN_DF$drug_psychedelics==0],SCREEN_DF$DP[SCREEN_DF$drug_psychedelics==1])

# Plot:
boxplot(SCREEN_DF$DP[SCREEN_DF$drug_psychedelics==0],SCREEN_DF$DP[SCREEN_DF$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_DF$drug_psychedelics==0)[2])), SCREEN_DF$DP[SCREEN_DF$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_DF$drug_psychedelics==1)[2])), SCREEN_DF$DP[SCREEN_DF$drug_psychedelics==1]), pch=16)


# Fit GLM:
fit.alldrugs <- glm(DP ~ drug_psychedelics+drug_opi+drug_mdma+drug_alc+drug_cannabis+drug_tobacco+drug_stim, data=SCREEN_DF)
summary(fit.alldrugs)
beta(fit.alldrugs) # standardized coeffs

# With demographics:
fit.full <- glm(DP ~ drug_psychedelics+drug_opi+drug_mdma+drug_alc+drug_cannabis+drug_tobacco+drug_stim+sex+age, data=SCREEN_DF)
summary(fit.full)
plot_coefs(fit.full, scale = TRUE)


# BADE:
fit <- (glm(EII2~age+sex+ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+PSY_prox+
           ALC_freq+TOB_freq+CAN_freq+MDMA_freq+STIM_freq+PSY_freq, data=HUDMAIN_df))
plot_coefs(fit, scale = TRUE)
