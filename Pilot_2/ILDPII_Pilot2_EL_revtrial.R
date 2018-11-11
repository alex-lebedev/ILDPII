# ILDPII_Pilot2_EL_revtrial.R 
# 
# Author: Alexander V. Lebedev
# Date: 2018-10-31
# Test different reversal events

library(rstan)
library(rstantools)


#sALL_eLife <- read.xlsx2('~/Downloads/elife2016_input.xlsx',1)[,c('subject', 'trial', 'stim', 'gsr','shock', 'revtrial','instr')]
#order <- read.xlsx2('~/Downloads/elife2016_input.xlsx',1)[,c('order', 'counterbalancing')]
#sALL_eLife$order <- paste(order[,1],order[,2], sep='')

#sALL_eLife$trial <- as.numeric(as.vector(sALL_eLife$trial))
load('/Users/alebedev/GitHub/ILDPII/Pilot_2/sALL_eLife.rda')


#sALL_eLife$response <- sqrt(as.numeric(as.vector(sALL_eLife$response)))
#revs <- sALL_eLife$revtrial
#revs[which(sALL_eLife$revtrial==1)]<-0
#revs[which(sALL_eLife$revtrial==1)+1]<-1
#sALL_eLife$revtrial <- revs


# Check correlation within-sets:
cor(sALL[sALL$order=='A2','response'][1:80],sALL[sALL$order=='A2','response'][81:160])
cor(sALL_eLife[sALL_eLife$order=='A1','response'][1:80],sALL_eLife[sALL_eLife$order=='A1','response'][81:160])


# Check correlation between-sets:
cor(sALL_eLife[sALL_eLife$order=='A2','response'][1:80],sALL[sALL$order=='A1','response'][1:80])
cor(sALL_eLife[sALL_eLife$order=='B1','response'][1:80],sALL[sALL$order=='A2','response'][1:80]) # A2 is equivalent to Bx in sALL_eLife




save('sALL_eLife', 'sALL', file='/Users/alebedev/GitHub/ILDPII/Pilot_2/sALL_final.rda')


