# ILDPII_Pilot2_main_plots.R
#
# Author: Alexander V. Lebedev
# Date: 2018-11-06
# Testing models on different data sets

##############
# E-life data:
##############

#############
# Plotting: #
#############

# All 80:
load( '/Users/alebedev/GitHub/ILDPII/Pilot_2/mysamplesUNINST_EL.rda')
load('/Users/alebedev/GitHub/ILDPII/Pilot_2/mysamplesINST_EL.rda')
load('/Users/alebedev/GitHub/ILDPII/Pilot_2/mysamplesINST_IL.rda')
load('/Users/alebedev/GitHub/ILDPII/Pilot_2/mysamplesUNINST_IL.rda')


# Alpha:
boxplot(apply(extract(mysamplesINST_EL)$A,2,mean), apply(extract(mysamplesUNINST_EL)$A,2,mean), ylim=c(0,0.2), col='lightgrey',cex.axis=2);
points(jitter(rep(1,rep(length(apply(extract(mysamplesINST_EL)$A,2,mean))))), apply(extract(mysamplesINST_EL)$A,2,mean), pch=16, col='black', cex=3)
points(jitter(rep(2,rep(length(apply(extract(mysamplesUNINST_EL)$A,2,mean))))), apply(extract(mysamplesUNINST_EL)$A,2,mean), pch=17, col='black', cex=3)

points(jitter(rep(1.5,rep(length(apply(extract(mysamplesINST_IL)$A,2,mean))))), apply(extract(mysamplesINST_IL)$A,2,mean), pch=1, col='black', cex=3)
points(jitter(rep(2.5,rep(length(apply(extract(mysamplesUNINST_IL)$A,2,mean))))), apply(extract(mysamplesUNINST_IL)$A,2,mean), pch=2, col='black', cex=3)


# EVinits 1,2 (Instructed)
boxplot(apply(extract(mysamplesINST_EL)$EVinit1,2,mean), apply(extract(mysamplesINST_EL)$EVinit2,2,mean), ylim=c(0,1), col='lightgrey',cex.axis=2);
points(jitter(rep(1,rep(length(apply(extract(mysamplesINST_EL)$EVinit1,2,mean))))), apply(extract(mysamplesINST_EL)$EVinit1,2,mean), pch=16, col='black', cex=3)
points(jitter(rep(2,rep(length(apply(extract(mysamplesINST_EL)$EVinit2,2,mean))))), apply(extract(mysamplesINST_EL)$EVinit2,2,mean), pch=17, col='black', cex=3)

points(jitter(rep(1.5,rep(length(apply(extract(mysamplesINST_IL)$EVinit1,2,mean))))), apply(extract(mysamplesINST_IL)$EVinit1,2,mean), pch=1, col='black', cex=3)
points(jitter(rep(2.5,rep(length(apply(extract(mysamplesINST_IL)$EVinit2,2,mean))))), apply(extract(mysamplesINST_IL)$EVinit2,2,mean), pch=2, col='black', cex=3)

# EVinits 1,2 (Uninstructed)
boxplot(apply(extract(mysamplesUNINST_EL)$EVinit1,2,mean), apply(extract(mysamplesUNINST_EL)$EVinit2,2,mean), ylim=c(0,1), col='lightgrey',cex.axis=2);
points(jitter(rep(1,rep(length(apply(extract(mysamplesUNINST_EL)$EVinit1,2,mean))))), apply(extract(mysamplesUNINST_EL)$EVinit1,2,mean), pch=16, col='black', cex=3)
points(jitter(rep(2,rep(length(apply(extract(mysamplesUNINST_EL)$EVinit2,2,mean))))), apply(extract(mysamplesUNINST_EL)$EVinit2,2,mean), pch=17, col='black', cex=3)

points(jitter(rep(1.5,rep(length(apply(extract(mysamplesUNINST_IL)$EVinit1,2,mean))))), apply(extract(mysamplesUNINST_IL)$EVinit1,2,mean), pch=1, col='black', cex=3)
points(jitter(rep(2.5,rep(length(apply(extract(mysamplesUNINST_IL)$EVinit2,2,mean))))), apply(extract(mysamplesUNINST_IL)$EVinit2,2,mean), pch=2, col='black', cex=3)






boxplot(rhos_uninst,rhos_inst, ylim=c(0, 1),cex.axis=2)
points(jitter(rep(1,rep(length(rhos_uninst)))), rhos_uninst, pch=16, col=2, cex=3)
points(jitter(rep(2,rep(length(rhos_inst)))), rhos_inst, pch=16, col=3, cex=3)

points(jitter(rep(1.5,rep(length(rhos_uninst_il)))), rhos_uninst_il, pch=2, col=2, cex=3)
points(jitter(rep(2.5,rep(length(rhos_inst_il)))), rhos_inst_il, pch=2, col=3, cex=3)



# Only forst 40:
load( '/Users/alebedev/GitHub/ILDPII/Pilot_2/mysamplesUNINST_EL_first40.rda')
load('/Users/alebedev/GitHub/ILDPII/Pilot_2/mysamplesINST_EL_first40.rda')
load('/Users/alebedev/GitHub/ILDPII/Pilot_2/mysamples1REV.rda')
load('/Users/alebedev/GitHub/ILDPII/Pilot_2/mysamplesINST_IL_first40.rda')
load('/Users/alebedev/GitHub/ILDPII/Pilot_2/mysamplesUNINST_IL_first40.rda')

boxplot(rhos_uninst,rhos_inst, rhos_1rev, ylim=c(0, 1),cex.axis=2)
points(jitter(rep(1,rep(length(rhos_uninst)))), rhos_uninst, pch=16, col=2, cex=3)
points(jitter(rep(2,rep(length(rhos_inst)))), rhos_inst, pch=16, col=3, cex=3)
points(jitter(rep(3,rep(length(rhos_1rev)))), rhos_1rev, pch=16, col=4, cex=3)

# GGPLOTS:

# Averaged GSR:
load('/Users/alebedev/GitHub/ILDPII/Pilot_2/sALL_final.rda');

df <- subset(sALL_eLife, sALL_eLife$instr=='X')
df <- subset(df, df$shock!=1)

df <- subset(df, df$order=='A1' | df$order=='A2')
df <- aggregate(df[, c('response', 'stimulus')], list(df$trial), mean)

df1 <- subset(df, df$stimulus==1)
df2 <- subset(df, df$stimulus==2)

ggplot(df,aes(x = Group.1, y = response)) + 
  geom_point(data=df1, aes(x = Group.1, y = response), size=3, color=2) +geom_line(data=df1, color=2, size=1.5)+
  geom_point(data=df2, aes(x = Group.1, y = response),size=3, color=3) + geom_line(data=df2, color=3,size=1.5)+
  theme_bw() +
  theme(axis.text=element_text(size=20), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank())


# Predicted from model:
load('/Users/alebedev/GitHub/ILDPII/Pilot_2/EVs_EL.rda')
ev1 <- as.data.frame(EV1_Ains); ev1$type <- 1; ev1$trial <- 1:dim(ev1)[1]; colnames(ev1)[1] <- 'EV'
ev2 <- as.data.frame(EV2_Ains); ev2$type <- 2; ev2$trial <- 1:dim(ev2)[1]; colnames(ev2)[1] <- 'EV'

df <- as.data.frame(rbind(ev1,ev2))
df$stimulus <- stimulus

ggplot(df,aes(x = trial, y = EV)) + 
  geom_point(data=ev1, aes(x = trial, y = EV), size=3, color=2) +geom_line(data=ev1, color=2, size=1.5)+
  geom_point(data=ev2, aes(x = trial, y = EV),size=3, color=3) + geom_line(data=ev2, color=3,size=1.5)+
  theme_bw() +
  theme(axis.text=element_text(size=20), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank())
