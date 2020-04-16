

# Pupillometry:
load('/Users/alebedev/Documents/Projects/HUD/pupillometry/HUD_RALT_pupillometry_df.rda')
ids <- unique(HUD_RALT_pupillometry_df$subject)
respCheck <- data.frame(subject=ids, tv=NA, pv=NA)

for (i in 1:dim(respCheck)[1]){
  tmp <- HUD_RALT_pupillometry_df[HUD_RALT_pupillometry_df$subject==ids[i],]
  tr1 <- c(tmp[c(1:20, 41:60),])
  tr2 <- c(tmp[c(21:40, 61:80),])
  respCheck$tv[i] <- t.test(c(tr1$response[tr1$stimulus==1 ],tr2$response[tr2$stimulus==2 ]),
                            c(tr1$response[tr1$stimulus==2],tr2$response[tr2$stimulus==1]))$statistic
  respCheck$pv[i] <- t.test(c(tr1$response[tr1$stimulus==1 ],tr2$response[tr2$stimulus==2 ]),
                            c(tr1$response[tr1$stimulus==2 ],tr2$response[tr2$stimulus==1 ]),'greater')$p.value
}
inclSubj <- respCheck$subject[which(respCheck$pv<0.5)]


load('/Users/alebedev/Documents/Projects/HUD/pupillometry/mysamplesHUD_PP.rda')
rhosPP<- rhos
alphaPP<- alpha
load('/Users/alebedev/Documents/Projects/HUD/pupillometry/mysamplesHUD_NP.rda')
rhosNP<- rhos
alphaNP<- alpha

HUD_np <- subset(HUD_RALT_pupillometry_df, HUD_RALT_pupillometry_df$group=='NP')
HUD_pp <- subset(HUD_RALT_pupillometry_df, HUD_RALT_pupillometry_df$group=='PP')



rhosNP<- rhosNP[which(is.element(unique(HUD_np$subject), inclSubj))]
rhosPP<- rhosPP[which(is.element(unique(HUD_pp$subject), inclSubj))]
alphaNP<- alphaNP[which(is.element(unique(HUD_np$subject), inclSubj))]
alphaPP<- alphaPP[which(is.element(unique(HUD_pp$subject), inclSubj))]

t.test(rhosNP, rhosPP)
t.test(alphaNP, alphaPP)



# GSR:
load('/Users/alebedev/Documents/Projects/HUD/gsrTTP_output/HUD_RALT_gsr_df.rda')
ids <- unique(HUD_RALT_gsr_df$subject)
respCheck <- data.frame(subject=ids, tv=NA, pv=NA)

for (i in 1:dim(respCheck)[1]){
  tmp <- HUD_RALT_gsr_df[HUD_RALT_gsr_df$subject==ids[i],]
  tr1 <- c(tmp[c(1:20, 41:60),])
  tr2 <- c(tmp[c(21:40, 61:80),])
  respCheck$tv[i] <- t.test(c(tr1$response[tr1$stimulus==1 ],tr2$response[tr2$stimulus==2 ]),
                            c(tr1$response[tr1$stimulus==2 ],tr2$response[tr2$stimulus==1 ]))$statistic
  respCheck$pv[i] <- t.test(c(tr1$response[tr1$stimulus==1 ],tr2$response[tr2$stimulus==2 ]),
                            c(tr1$response[tr1$stimulus==2],tr2$response[tr2$stimulus==1 ]),'greater')$p.value
}
inclSubj <- respCheck$subject[which(respCheck$pv<0.05)]


load('/Users/alebedev/Documents/Projects/HUD/gsrTTP_output/mysamplesHUD_PP.rda')
rhosPP<- rhos
alphaPP<- alpha
load('/Users/alebedev/Documents/Projects/HUD/gsrTTP_output/mysamplesHUD_NP.rda')
rhosNP<- rhos
alphaNP<- alpha

HUD_np <- subset(HUD_RALT_gsr_df, HUD_RALT_gsr_df$group=='NP')
HUD_pp <- subset(HUD_RALT_gsr_df, HUD_RALT_gsr_df$group=='PP')



rhosNP<- rhosNP[which(is.element(unique(HUD_np$subject), inclSubj))]
rhosPP<- rhosPP[which(is.element(unique(HUD_pp$subject), inclSubj))]
alphaNP<- alphaNP[which(is.element(unique(HUD_np$subject), inclSubj))]
alphaPP<- alphaPP[which(is.element(unique(HUD_pp$subject), inclSubj))]

t.test(rhosNP, rhosPP)
t.test(alphaNP, alphaPP)