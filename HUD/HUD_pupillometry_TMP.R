
library(ggplot2)
library(xlsx)
library(zoo)
library(pracma)


#eyes <- read.delim('/Users/alebedev/GitHub/ILDPII/HUD/pupillometry/RALT_jj Samples.txt')
#eyes <- read.delim('/Users/alebedev/GitHub/ILDPII/HUD/pupillometry/RALT_Alex Samples.txt')
#eyes <- read.delim('/Users/alebedev/GitHub/ILDPII/HUD/pupillometry/RALT_uA1_ENG_AlexNoGSR Samples.txt')
# sub 15, 30, 23, 6, 25  looks odd
#fnum=2 # file number
dlist <- list.files('/Users/alebedev/Documents/Projects/HUD/pupillometry/ET/RALT/', '*.txt')
HUD_RALT_pupillometry_df <- data.frame(matrix(NA, c(length(dlist)*80),7))
colnames(HUD_RALT_pupillometry_df) <- c('subject', 'trial', 'stimulus', 'response', 'shock', 'revtrial', 'order')


for (fnum in 1:length(dlist)){
  eyes <- read.delim(paste('/Users/alebedev/Documents/Projects/HUD/pupillometry/ET/RALT/',dlist[fnum], sep=''))
  order <- strsplit(dlist[fnum], "[[:punct:][:space:]]+")[[1]][2]
  subj <- strsplit(dlist[fnum], "[[:punct:][:space:]]+")[[1]][5]
  
  
  if (order=='iA1') {
    trInfo <- '/Users/alebedev/Documents/Projects/HUD/pupillometry/faces_iA1_ENG.xlsx'
  } else if (order=='iA2') {
    trInfo <- '/Users/alebedev/Documents/Projects/HUD/pupillometry/faces_iA2_ENG.xlsx'
  } else if (order=='iB1') {
    trInfo <- '/Users/alebedev/Documents/Projects/HUD/pupillometry/faces_iB1_ENG.xlsx'
  } else if (order=='iB2') {
    trInfo <- '/Users/alebedev/Documents/Projects/HUD/pupillometry/faces_iB2_ENG.xlsx'
  }
  
  
  #tstarts <- c(which(eyes$Type=='MSG')[1]-10, which(eyes$Type=='MSG'))
  tstarts <- which(eyes$Type=='MSG')+1
  eyes$Time <- as.numeric(as.vector(eyes$Time))
  eyes$L.Mapped.Diameter..mm. <- as.numeric(as.vector(eyes$L.Mapped.Diameter..mm.), options(warn=1))
  eyes$R.Mapped.Diameter..mm. <- as.numeric(as.vector(eyes$R.Mapped.Diameter..mm.), options(warn=1))
  
  eyes$tstamps <- 0
  
  

  
  for(i in 1:c(length(tstarts)-1)){
    eyes$tstamps[tstarts[i]:tstarts[i+1]] <- eyes$Time[tstarts[i]:tstarts[i+1]]-eyes$Time[tstarts[i]]
  }
  i <- length(tstarts)
  eyes$tstamps[tstarts[i]:length(eyes$tstamps)] <- eyes$Time[tstarts[i]:length(eyes$Time)]-eyes$Time[tstarts[i]]
  
  eyes$L.Mapped.Diameter..mm.[eyes$L.Mapped.Diameter..mm.<1 | eyes$L.Mapped.Diameter..mm.>9] <- NA
  eyes$R.Mapped.Diameter..mm.[eyes$R.Mapped.Diameter..mm.<1 | eyes$R.Mapped.Diameter..mm.>9] <- NA
  eyes$L.Mapped.Diameter..mm.[1:length(na.approx(eyes$L.Mapped.Diameter..mm., x=eyes$Time))] <- na.approx(eyes$L.Mapped.Diameter..mm., x=eyes$Time) 
  eyes$R.Mapped.Diameter..mm.[1:length(na.approx(eyes$R.Mapped.Diameter..mm., x=eyes$Time))] <- na.approx(eyes$R.Mapped.Diameter..mm., x=eyes$Time)
  
  #eyes$L.Mapped.Diameter..mm. <- mean(eyes$L.Mapped.Diameter..mm., na.rm = T)+detrend(eyes$L.Mapped.Diameter..mm.)
  #eyes$R.Mapped.Diameter..mm. <- mean(eyes$R.Mapped.Diameter..mm., na.rm = T)+detrend(eyes$R.Mapped.Diameter..mm.)
  eyes$L.Mapped.Diameter..mm. <- detrend(eyes$L.Mapped.Diameter..mm.)
  eyes$R.Mapped.Diameter..mm. <- detrend(eyes$R.Mapped.Diameter..mm.)
  
  trial_info<- read.xlsx2(trInfo,1)
  trial_info$trial <- as.numeric(as.vector(trial_info$trial))
  trial_info$shock <- as.numeric(as.vector(trial_info$shock))
  
  firstCSp <- trial_info$stype[which(trial_info$shock!=0)[1]]
  trial_info$revtrial <- 0
  trial_info$revtrial[which(as.numeric(trial_info$revInst)==2)+1] <- 1
  trial_info$cstype <- NA
  trial_info$cstype[trial_info$stype==firstCSp & ((as.numeric(trial_info$block))%%2==1)] <- 1
  trial_info$cstype[trial_info$stype!=firstCSp & ((as.numeric(trial_info$block))%%2==1)] <- 0
  
  trial_info$cstype[trial_info$stype==firstCSp & ((as.numeric(trial_info$block))%%2!=1)] <- 0
  trial_info$cstype[trial_info$stype!=firstCSp & ((as.numeric(trial_info$block))%%2!=1)] <- 1
  
  trial_info[,c(2, 9,8, 3)]
  
  vv <- rep(NA,length(tstarts))
  for (i in 1:length(tstarts)){vv[i]<-tstarts[i+1]-tstarts[i]}
  
  i=length(vv)
  vv[length(vv)] <-dim(eyes)[1]-tstarts[i]
  bits <- min(vv[-1])
  

  eye_trials <- as.data.frame(eyes)
  eye_trials$bit <- 0
  for (i in 1:length(tstarts)){
    eye_trials$Trial[c(tstarts[i]: (tstarts[i]+vv[i]))] <- i
    eye_trials$bit[which(eye_trials$Trial==i)] <- 1:length(which(eye_trials$Trial==i))
    
  }
  
  eye_trials <- eye_trials[c(tstarts[1]:dim(eye_trials)[1]),]
  
  
  
  eye_trials <- eye_trials[,c('Trial', 'bit', 'L.Mapped.Diameter..mm.','R.Mapped.Diameter..mm.')]
  colnames(eye_trials) <- c('trial', 'bit','eyeL', 'eyeR')
  
  
  eye_trials$trial <- eye_trials$trial-1
  
  eye_trials$shock <- 0
  eye_trials$revtrial <- 0
  eye_trials$cstype <- 0
  eye_trials$stype <- 0
  
  eye_trials$shock[is.element(eye_trials$trial, trial_info$trial[which(trial_info$shock>0)])] <- 1
  eye_trials$revtrial[is.element(eye_trials$trial, trial_info$trial[which(trial_info$revtrial==1)])] <- 1
  eye_trials$cstype[is.element(eye_trials$trial, trial_info$trial[which(trial_info$cstype==1)])] <- 1
  eye_trials$stype[is.element(eye_trials$trial, trial_info$trial[which(trial_info$stype=='A')])] <- 1
  
  eye_trials$eyeLR <- apply(eye_trials[,c('eyeL', 'eyeR')],1,mean)
  eye_trials$bit<-eye_trials$bit/250
  
  #df <- eye_trials
  #df0 <- subset(eye_trials, eye_trials$cstype==0 & eye_trials$shock==0)
  #df1 <- subset(eye_trials, eye_trials$cstype==1 & eye_trials$shock==0)
  
  #ggplot(df,aes(x = bit, y = eyeLR)) + 
  #  geom_smooth(data=df0, color=3, size=3)+geom_line(data=df0, aes(x = bit, y = eyeLR, group=trial), color=3, size=0.1)+
  #  geom_smooth(data=df1, color=2,size=3)+geom_line(data=df1,aes(x = bit, y = eyeLR, group=trial), color=2,size=0.1)+
  #  theme_bw() +
  #  theme(axis.text=element_text(size=20), axis.title.x=element_blank(),
  #        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #        panel.border = element_blank())
  
  finalEYE_df <- data.frame(trial=c(1:max(eye_trials$trial)))
  for (i in 1:max(eye_trials$trial)){
    tmpNow <- subset(eye_trials, eye_trials$trial == i)
    tmpPast <- subset(eye_trials, eye_trials$trial == c(i-1))
    rownames(tmpNow) <- c()
    rownames(tmpPast) <- c()
    finalEYE_df$response[i]<- max(tmpNow$eyeLR[round(bits/100,0):bits], na.rm=T)-mean(tail(tmpPast$eyeLR,2))
  }
  
  finalEYE_df$response <- finalEYE_df$response-min(finalEYE_df$response)
  
  finalEYE_d <- merge(finalEYE_df, trial_info, by='trial')
  finalEYE_d$shock <- finalEYE_d$shock*5
  finalEYE_d$shock[finalEYE_d$shock==0.2]<-1
  finalEYE_d$revtrial <- 0; finalEYE_d$revtrial[which(as.numeric(finalEYE_d$revInst)==2)+1] <- 1
  
  
  finalEYE_d$response <- sqrt(finalEYE_d$response)
  t.test(finalEYE_d$response[finalEYE_d$cstype=='0' & finalEYE_d$shock!=1],finalEYE_d$response[finalEYE_d$cstype=='1' & finalEYE_d$shock!=1])
  
  boxplot(finalEYE_d$response[finalEYE_d$cstype=='0' & finalEYE_d$shock!=1],finalEYE_d$response[finalEYE_d$cstype=='1' & finalEYE_d$shock!=1], xaxt='n', main=paste('subject', subj))
  points(cbind(jitter(rep(1,length(finalEYE_d$response[finalEYE_d$cstype=='0' & finalEYE_d$shock!=1]))),finalEYE_d$response[finalEYE_d$cstype=='0' & finalEYE_d$shock!=1]), cex=2)
  points(cbind(jitter(rep(2,length(finalEYE_d$response[finalEYE_d$cstype=='1' & finalEYE_d$shock!=1]))),finalEYE_d$response[finalEYE_d$cstype=='1' & finalEYE_d$shock!=1]), cex=2, pch=2)
  axis(1, at=1:2, labels=c('CS-', 'CS+'))
  
  
  finalEYE_d$stimulus <- as.numeric(finalEYE_d$stype)
  finalEYE_d$subject <- subj
  finalEYE_d$order <- order
  
  HUD_RALT_pupillometry_df[c((80*fnum)-79):(80*fnum),] <- finalEYE_d[,c('subject', 'trial', 'stimulus', 'response', 'shock', 'revtrial', 'order')]
}

# Study ID-key:
load('/Users/alebedev/Documents/Projects/HUD/HUD.rda')
esids <- read.xlsx2('/Users/alebedev/Documents/Projects/HUD/visits/tested.xlsx', 1)
esids$email <- gsub(" ","",tolower(esids$email))
esids$email <- gsub(";","",tolower(esids$email))
tmp <- merge(SCRFU_df, esids, by='email')
tmp <- tmp[,c('ID.y', 'group')]
tmp <- tmp[!duplicated(tmp$ID),]
colnames(tmp)[1] <- 'ID'
for (i in 1:dim(tmp)[1]){
  tmp$subject[i] <- strsplit(as.vector(tmp$ID),'sub-')[[i]][2]
}

HUD_RALT_pupillometry_df <- merge(HUD_RALT_pupillometry_df,tmp[,c('subject', 'group')], by='subject')
save(HUD_RALT_pupillometry_df,file='/Users/alebedev/Documents/Projects/HUD/pupillometry/HUD_RALT_pupillometry_df.rda')





