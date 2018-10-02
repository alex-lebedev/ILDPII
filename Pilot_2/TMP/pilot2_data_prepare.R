# Load libraries:

library(R.matlab)
library(RSEIS)
library(xlsx)
library(ggplot2)

rdat <- readMat('/Users/alebedev/GitHub/ILDPII/Pilot_2/ACQ/sub-00016-2018-09-05.acq.mat')$channels[2][[1]][[1]][[1]]
shock <- readMat('/Users/alebedev/GitHub/ILDPII/Pilot_2/ACQ/sub-00016-2018-09-05.acq.mat')$channels[3][[1]][[1]][[1]]

# s14 - A2ins gsr<-rdat[1,100000:dim(rdat)[2]]; gsr_d <- detrend(gsr); shock<-shock[1,100000:dim(shock)[2]]; 
# s15 - A2unins
# s16 - A1ins
# s17 - A2ins

#endp <- which(rdat[,1]< c(mean(rdat[,1]) - 3*sd(rdat[,1])))[1]-500
#rdat <- rdat[1:endp,]
gsr <- rdat[1,]
gsr_d <- detrend(gsr)
shock <- shock[1,]

d <- as.data.frame(cbind(gsr, gsr_d,shock))

#ts.plot(d,gpars=list(yaxt='n', col=c(1:3)))
d$ms <- as.numeric(as.vector(row.names(d)))
# show data with two shock trials:
#ts.plot(d[c(16000*5):c(16000*9),2:3],gpars=list(yaxt='n', col=c(1:2)))

timing <- read.xlsx2('/Users/alebedev/GitHub/ILDPII/Pilot_2/pilot2_TaskDesign.xlsx',1)

timing$trial <- as.numeric(as.vector(timing$trial))
timing$shockstart <- as.numeric(as.vector(timing$shockstart))
timing$shock <- as.numeric(as.vector(timing$shock))

timing_long <- data.frame(trial=rep(NA,max(timing$trial)*16000+16000))
timing_long$shock <- NA

for (i in 1:max(timing$trial)){
  tmp <- subset(timing, timing$trial == i)
  stamp <- (tmp$trial-1)*16000+1
  timing_long$trial[stamp:c(stamp+16000)] <- i
  if (tmp$shock==0.2){
    timing_long$shock[c(stamp+tmp$shockstart*1000):c(stamp+tmp$shockstart*1000+200)] <- 1
    }
}
timing_long <- timing_long[!is.na(timing_long$trial),]
timing_long$ms <- as.numeric(as.vector(row.names(timing_long)))
timing_long$shock[is.na(timing_long$shock)]<-0

# max(timing$shockstart)*2
#i=41; tmp <- subset(dd, dd$trial == i); ts.plot(tmp$gsr)



# Check between CS+ timing (should be roughly 31.78 seconds):
(d$ms[which(d$shock>0)[185]]-d$ms[which(d$shock>0)[1]])/1000
(16*7+5.12)-(16*5+5.34)
(timing_long$ms[which(timing_long$shock>0)[202]]-timing_long$ms[which(timing_long$shock>0)[1]])/1000

# ms until the first electric shock (theory):
timing_long$ms[which(timing_long$shock>0)[1]]
# ms until the first electric shock (practice: actual gsr data):
d$ms[which(d$shock>0)[1]]

# Select based on timing model:

stamp1 <- d$ms[which(d$shock>0)[1]]-timing_long$ms[which(timing_long$shock>0)[1]]
d_sel <- d[stamp1:dim(d)[1],]

d_sel$ms <- as.numeric(c(1:dim(d_sel)[1]))

dd <- merge(d_sel, timing_long, by='ms')
dd$gsr_d<- dd$gsr_d-min(dd$gsr_d)

cor(dd$shock.x,dd$shock.y)
#ts.plot(cbind(dd$shock.x,dd$shock.y),gpars=list(col=c(1:2)))


final_df <- data.frame(trial=c(1:max(dd$trial)))

for (i in 1:max(dd$trial)){
  tmp <- subset(dd, dd$trial == i)
  final_df$gsr[i]<- max(tmp$gsr[2000:16000])-min(tmp$gsr[1:5000])
  final_df$gsr_d[i]<- max(tmp$gsr_d[2000:16000])-min(tmp$gsr_d[1:5000])
}



final_d <- merge(final_df, timing, by='trial')
final_d$cs <- 0
final_d$shock[final_d$shock==0.2]<-1; final_d$revtrial <- 0;
final_d$revtrial[which(final_d$revInst=='Sambandet mellan stimulit och elstötar kommer nu att omvändas (Tryck MELLANSLAG)')+1]<-1

s16 <- data.frame(subject=16,
                  trial=final_d$trial,stimulus=as.numeric(final_d$stype),
                  response=sqrt(final_d$gsr), shock=final_d$shock,
                  revtrial=final_d$revtrial)

sALL <- rbind(s14,s16,s17)
save(sALL, file='/Users/alebedev/GitHub/ILDPII/Pilot_2/sALL_instructed.rda')


subjList <- unique(sALL[,'subject'])  # list of subjects x blocks
numSubjs <- length(subjList)  # number of subjects
maxTrials <- max(sALL$trial)
Tsubj <- as.vector(rep(0, numSubjs))
response <- array(0, c(numSubjs, maxTrials) )
stimulus <- array(0, c(numSubjs, maxTrials) )
shock <- array(0, c(numSubjs, maxTrials) )
revtrial <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  Tsubj[i] <- sum(sALL$subject == curSubj)  # Tsubj[N]
  useTrials    <- Tsubj[i]
  tmp          <- subset(sALL, sALL$subject == curSubj)
  response[i, 1:useTrials]   <- tmp$response
  stimulus[i, 1:useTrials]   <- tmp$stimulus
  shock[i, 1:useTrials]   <- tmp$shock
  revtrial[i, 1:useTrials]   <- tmp$revtrial
}

dataList <- list(
  N             = numSubjs,
  T             = maxTrials,
  response      = response,
  stimulus      = stimulus,
  shock         = shock,
  Tsubj         = Tsubj,
  revtrial      = revtrial
)










final_d$cs[is.element(final_d$block,c('block1','block3')) & final_d$face=='stim/FaceA.JPG']<-1
final_d$cs[is.element(final_d$block,c('block2','block4')) & final_d$face=='stim/FaceB.JPG']<-1
t.test(final_d$gsr[final_d$cs==0],final_d$gsr[final_d$cs==1])
boxplot(final_d$gsr[final_d$cs==0],final_d$gsr[final_d$cs==1])


# Ploting responses per trial:
final_d$cs <- as.factor(final_d$cs)
ggplot(final_d) + geom_point(aes(x = cs, y = gsr, color=cs), size = 1, data=final_d)+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Time-series plot of individual responses:
ts.plot(cbind(final_d$gsr, as.numeric(final_d$block),as.numeric(final_d$cs)*3, (as.numeric(final_d$fix)-1)*10), gpars=list(yaxt='n', col=c(1:4)))


ddd <- merge(dd, final_d[,c('trial','stype', 'block', 'face', 'shock', 'shockstart', 'fix', 'cs')], by='trial')
ddd$timing <- c(rep(1:16000,80),1)

d <- aggregate(. ~ timing+cs, ddd[,], mean)

ts.plot(cbind(d$gsr_d[d$cs==0],d$gsr_d[d$cs==1]), gpars=list(col=c(1:2)))
  

ddd$cs <- as.factor(as.numeric(as.vector((ddd$cs)))+1)

ddd1 <- ddd

ggplot(ddd1) + 
  geom_point(aes(x = timing, y = gsr_d, color=cs), size = 1, data=ddd1) +
  stat_smooth(aes(x = timing, y = gsr_d, color=cs, method = "loess"), size=1, se=T, span=0.95, data=ddd1) +
#  coord_cartesian(ylim = c(0.1, 0.23)) + 
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())




ddd1 <- d

ddd1$cs <- as.factor(ddd1$cs)
ggplot(ddd1) + 
  #geom_point(aes(x = timing, y = gsr_d, color=cs), size = 1, data=ddd1) +
  
  stat_smooth(aes(x = timing, y = gsr_d, color=cs, method = "loess"), size=1, se=T, span=0.95, data=ddd1) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())




