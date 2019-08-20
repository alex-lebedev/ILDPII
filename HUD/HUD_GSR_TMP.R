library(R.matlab)
library(RSEIS)
library(xlsx)
library(ggplot2)

rdat <- readMat('/Users/alebedev/GitHub/ILDPII/HUD/GSR/sub-Jesper-2019-01-11.acq.mat')$channels[2][[1]][[1]][[1]]
shock <- readMat('/Users/alebedev/GitHub/ILDPII/HUD/GSR/sub-Jesper-2019-01-11.acq.mat')$channels[3][[1]][[1]][[1]]
tstarts <- readMat('/Users/alebedev/GitHub/ILDPII/HUD/GSR/sub-Jesper-2019-01-11.acq.mat')$channels[4][[1]][[1]][[1]]
tstartFirst <- which(tstarts==5)[1]
tstartLast <- which(tstarts==5)[length(which(tstarts==5))]
tdur <- 12000



# s14 - A2ins gsr<-rdat[1,100000:dim(rdat)[2]]; gsr_d <- detrend(gsr); shock<-shock[1,100000:dim(shock)[2]]; 
# s15 - A2unins
# s16 - A1ins
# s17 - A2ins

#endp <- which(rdat[,1]< c(mean(rdat[,1]) - 3*sd(rdat[,1])))[1]-500
#rdat <- rdat[1:endp,]
gsr <- rdat[1,]
gsr_d <- detrend(gsr)
shock <- shock[1,]

d <- data.frame(gsr=gsr, gsr_d=gsr_d[,1],shock=shock,tstarts=tstarts[1,])

d <- d[tstartFirst:(tstartLast+tdur),]

# Plot:
#ts.plot(d[,2:4], col=c(2:4))

#ts.plot(d,gpars=list(yaxt='n', col=c(1:3)))
d$ms <- as.numeric(as.vector(row.names(d)))
# show data with two shock trials:
#ts.plot(d[c(16000*5):c(16000*9),2:3],gpars=list(yaxt='n', col=c(1:2)))

trial_info <- read.xlsx2('/Users/alebedev/GitHub/ILDPII/HUD/pupillometry/faces_iA1_1rev_SWE.xlsx',1) 

trial_info$trial <- as.numeric(as.vector(trial_info$trial))
trial_info$shockstart <- as.numeric(as.vector(trial_info$shockstart))
trial_info$shock <- as.numeric(as.vector(trial_info$shock))

timing_long <- data.frame(trial=rep(NA,max(trial_info$trial)*tdur+tdur))
timing_long$shock <- NA

for (i in 1:max(trial_info$trial)){
  tmp <- subset(trial_info, trial_info$trial == i)
  stamp <- (tmp$trial-1)*tdur+1
  
  timing_long$trial[stamp:c(stamp+tdur)] <- i
  if (tmp$shock==0.2){
    timing_long$shock[c(stamp+tmp$shockstart*1000):c(stamp+tmp$shockstart*1000+200)] <- 1
  }
}
timing_long <- timing_long[!is.na(timing_long$trial),]
timing_long$shock[is.na(timing_long$shock)]<-0

d <- d[1:(tdur*max(trial_info$trial)),]
timing_long <- timing_long[1:(tdur*max(trial_info$trial)),]


timing_long$gsr <- d$gsr
rownames(timing_long) <- c()


timing_long$bit <- rep(c(1:tdur), max(trial_info$trial))
timing_long$shock <- 0
timing_long$revtrial <- 0
timing_long$cstype <- 0
timing_long$stype <- 0

timing_long$shock[is.element(timing_long$trial, trial_info$trial[which(trial_info$shock>0)])] <- 1
timing_long$revtrial[is.element(timing_long$trial, trial_info$trial[which(trial_info$revtrial==1)])] <- 1
timing_long$cstype[is.element(timing_long$trial, trial_info$trial[which(trial_info$cstype=='cs+')])] <- 1
timing_long$stype[is.element(timing_long$trial, trial_info$trial[which(trial_info$stype=='A')])] <- 1



df <- timing_long
df0 <- subset(timing_long, timing_long$cstype==0 & timing_long$shock==0)
df1 <- subset(timing_long, timing_long$cstype==1 & timing_long$shock==0)


ggplot(df,aes(x = bit, y = gsr)) + 
  geom_smooth(data=df0, color=3, size=1.5)+
  geom_smooth(data=df1, color=2,size=1.5)+
  theme_bw() +
  theme(axis.text=element_text(size=20), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank())


final_df <- data.frame(trial=c(1:max(timing_long$trial)))
for (i in 1:max(timing_long$trial)){
  tmp <- subset(timing_long, timing_long$trial == i)
  rownames(tmp) <- c()
  final_df$gsr[i]<- max(tmp$gsr[2000:tdur])-min(tmp$gsr[1:5000])
}


final_d <- merge(final_df, trial_info, by='trial')
final_d$shock[final_d$shock==0.2]<-1; final_d$revtrial <- 0;
final_d$revtrial[which(final_d$revInst=='Sambandet mellan stimulit och elstötar kommer nu att omvändas (Tryck MELLANSLAG)')+1]<-1

finalGSR_d <- final_d
finalGSR_d$stimulus <- as.numeric(finalGSR_d$stype)

save(finalGSR_d,file='/Users/alebedev/GitHub/ILDPII/HUD/pupillometry/finalGSR_d.rda')

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

