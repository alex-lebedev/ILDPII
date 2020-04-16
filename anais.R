# ANAIS data:

library(xlsx)
library(R.matlab)

ildp <- read.xlsx('~/Downloads/data_extinction.xlsx', 1)[,c('ID', 'Group', 'NICS.DIFF', 'PDItot','PDIdistressNorm',	'PDIpreoccNorm',	'PDIconvNorm')]
ildp$NICS.DIFF.LOG <- log1p(ildp$NICS.DIFF+50)
ildp <- ildp[order(ildp$ID),]
IDs <- data.frame(ID=dir('/Volumes/REBOOTII/ANAIS/ANALYSIS/Behaviour/DATA/ALL/'))

IDs$ID <- as.vector(IDs$ID)
IDs$ID <- as.numeric(strsplit(IDs$ID,'.nii'))


dat <- merge(IDs, ildp, by=c('ID'), sort=F)
dat <- dat[order(dat$Group),]
dat$path <- (paste('/Volumes/REBOOTII/ANAIS/ANALYSIS/Behaviour/DATA/ALL/',dat$ID, '.nii,1', sep=''))

writeMat('/Volumes/REBOOTII/ANAIS/ANALYSIS/Behaviour/beh.mat', nics=dat$NICS.DIFF, pdi=dat$PDItot, nicsLog=dat$NICS.DIFF.LOG,
         nicsCON=dat$NICS.DIFF[dat$Group=='control'], pdiCON=dat$PDItot[dat$Group=='control'],
         nicsHDP=dat$NICS.DIFF[dat$Group=='HDP'], pdiHDP=dat$PDItot[dat$Group=='HDP'])







library(xlsx)
library(R.matlab)
library(ggplot2)


rois <- as.data.frame(readMat('/Volumes/REBOOTII/ANAIS/ANALYSIS/ConsExtract/rois.mat')$rois)
colnames(rois) <- c('mpfc','fear','amygdala')

ildp <- read.xlsx('~/Downloads/data_extinction.xlsx', 1)[,c('ID', 'Group', 'NICS.DIFF', 'PDItot','PDIdistressNorm',	'PDIpreoccNorm',	'PDIconvNorm')]
ildp$NICS.DIFF.LOG <- log1p(ildp$NICS.DIFF+50)
ildp <- ildp[order(ildp$ID),]
IDs <- data.frame(ID=dir('/Volumes/REBOOTII/ANAIS/ANALYSIS/ConsExtract/con13'))


dat <- merge(IDs, ildp, by=c('ID'), sort=F)
dat <- cbind(dat, rois)
summary(glm(mpfc~Group, data=dat))


#Plotting:
library(dplyr)

dat_summary <- dat %>% # the names of the new data frame and the data frame to be summarised
  group_by(Group) %>%   # the grouping variable
  summarise(mean_PL = mean(mpfc),  # calculates the mean of each group
            sd_PL = sd(mpfc), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(mpfc)/sqrt(n())) # calculates the standard error of each group
datPlot <- ggplot(dat_summary, aes(Group, mean_PL)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = mean_PL - SE_PL, ymax = mean_PL + SE_PL), width=0.2)
datPlot + labs(y="Contrast magnitude (NICS+ > NICS-)", x = "Group") + theme_classic()




rois <- as.data.frame(readMat('/Volumes/REBOOTII/ANAIS/ANALYSIS/ConsExtract/rois2.mat')$rois)
colnames(rois) <- c('fear','amygdala')
ildp <- read.xlsx('~/Downloads/data_extinction.xlsx', 1)[,c('ID', 'Group', 'NICS.DIFF', 'PDItot','PDIdistressNorm',	'PDIpreoccNorm',	'PDIconvNorm')]
ildp <- ildp[order(ildp$ID),]
IDs <- data.frame(ID=dir('/Volumes/REBOOTII/ANAIS/ANALYSIS/ConsExtract/con13'))
dat <- merge(IDs, ildp, by=c('ID'), sort=F)
dat <- cbind(dat, rois)
# Extinction ~ FearNWK-response in groups:
summary(glm(c(dat$amygdala+dat$fear)~dat$NICS.DIFF*dat$Group))
summary(glm(dat$NICS.DIFF[dat$Group=='control']~c(dat$fear[dat$Group=='control']+dat$amygdala[dat$Group=='control'])))
summary(glm(dat$NICS.DIFF[dat$Group=='HDP']~c(dat$fear[dat$Group=='HDP']+dat$amygdala[dat$Group=='HDP'])))

# Plot:
plot(dat$fear+dat$amygdala,dat$NICS.DIFF, type='n')
points(c(dat$fear[dat$Group=='HDP']+dat$amygdala[dat$Group=='HDP']),dat$NICS.DIFF[dat$Group=='HDP'], pch=17, cex=5, col='darkred')
abline(summary(glm(dat$NICS.DIFF[dat$Group=='HDP']~c(dat$fear[dat$Group=='HDP']+dat$amygdala[dat$Group=='HDP']))), lwd=7, col='darkred')
points(c(dat$fear[dat$Group=='control']+dat$amygdala[dat$Group=='control']),dat$NICS.DIFF[dat$Group=='control'], pch=16, cex=5, col='darkblue')
abline(summary(glm(dat$NICS.DIFF[dat$Group=='control']~c(dat$fear[dat$Group=='control']+dat$amygdala[dat$Group=='control']))), lwd=7, col='darkblue')
cor(dat$NICS.DIFF[dat$Group=='control'],dat$fear[dat$Group=='control']+dat$amygdala[dat$Group=='control'])
cor(dat$NICS.DIFF[dat$Group=='HDP'],dat$fear[dat$Group=='HDP']+dat$amygdala[dat$Group=='HDP'])


#
library(xlsx)
library(R.matlab)
library(corrplot)

ildp <- read.xlsx('~/Downloads/data_extinction.xlsx', 1)[,c('ID', 'Group', 'NICS.DIFF', 'PDItot','PDIdistressNorm',	'PDIpreoccNorm',	'PDIconvNorm')]
ildp$NICS.DIFF.LOG <- log1p(ildp$NICS.DIFF+50)
ildp <- ildp[order(ildp$ID),]
IDs <- data.frame(ID=dir('/Volumes/REBOOTII/ANAIS/FunImg/'))
IDs$ID <- as.vector(IDs$ID)
IDs$ID <- as.numeric(strsplit(IDs$ID,'.nii'))
dat <- merge(IDs, ildp, by=c('ID'), sort=F)

#dat$path <- (paste('/Volumes/REBOOTII/ANAIS/ANALYSIS/Behaviour/DATA/ALL/',dat$ID, '.nii,1', sep=''))

CONNECT <- readMat('/Volumes/REBOOTII/ANAIS/ANALYSIS/connectivityBlocks.mat')
conn <- data.frame(ID=CONNECT$nam)
conn <- merge(conn, dat, by='ID')


mtx <- CONNECT$mNICSP-CONNECT$mNICSM
#mtx <- CONNECT$mNICSP

mHC <- apply(mtx[,,conn$Group=='control'],c(1,2),mean)
mHDP <- apply(mtx[,,conn$Group=='HDP'],c(1,2),mean)

colnames(mHC) <- c('Amyg', 'Fear', 'LOFC', 'MPFC')
#colnames(mHC) <- c(1,2,3,4)

rownames(mHC) <- colnames(mHC)
colnames(mHDP) <- colnames(mHC) 
rownames(mHDP) <- rownames(mHC)

mHCp <- mHC
mHDPp <- mHDP


#col2 <- colorRampPalette(c("white","white","white","white","white","yellow","yellow","#FF7F00","#FF7F00","red","red","#7F0000","#7F0000"))


#  corrplot(mtcars_cor, method="shade", shade.col=NA, tl.col="black", tl.srt=45, col=col(200), addCoef.col="black", addcolorlabel="no", order="AOE")
#corrplot(mHDP-mHC, col=col1(30),insig='blank', method='shade', type='full',diag=T,addgrid.col='black',tl.cex=2, tl.col='black')

pvHDP <- mHDP
ptHDP <- mHDP
pvHC <- mHDP
ptHC <- mHDP
pvDIFF <- mHDP
ptDIFF <- mHDP

for (n in 1:4){
  for (m in 1:4) {
    if(n!=m){
      pvHDP[n,m] <- t.test(mtx[n,m,conn$Group=='HDP'])$p.val
      ptHDP[n,m] <- t.test(mtx[n,m,conn$Group=='HDP'])$stat*(-1)
      pvHC[n,m] <- t.test(mtx[n,m,conn$Group=='control'])$p.val
      ptHC[n,m] <- t.test(mtx[n,m,conn$Group=='control'])$stat*(-1)
      pvDIFF[n,m] <- t.test(mtx[n,m,conn$Group=='control'],mtx[n,m,conn$Group=='HDP'])$p.val
      ptDIFF[n,m] <- t.test(mtx[n,m,conn$Group=='control'],mtx[n,m,conn$Group=='HDP'])$stat*(-1)
    } else pvm[n,m] <- 1
  }
}

mDIFF <- mHDP-mHC
mDIFF[pvDIFF>0.05]<-0


mHC[pvHC>0.05]<-0
mHDP[pvHDP>0.05]<-0

par(mfrow=c(1,3))
col1 <- colorRampPalette(c("darkblue","blue","blue","white","white","yellow","red"))
corrplot(mHC, col=col1(8),insig='blank', method='shade', type='full',diag=T,addgrid.col='black',tl.cex=2, tl.col='black')
corrplot(mHDP, col=col1(8),insig='blank', method='shade', type='full',diag=T,addgrid.col='black',tl.cex=2, tl.col='black')
corrplot(mDIFF, col=col2(15),insig='blank', method='shade', type='full',diag=T,addgrid.col='black',tl.cex=2, tl.col='black')


r1=1
r2=4
t.test(mtx[r1,r2,conn$Group=='control'],mtx[r1,r2,conn$Group=='HDP'])

r1=2
r2=4
t.test(mtx[r1,r2,conn$Group=='control'],mtx[r1,r2,conn$Group=='HDP'])


r1=2
r2=4
summary(glm((mtx[r1,r2,]~conn$NICS.DIFF*conn$Group)))
mINT[r1,r2] <- summary(glm((mtx[r1,r2,]~conn$NICS.DIFF*conn$Group)))$coefficients[4,1]


# Plot:
par(mfrow=c(1,1))

plot(mtx[r1,r2,],conn$NICS.DIFF, type='n')

points(mtx[r1,r2,][conn$Group=='HDP'],conn$NICS.DIFF[dat$Group=='HDP'], pch=17, cex=5, col='darkred')
abline(summary(glm(conn$NICS.DIFF[dat$Group=='HDP']~mtx[r1,r2,][conn$Group=='HDP'])), lwd=7, col='darkred')
points(mtx[r1,r2,][conn$Group=='control'],conn$NICS.DIFF[dat$Group=='control'], pch=16, cex=5, col='darkblue')
abline(summary(glm(conn$NICS.DIFF[dat$Group=='control']~mtx[r1,r2,][conn$Group=='control'])), lwd=7, col='darkblue')

cor(mtx[r1,r2,][conn$Group=='control'],conn$NICS.DIFF[dat$Group=='control'])
cor(mtx[r1,r2,][conn$Group=='HDP'],conn$NICS.DIFF[dat$Group=='HDP'])
summary(glm((mtx[r1,r2,][conn$Group=='HDP']~conn$NICS.DIFF[dat$Group=='HDP'])))





#cor(mtx[1,4,conn$Group=='control'],conn$NICS.DIFF[conn$Group=='control'])



#r1=1
#r2=4
#par(mfrow=c(1,1))
#plot(mtx[r1,r2,],conn$NICS.DIFF, pch=1, col='blue', type='n')
#points(mtx[r1,r2,conn$Group=='control'],conn$NICS.DIFF[conn$Group=='control'], pch=1, col='blue')

#abline(summary(glm((conn$NICS.DIFF[conn$Group=='control']~mtx[r1,r2,conn$Group=='control']))), pch=2, col='blue')
#points(mtx[r1,r2,conn$Group=='HDP'],conn$NICS.DIFF[conn$Group=='HDP'], pch=2, col='red')
#abline(summary(glm((conn$NICS.DIFF[conn$Group=='HDP']~mtx[r1,r2,conn$Group=='HDP']))), pch=2, col='red')


mINT <- mHDP
mINTp <- mHDP

for (n in 1:4){
  for (m in 1:4) {
    if(n!=m){
      mINT[n,m] <- summary(glm((mtx[n,m,]~conn$NICS.DIFF*conn$Group)))$coefficients[4,1]
      mINTp[n,m] <- summary(glm((mtx[n,m,]~conn$NICS.DIFF*conn$Group)))$coefficients[4,4]
    }
  }
}




par(mfrow=c(1,1))
corrplot(mINT*20, col=col1(10),insig='p-value', method='shade', type='full',diag=T,addgrid.col='black',tl.cex=2, tl.col='black')


corrplot(mINT, col=col1(20),insig='p-value', method='shade', type='full',diag=T,addgrid.col='black',tl.cex=2, tl.col='black')



# DELETE:

dir('/Volumes/REBOOTII/ANAIS/Results/ROISignals_FunImgARWSF/', '^ROICorrelation_[0-9]*.txt')

mtx <- as.data.frame(matrix(NA, dim(dat)[1],4))
colnames(mtx) <- c('ThreatDet-Fear', 'Fear-HOPriors', 'HOPriors-Threat', 'ThreatCon-ThreatDet')


ids <- dat$ID
for ( i in 1:dim(dat)[1]){
  tmp <- as.matrix(read.table(paste('/Volumes/REBOOTII/ANAIS/Results/ROISignals_FunImgARWSF/ROICorrelation_', ids[i],'.txt', sep='')))
  mtx[i,1] <- tmp[1,2]
  mtx[i,2] <- tmp[2,3]
  mtx[i,3] <- tmp[3,4]
  mtx[i,4] <- tmp[1,4]
}


datcor <- cbind(dat, mtx)

n=9
summary(glm(datcor[,n]~datcor$Group))
colnames(datcor)[n]


