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