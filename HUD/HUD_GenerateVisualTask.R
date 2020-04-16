library(xlsx)

dots_dat <- read.xlsx2('/Users/alebedev/Desktop/Psychopy/dots/dots.xlsx',1)



nt_block = 25

dots_dat <- data.frame(trial=1:150,
                       block=c(rep(1,nt_block),rep(2,nt_block),rep(3,nt_block),
                               rep(4,nt_block),rep(5,nt_block),rep(6,nt_block)),
                       ndots=100, filetime=1000)


set.seed(1935)
bt <- sample(c('static', 'dynamic'), 6, replace=T)
for (block in unique(dots_dat$block)){
  dots_dat$btype[dots_dat$block==block] <- bt[block]
  dots_dat$direction[dots_dat$block==block] <- sample(c(0, 180, 90, 45, 135), nt_block, replace = T)
  dots_dat$coherence[dots_dat$block==block] <- sample(c(0,0.25,0.5,0.75), nt_block, replace = T)
  dots_dat$speed[dots_dat$block==block] <- sample(c(0, 0.0001, 0.001), nt_block, replace = T)
  dots_dat$direction[dots_dat$block==block] <- sample(c(0, 180, 90, 45, 135), nt_block, replace = T)
}

dots_dat$corr <- 'z'
dots_dat$corr[dots_dat$speed!=0] <- 'm'
write.xlsx2(dots_dat, file='/Users/alebedev/Desktop/Psychopy/dots/dotsfinal.xlsx', row.names = F)




# Analyze:

rdata <- list.files('/Users/alebedev/Desktop/Psychopy/dots/data/', '*.csv')


output <- data.frame(ID = paste('s',1:length(rdata), sep=''),
                     AccST = rep(NA, length(rdata)), AccDYN = rep(NA, length(rdata)),
                     AccSTfast= rep(NA, length(rdata)), AccSTslow= rep(NA, length(rdata)),
                     AccDYNfast= rep(NA, length(rdata)), AccDYNslow= rep(NA, length(rdata)),
                     rtST = rep(NA, length(rdata)), rtDYN = rep(NA, length(rdata)),
                     rtSTfast= rep(NA, length(rdata)), rtSTslow= rep(NA, length(rdata)),
                     rtDYNfast= rep(NA, length(rdata)), rtDYNslow= rep(NA, length(rdata))
                                    )


for (i in 1:length(rdata)){
  tmp <- read.csv(paste('/Users/alebedev/Desktop/Psychopy/dots/data/', rdata[i], sep=''))
  
  output$AccST[i] <- mean(tmp$resp_stat.corr, na.rm=T)
  output$AccDYN[i] <- mean(tmp$resp_dyn.corr, na.rm=T)
  output$AccSTslow[i] <- mean(tmp$resp_stat.corr[tmp$speed<0.001], na.rm=T)
  output$AccDYNslow[i] <- mean(tmp$resp_dyn.corr[tmp$speed<0.001], na.rm=T)
  output$AccSTfast[i] <- mean(tmp$resp_stat.corr[tmp$speed>=0.001], na.rm=T)
  output$AccDYNfast[i] <- mean(tmp$resp_dyn.corr[tmp$speed>=0.001], na.rm=T)
  
  output$rtST[i] <- mean(tmp$resp_stat.rt, na.rm=T)
  output$rtDYN[i] <- mean(tmp$resp_dyn.rt, na.rm=T)
  output$rtSTslow[i] <- mean(tmp$resp_stat.rt[tmp$speed<0.001], na.rm=T)
  output$rtDYNslow[i] <- mean(tmp$resp_dyn.rt[tmp$speed<0.001], na.rm=T)
  output$rtSTfast[i] <- mean(tmp$resp_stat.rt[tmp$speed>=0.001], na.rm=T)
  output$rtDYNfast[i] <- mean(tmp$resp_dyn.rt[tmp$speed>=0.001], na.rm=T)
}

write.xlsx2(output, file='/Users/alebedev/Desktop/Psychopy/dots/dotsPlot_output.xlsx', row.names = F)



