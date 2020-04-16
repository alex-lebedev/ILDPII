#ILDPII_CovConsr.R


library(xlsx)
library(plotrix)
library(pls)
library(stringi)

rm(list=ls())

# New Follow-up:
fdata <- read.xlsx2('~/Downloads/cvp.xlsx', 2, stringsAsFactors=F)
fdata_df <- data.frame(email=fdata$VAR1, country=fdata$VAR2, city=fdata$VAR3, age = fdata$VAR211,
                       CV1=fdata$CV_1, CV2=fdata$CV_2,CV3=fdata$CV_3, CV4=fdata$CV_4,CV5=fdata$CV_5,
                       CV6=fdata$CV_6,CV7=fdata$CV_7,CV8=fdata$CV_8, CV9=fdata$CV_9,CV10=fdata$CV_10,
                       CV11=fdata$CV_11,CV12=fdata$CV_12,CV13=fdata$CV_13,CV14=fdata$CV_14,CV15=fdata$CV_15,
                       CV16=fdata$CV_16, CV17=fdata$CV_17,CV18=fdata$CV_18,CV19=fdata$CV_19,
                       CV20=fdata$CV_20,CV21=fdata$CV_21,CV22=fdata$CV_22)


fdata_df[,4:26] <- apply(fdata_df[,4:26],2, as.vector)
fdata_df[,4:26] <- apply(fdata_df[,4:26],2, as.numeric)

x <- fdata_df[,5:26]


fdata_df$country <- as.vector(fdata_df$country)
fdata_df$country <- tolower(fdata_df$country)
fdata_df$country <- gsub(",",".",fdata_df$country)
fdata_df$country <- gsub(";",".",fdata_df$country)
fdata_df$country <- gsub(" ","",fdata_df$country)
fdata_df$city <- as.vector(fdata_df$city)
fdata_df$city <- tolower(fdata_df$city)
fdata_df$city <- gsub(",",".",fdata_df$city)
fdata_df$city <- gsub(";",".",fdata_df$city)
fdata_df$city <- gsub(" ","",fdata_df$city)



fdata_df$city[stri_detect_fixed(as.vector(fdata_df$city), 'ockh')] <- 'stockholm'
fdata_df$city[stri_detect_fixed(as.vector(fdata_df$city), 'ume')] <- 'umeå'
fdata_df$city[stri_detect_fixed(as.vector(fdata_df$city), 'olna')] <- 'solna'
fdata_df$country[stri_detect_fixed(as.vector(fdata_df$country), 'ockho')] <- 'sverige'
fdata_df$country[stri_detect_fixed(as.vector(fdata_df$country), 'svergie')] <- 'sverige'
fdata_df$country[stri_detect_fixed(as.vector(fdata_df$country), 'göt')] <- 'sverige'
fdata_df$country[stri_detect_fixed(as.vector(fdata_df$country), 'swe')] <- 'sverige'



pcr.mod <- prcomp(x)


scores <- pcr.mod$x
loadings <- pcr.mod$rotation


plot(loadings[,1:2], labels=names(x))
points(loadings[,1:2], labels=names(x))


plot(loadings[,1:2], pch=16, cex=0.1)
arrows(rep(0.0,length(loadings[,1])), rep(0,length(loadings[,1])), 
                                         loadings[,1], loadings[,2], col = 2)
text(loadings[,1:2], pos=3, names(x), cex=c(rep(0.55,6),1,rep(0.55,16)),
     col=1)
grid(50,50)

fdata_df$score1 <- 1-scores[,1]
fdata_df$score2 <- scores[,2]
fdata_df$score3 <- scores[,3]
fdata_df$score4 <- scores[,4]
fdata_df$score5 <- scores[,5]

covid19 <- fdata_df
covid19$email <- as.vector(covid19$email)
covid19$email <- tolower(covid19$email)
covid19$email <- gsub(",",".",covid19$email)
covid19$email <- gsub(";",".",covid19$email)
covid19$email <- gsub(" ","",covid19$email)

load('/Users/alebedev/Documents/Projects/HUD/HUD_final_mergedApril2020.rda')


ddd <- merge(covid19, ALLSCR, by='email')
cor(ddd$DP, ddd$score1)

ddd <- merge(covid19, ALLFU, by='email')
cor(ddd$CONS5, ddd$score1)

load('/Users/alebedev/Documents/Projects/ILDPII/AndresOtilia_anonymized.rda')
ddd <- merge(covid19, ALLEBS, by='email')

fitCONS5 <- (glm(EBS_evid~ALC_prox+TOB_prox+CAN_prox+MDMA_prox+STIM_prox+OPI_prox+PSY_prox, data=ddd))
summary(fitCONS5)

