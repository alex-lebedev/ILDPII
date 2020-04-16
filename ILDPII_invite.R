# Invitations:
rm(list=ls())

# Run ILDPII_NewScreen.R
source('~/GitHub/ILDPII/ILDPII_NewScreen.R')

library(xlsx)

scanned <- read.xlsx2(paste0('~/Downloads/ILDPII_scanning.xlsx'),1, stringsAsFactors=F)$email
scanned <- scanned[scanned!=""]
blist <- read.xlsx2(paste0('~/Downloads/ILDPII_scanning.xlsx'),2, stringsAsFactors=F)$email
blist <- blist[blist!=""]
# TESTED:
tested <- as.vector(read.xlsx2('/Users/alebedev/Documents/Projects/HUD/visits/tested.xlsx', 1)$email)

excl <- c(scanned, blist, tested)
excl <- tolower(excl)
excl <- gsub(",",".",excl)
excl <- gsub(";",".",excl)
excl <- gsub(" ","",excl)

load('/Users/alebedev/Documents/Projects/HUD/HUD_final_March2020.rda')
load('/Users/alebedev/Documents/Projects/ILDPII/ILDPII_screen.rda')

# New Follow-up:

fdata <- read.xlsx2(paste0('~/Downloads/NFU.xlsx'),2, stringsAsFactors=F)
fdata_df <- data.frame(email=fdata$VAR1, participation=fdata$VAR4, participation_comm=fdata$VAR4C, 
                       meds=fdata$VAR005, meds_which=fdata$VAR005C,
                       md=fdata$VAR003_1,	bpd=fdata$VAR003_2,	sch=fdata$VAR003_3,
                       adhd=fdata$VAR003_4,	aut=fdata$VAR003_5, ocd=fdata$VAR003_6,
                       other=fdata$VAR003_7,other_which=fdata$VAR003C)


for (i in 1:length(fdata_df$email)){
  # Select ith row:
  tmp <- subset(fdata[i,])
  fdata_df$O[i] <- mean(as.numeric(tmp[,paste0('VAR2_',c(7,8,9))]), na.rm=T)
  fdata_df$C[i] <- mean(as.numeric(tmp[,paste0('VAR2_',c(13,15))]), 7-as.numeric(tmp[,paste0('VAR2_',c(14))]))  
  fdata_df$E[i] <- mean(as.numeric(tmp[,paste0('VAR2_',c(4,5))]), 7-as.numeric(tmp[,paste0('VAR2_',c(6))]))  
  fdata_df$A[i] <- mean(as.numeric(tmp[,paste0('VAR2_',c(11,12))]), 7-as.numeric(tmp[,paste0('VAR2_',c(10))]))  
  fdata_df$N[i] <- mean(as.numeric(tmp[,paste0('VAR2_',c(1,2))]), 7-as.numeric(tmp[,paste0('VAR2_',c(3))]))  
  fdata_df$CONS_public[i] = as.numeric(tmp$AF_1)
  fdata_df$CONS_polit[i]= as.numeric(tmp$AF_2)
  fdata_df$CONS_monit[i]= as.numeric(tmp$AF_3)
  fdata_df$CONS_connect[i]= as.numeric(tmp$AF_4)
  fdata_df$CONS_org[i]= as.numeric(tmp$AF_5)
  fdata_df$ALC_freq[i]= as.numeric(tmp$ALC1)
  fdata_df$ALC_prox[i]= as.numeric(tmp$ALC2)
  fdata_df$ALC_age[i]= tmp$ALC3
  fdata_df$TOB_freq[i]= as.numeric(tmp$TOB1)
  fdata_df$TOB_prox[i]= as.numeric(tmp$TOB2)
  fdata_df$TOB_age[i]= tmp$TOB3
  fdata_df$MDMA_freq[i]= as.numeric(tmp$MDMA1)
  fdata_df$MDMA_prox[i]= as.numeric(tmp$MDMA2)
  fdata_df$MDMA_age[i]= tmp$MDMA3
  fdata_df$CAN_freq[i]= as.numeric(tmp$CAN1)
  fdata_df$CAN_prox[i]= as.numeric(tmp$CAN2)
  fdata_df$CAN_age[i]= tmp$CAN3
  fdata_df$STIM_freq[i]= as.numeric(tmp$STIM1)
  fdata_df$STIM_prox[i]= as.numeric(tmp$STIM2)
  fdata_df$STIM_age[i]= tmp$STIM3
  fdata_df$OPI_freq[i]= as.numeric(tmp$OPI1)
  fdata_df$OPI_prox[i]= as.numeric(tmp$OPI2)
  fdata_df$OPI_age[i]= tmp$OPI3
  fdata_df$PSY_freq[i]= as.numeric(tmp$PSY1)
  fdata_df$PSY_prox[i]= as.numeric(tmp$PSY2)
  fdata_df$PSY_age[i]= tmp$PSY3
  fdata_df$MLQ_presence[i] <- mean(as.numeric(tmp[,paste0('MLQ_',c(1,4,5,6))]), 7-as.numeric(tmp[,paste0('MLQ_',c(9))]))
  fdata_df$MLQ_search[i] <- mean(as.numeric(tmp[,paste0('MLQ_',c(2,3,7,8,10))]))
  fdata_df$SWLS[i] <- mean(as.numeric(tmp[,paste0('SWLS_',c(1:5))]))
  
  fdata_df$EBS_feel[i] <- mean(as.numeric(tmp[,paste0('EBS_feel_',c(1:4))]))
  fdata_df$EBS_evid[i] <- mean(as.numeric(tmp[,paste0('EBS_evid_',c(1:4))]))
  fdata_df$EBS_polit[i] <- mean(as.numeric(tmp[,paste0('EBS_polit_',c(1:4))]))
}

fdata_df$CONS5 <- apply(fdata_df[,c('CONS_public','CONS_polit', 'CONS_monit','CONS_connect', 'CONS_org')],1,mean)

fdata_df$email <- tolower(fdata_df$email)
fdata_df$email <- gsub(",",".",fdata_df$email)
fdata_df$email <- gsub(";",".",fdata_df$email)
fdata_df$email <- gsub(" ","",fdata_df$email)
fdata_df <- fdata_df[!duplicated(fdata_df$email),]

fdata_df$email[fdata_df$email=="ida.frossander@fi.se"] <- "ida.frossander@gmail.com"
fdata_df$email[fdata_df$email=="zuzana.sekakova@gmail.com"] <- "zuzana.sekajova@gmail.com"
fdata_df$email[fdata_df$email=="emeliesegwr@hotmail.se"] <- "emelieseger@hotmail.se" 

########################### GENERATE SCANNED 
# DEFINE TESTED:
scr1 <- subset(SCRFU_df, is.element(SCRFU_df$email, setdiff(SCRFU_df$email, SCREEN_df$email)))
ILDPII_screen$nonbingen <- 'no'
ILDPII_screen$nonbingen[ILDPII_screen$sex==3] <- 'yes'
allscr <- rbind(scr1[,c('email', 'sex', 'nonbingen', 'age', 'mothertongue','PDI_total')],
                SCREEN_df[,c('email', 'sex', 'nonbingen', 'age', 'mothertongue','PDI_total')],
                ILDPII_screen[,c('email', 'sex', 'nonbingen', 'age', 'mothertongue','PDI_total')]
)
rownames(allscr) <- c()
allscr <- allscr[!duplicated(allscr$email, fromLast=T),]
ildp_tested <- read.xlsx2(paste0('~/Downloads/ILDPII_scanning.xlsx'),1, stringsAsFactors=F)
ildp_tested <- subset(ildp_tested[,c('email', 'study.ID', 'RAVLT_TaskVer')],ildp_tested$email!="")
sc <- merge(ildp_tested, allscr, by='email')
sc$PDIgroup <- sc$PDI_total>9
sc <- subset(sc, sc$PDI_total>9 | sc$PDI_total<4)
sc$sex[sc$sex==1] <- 'man'
sc$sex[sc$sex==2] <- 'kvinna'
for (i in 1:length(sc$RAVLT_TaskVer)){
  tmp <- strsplit(sc$RAVLT_TaskVer, '_')[[i]]
  sc$TaskLang[i] <- tmp[2]
  sc$TaskVer[i] <- tmp[1]
  sc$TaskType[i] <- strtrim(tmp[1], c(1))
}
sc$TaskBlock <- 'NA'
sc$TaskBlock[sc$TaskVer=='uB1'] <- 'B1'
sc$TaskBlock[sc$TaskVer=='uB2'] <- 'B2'
sc$TaskBlock[sc$TaskVer=='uA1'] <- 'A1'
sc$TaskBlock[sc$TaskVer=='uA2'] <- 'A2'
sc$TaskBlock[sc$TaskVer=='iB1'] <- 'B1'
sc$TaskBlock[sc$TaskVer=='iB2'] <- 'B2'
sc$TaskBlock[sc$TaskVer=='iA1'] <- 'A1'
sc$TaskBlock[sc$TaskVer=='iA2'] <- 'A2'
sc$age <- as.numeric(sc$age)
########################


tmp <- merge(fdata_df, SCREEN_df, by='email')
tmp$age <- as.numeric(as.vector(tmp$age))
tmp$md <- as.numeric(tmp$md) 
tmp$bpd <- as.numeric(tmp$bpd)
tmp$sch<- as.numeric(tmp$sch)
tmp$adhd<- as.numeric(tmp$adhd)
tmp$aut<- as.numeric(tmp$aut)
tmp$ocd<- as.numeric(tmp$ocd)
tmp$meds <- as.numeric(tmp$meds)
tmp$md[is.na(tmp$md)] <- 2
tmp$bpd[is.na(tmp$bpd)] <- 2
tmp$sch[is.na(tmp$sch)] <- 2
tmp$adhd[is.na(tmp$adhd)] <- 2
tmp$aut[is.na(tmp$aut)] <- 2
tmp$ocd[is.na(tmp$ocd)] <- 2
tmp$meds[is.na(tmp$meds)] <- 2
tmp$mothertongue <- gsub(" ","",tolower(tmp$mothertongue))
tmp$mothertongue <- gsub(" ","",tmp$mothertongue)

tmp <- subset(tmp, !is.element(tmp$email,excl))
tmp <- subset(tmp, tmp$age>=18 & tmp$age<35 & tmp$md!=1 & tmp$bpd!=1 &
                tmp$sch!=1 & tmp$adhd!=1 & tmp$aut!=1 & tmp$ocd!=1 & tmp$meds!=1 &
                tmp$participation!=3  & (tmp$mothertongue=='svenska' | tmp$mothertongue=='english'))
tmp$PDIgroup <- tmp$PDI_total>9
tmp1 <- tmp[,c('email', 'sex', 'nonbingen', 'age', 'mothertongue','PDIgroup', 'PDI_total','other', 'meds', 'meds_which')]
tmp1$FUtype <- 'ILDP'


tmp <- ILDPII_screen
tmp$age <- as.numeric(as.vector(tmp$age))
tmp$diagMDep <- as.numeric(tmp$diagMDep) 
tmp$diagBP <- as.numeric(tmp$diagBP)
tmp$diagScz<- as.numeric(tmp$diagScz)
tmp$diagADHD<- as.numeric(tmp$diagADHD)
tmp$diagASD<- as.numeric(tmp$diagASD)
tmp$diagOCD<- as.numeric(tmp$diagOCD)
tmp$medsY1 <- as.numeric(tmp$medsY1)
tmp$diagMDep[is.na(tmp$diagMDep)] <- 2
tmp$diagBP[is.na(tmp$diagBP)] <- 2
tmp$diagScz[is.na(tmp$diagScz)] <- 2
tmp$diagADHD[is.na(tmp$diagADHD)] <- 2
tmp$diagASD[is.na(tmp$diagASD)] <- 2
tmp$diagOCD[is.na(tmp$diagOCD)] <- 2
tmp$medsY1[is.na(tmp$medsY1)] <- 2
tmp <- subset(tmp, !is.element(tmp$email,excl))
tmp <- subset(tmp, !is.element(tmp$email,tmp1$email))
tmp <- subset(tmp, tmp$age>=18 & tmp$age<35 & tmp$diagMDep!=1 & tmp$diagBP!=1 &
                tmp$diagScz!=1 & tmp$diagADHD!=1 & tmp$diagASD!=1 & tmp$diagOCD!=1 & tmp$medsY1!=1 &
                tmp$participation!=3  & (tmp$mothertongue!=4))
tmp$PDIgroup <- tmp$PDI_total>9
tmp$nonbingen <- 'no'
tmp$nonbingen[tmp$sex==3] <- 'yes'

tmp2 <- tmp[,c('email', 'sex', 'nonbingen', 'age', 'mothertongue','PDIgroup', 'PDI_total','diagOther', 'medsY1', 'medsWhich')]
tmp2$FUtype <- 'NewScreen'
colnames(tmp2) <- colnames(tmp1)

tmp <- rbind(tmp1,tmp2)
#tmp <- subset(tmp, !(tmp$PDIgroup==F & tmp$other==1))
tmp$other[is.na(tmp$other==999)] <- 2
tmp$other[tmp$other==999] <- 2

# SELECTING GROUPS 
g1 <- subset(tmp, tmp$PDIgroup==T)
#g2 <- subset(tmp, tmp$PDIgroup==F & tmp$PDI_total<4)

g2 <- subset(tmp, tmp$PDIgroup==F & tmp$age>(mean(sc$age[sc$PDIgroup==T], na.rm = T)-1.5*sd(sc$age[sc$PDIgroup==T], na.rm = T)) & 
             tmp$PDI_total<4)


g2$other[is.na(g2$other)] <- 2
g2 <- subset(g2, g2$other!=1 & g2$meds!=1)
inv <- rbind(g1, g2)
inv <- inv[!as.numeric(inv$meds_which)!=1,]
inv$mothertongue[inv$mothertongue==2] <- 'svenska'
inv$mothertongue[inv$mothertongue==1] <- 'english'
inv$mothertongue[inv$mothertongue==3] <- 'check'

inv$sex[inv$sex==1] <- 'man'
inv$sex[inv$sex==2] <- 'kvinna'
inv <- subset(inv, inv$nonbingen=='no')


invPDI <- subset(inv, inv$PDIgroup==T)
#invPDI <- subset(inv, inv$PDIgroup==T & inv$sex=='man')
invCON <- subset(inv, inv$PDIgroup==F & inv$sex=='kvinna')

inv <- rbind(invPDI, invCON[1:(nrow(invPDI)-4),])




  
write.csv2(inv, file = paste0('~/Downloads/ildp_invite_', Sys.Date(),'.csv'))

sample(c('iA1','iA2','iB1','iB2','uA1','uA2','uB1','uB2'))


ddd <- rbind(SCREEN_df[,c('email', 'PDI_total', 'sex', 'age', 'sex', 'mothertongue')], SCRFU_df[,c('email', 'PDI_total', 'sex', 'age', 'sex', 'mothertongue')],
             ILDPII_screen[,c('email', 'PDI_total', 'sex', 'age', 'sex', 'mothertongue')])
ddd <- ddd[!duplicated(ddd$email, fromLast = T),]

sc <- subset(ddd, is.element(ddd$email, scanned))
sc <- subset(sc, sc$PDI_total>9 | sc$PDI_total<4)



