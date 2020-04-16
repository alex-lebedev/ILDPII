# 

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
# Testing:
chisq.test(table(sc$TaskLang, sc$PDIgroup))
chisq.test(table(sc$TaskType, sc$PDIgroup))

chisq.test(table(sc$sex, sc$PDIgroup))


