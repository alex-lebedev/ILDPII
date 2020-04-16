
# COVID_19:
cvp <- read.xlsx2(paste('~/Downloads/cvp.xlsx',sep=''),2, stringsAsFactors=F)
# only to those who hasn't responded
cvp$VAR1 <- tolower(cvp$VAR1)
cvp$VAR1 <- gsub(",",".",cvp$VAR1)
cvp$VAR1 <- gsub(";",".",cvp$VAR1)
cvp$VAR1 <- gsub(" ","",cvp$VAR1)

ALLSCR_sel <- subset(ALLSCR, is.element(ALLSCR$email,cvp$VAR1)==F)

fu_list <- ALLSCR_sel[,c('email', 'age','sex', 'mothertongue')]
write.csv2(fu_list, file = paste0('~/Downloads/covid19FUlist_', Sys.Date(),'.csv'))

