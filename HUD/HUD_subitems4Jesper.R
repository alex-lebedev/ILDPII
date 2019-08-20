# Subitem extracton:
# Jesper

# PP1-2: visual and auditory disturbances (VAR3_16 VAR3_17)
# Screening: OLIFE (shapes in the dark): VAR058 



library(xlsx)

screen_raw <- read.xlsx2('/Users/alebedev/Downloads/Export.xlsx',2, stringsAsFactors=F)

screen_raw[screen_raw==999] <- NA
screen_subitems <- data.frame(ID = screen_raw$ID, consentYes1 = screen_raw$VAR000, username = screen_raw$VAR001,email=screen_raw$VAR213,  
                        age=screen_raw$VAR211,sex=screen_raw$VAR212, education=screen_raw$VAR214, occupation=screen_raw$VAR215, 
                        mothertongue=screen_raw$VAR216, surveyfoundout=screen_raw$VAR217,
                        somaticDS = screen_raw$VAR002,
                        diagMDep = screen_raw$VAR003_1,diagBP = screen_raw$VAR003_2, diagScz = screen_raw$VAR003_3,
                        diagADHD = screen_raw$VAR003_4,diagASD = screen_raw$VAR003_5, diagOCD = screen_raw$VAR003_6,
                        diagOther = screen_raw$VAR003_7, diagOtherWhich = screen_raw$VAR003C, brainInjuryY1 = screen_raw$VAR004,
                        medsY1 = screen_raw$VAR005, medsWhich = screen_raw$VAR005C,
                        drug_alc = screen_raw$VAR101_1,drug_tobacco = screen_raw$VAR101_2, drug_mdma = screen_raw$VAR101_3,
                        drug_cannabis = screen_raw$VAR101_4, drug_stim = screen_raw$VAR101_5, drug_opi = screen_raw$VAR101_6,
                        drug_psychedelics = screen_raw$VAR101_7, drug_none = screen_raw$VAR101_8,
                        OLIFE_ShapesDark = screen_raw$VAR058 
                        )
screen_subitems$drug_alc <- as.factor(2-as.numeric(screen_subitems$drug_alc))
screen_subitems$drug_tobacco <- as.factor(2-as.numeric(screen_subitems$drug_tobacco))
screen_subitems$drug_mdma <- as.factor(2-as.numeric(screen_subitems$drug_mdma))
screen_subitems$drug_cannabis <- as.factor(2-as.numeric(screen_subitems$drug_cannabis))
screen_subitems$drug_stim <- as.factor(2-as.numeric(screen_subitems$drug_stim))
screen_subitems$drug_opi <- as.factor(2-as.numeric(screen_subitems$drug_opi))
screen_subitems$drug_psychedelics <- as.factor(2-as.numeric(screen_subitems$drug_psychedelics))
screen_subitems$drug_none <- as.factor(2-as.numeric(screen_subitems$drug_none))


dtypes <- c('PP', 'PP2')
d1 <- read.xlsx2(paste('~/Downloads/',dtypes[1], '.xlsx',sep=''),2, stringsAsFactors=F)
d2 <- read.xlsx2(paste('~/Downloads/',dtypes[2], '.xlsx',sep=''),2, stringsAsFactors=F)

dd1 <- data.frame(email=d1$VAR1, visual = as.numeric(d1$VAR3_16), auditory=as.numeric(d1$VAR3_17), desDrug1=NA,desDrug2=NA)
dd2 <- data.frame(email=d2$VAR1, visual = as.numeric(d2$VAR3_16), auditory=as.numeric(d2$VAR3_17), desDrug1=d2$DP1, desDrug2=d2$DP1)

dd <- rbind(dd1,dd2)


# Visual
table(dd$visual>0)/sum(table(dd$visual>0))
table(dd$visual==1)[2]/table(dd$visual>0)[2]
table(dd$visual==2)[2]/table(dd$visual>0)[2]
table(dd$visual==3)[2]/table(dd$visual>0)[2]
pie(table(dd$visual))

# Auditory:
table(dd$auditory>0)/sum(table(dd$auditory>0))
table(dd$auditory==1)[2]/table(dd$auditory>0)[2]
table(dd$auditory==2)[2]/table(dd$auditory>0)[2]
table(dd$auditory==3)[2]/table(dd$auditory>0)[2]


# RT:
dots <- data.frame(subject = rep(HUDMAIN_df$subject,4), group = rep(HUDMAIN_df$group.y,4),
                   RT=c(HUDMAIN_df$rtSTfast,HUDMAIN_df$rtSTslow, HUDMAIN_df$rtDYNfast,HUDMAIN_df$rtDYNslow),
                   BGD = as.ordered(c(rep('static', dim(HUDMAIN_df)[1]*2),rep('dynamic', dim(HUDMAIN_df)[1]*2))),
                   SPEED = as.ordered(rep(c(rep('fast', dim(HUDMAIN_df)[1]),rep('slow', dim(HUDMAIN_df)[1])),2))
)

# Acc:
dots <- data.frame(subject = rep(HUDMAIN_df$subject,4), group = rep(HUDMAIN_df$group.y,4),
                   Acc=c(HUDMAIN_df$AccSTfast,HUDMAIN_df$AccSTslow, HUDMAIN_df$AccDYNfast,HUDMAIN_df$AccDYNslow),
                   BGD = as.ordered(c(rep('static', dim(HUDMAIN_df)[1]*2),rep('dynamic', dim(HUDMAIN_df)[1]*2))),
                   SPEED = as.ordered(rep(c(rep('fast', dim(HUDMAIN_df)[1]),rep('slow', dim(HUDMAIN_df)[1])),2))
)

# Regular GLM:
summary(glm(RT~BGD*SPEED*group, data=dots))
summary(glm(Acc~BGD*SPEED*group, data=dots))


# MLE:
modME <- lme(RT~BGD*SPEED*group,data=dots, random=~1|subject)
summary(modME)
modME <- lme(Acc~BGD*SPEED*group,data=dots, random=~1|subject)
summary(modME)

dotsST <- subset(dots, dots$BGD=='static')
dotsDYN <- subset(dots, dots$BGD=='dynamic')

ggplot(dotsST, aes(x=SPEED, y=RT, fill=factor(group))) + 
  stat_summary(fun.y=mean, geom="bar",position=position_dodge(1)) + 
  stat_summary(fun.ymin=min,fun.ymax=max,geom="errorbar",
               color="grey40",position=position_dodge(1), width=.2)+ ylim(0, 3)
ggplot(dotsDYN, aes(x=SPEED, y=RT, fill=factor(group))) + 
  stat_summary(fun.y=mean, geom="bar",position=position_dodge(1)) + 
  stat_summary(fun.ymin=min,fun.ymax=max,geom="errorbar",
               color="grey40",position=position_dodge(1), width=.2)+ ylim(0, 3)