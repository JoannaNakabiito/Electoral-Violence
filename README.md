# Electoral-Violence
The first quantitative study, using data on African national elections from 1992-2019, examining whether National Inter-religious Councils' (NIRCs) electoral-related peacebuilding interventions lower the severity of electoral violence and if their ability to do so depends on NIRCs’ social power.

##CODE
install.packages("Matching")
library(stargazer)
library(Matching)
install.packages("rbounds")
install.packages("Matching")
library(rbounds)
install.packages("stargazer")
install.packages("dplyr")
install.packages("rgenoud")
install.packages("optmatch")
install.packages(genoud)
install.packages("tableone")
install.packages("devtools")
install.packages("AER")
install.packages("systemfit")
install.packages("vcov")
install.packages("githubinstall")
install.packages("bread")
install.packages("ivtools")
devtools::install_github("dustinfife/flexplot")
library(githubinstall)
# install the development version
devtools::install_github("dustinfife/flexplot", ref="development", force = TRUE)
install.packages("sandwich")
install.packages("survival")
#Package for interaction effects
install.packages("DAMisc")
install.packages("interactions")
install.packages("showtext")
update.packages(ask = FALSE, checkBuilt = TRUE)
install.packages("mice")
install.packages("tidyverse")
install.packages("VIM")
#FOR PANEL DATA
install.packages("geepack")
install.packages("MESS")

hist(Matched13$Power)
Matched13$group <- as.factor(ifelse(Matched13$PowerIndex1 == 2.2, 'VH', 
                                    ifelse(Matched13$PowerIndex1 == 2.0, 'H', 
                                           ifelse(Matched13$PowerIndex1 == 1.8, 'M',
                                                  ifelse(Matched13$PowerIndex1 == 1.6,'L', 'VL')))))



###########3
Matched13$XPower[Matched13$XPower==1]<-0
Matched13$XPower[Matched13$XPower==2]<-1
Matched13$NELDA1.LegPower[Matched13$NELDA1.LegPower==1]<-0
Matched13$NELDA1.LegPower[Matched13$NELDA1.LegPower==2]<-1
Matched13$NELDA1.Affiliation[Matched13$NELDA1.Affiliation==1]<-0
Matched13$NELDA1.Affiliation[Matched13$NELDA1.Affiliation==2]<-1
Matched13$NELDA1.SubsidiaryOrg[Matched13$NELDA1.SubsidiaryOrg==1]<-0
Matched13$NELDA1.SubsidiaryOrg[Matched13$NELDA1.SubsidiaryOrg==2]<-1
Matched13$NELDA1.SocialMedia[Matched13$NELDA1.SocialMedia==1]<-0
Matched13$NELDA1.SocialMedia[Matched13$NELDA1.SocialMedia==2]<-1
Matched13$NELDA1.ExpertPower[Matched13$NELDA1.ExpertPower==1]<-0
Matched13$NELDA1.ExpertPower[Matched13$NELDA1.ExpertPower==2]<-1
Matched13$NELDA1.ExpertPower[Matched13$NELDA1.ExpertPower==3]<-1
hist(Matched13$st_PowerIndex)

View(Matched13)
require(haven)
Matched13 %>%
  rowwise() %>%
  mutate(st_PowerIndex = mean(c(NELDA1.ExpertPower, NELDA1.LegPower, NELDA1.Affiliation, NELDA1.SubsidiaryOrg,NELDA1.SocialMedia ))) ->Matched13

Matched13$Power <- as.factor(ifelse(Matched13$st_PowerIndex1 <= -0.9368670, 'A', 'B'))

Matched13$group <- as.factor(ifelse(Matched13$st_PowerIndex <= 0.2, 'A', 
                                    ifelse(Matched13$st_PowerIndex > 0.8, 'C', 'B')))

# the 0.2
Matched13$groupss <- as.factor(ifelse(Matched13$st_PowerIndex1 < -1.5, 'A',
                                      ifelse(Matched13$st_PowerIndex1 > 0.4, 'C', 'B')))
# the 0.4
Matched13$groupss <- as.factor(ifelse(Matched13$st_PowerIndex1 < -0.2, 'A',
                                      ifelse(Matched13$st_PowerIndex1 > 0.9, 'C', 'B')))
#The 0.6
Matched13$Power <- as.factor(ifelse(Matched13$st_PowerIndex1 <= -0.9368670, 'A', 'B'))

Matched13$PeaceActivities<-as.factor(Matched13$PeaceActivities)
Matched13$PeaceActivities<-as.factor(Matched13$Power)
View(Matched13)
hist(Matched13$st_PowerIndex1)
Matched13
Matched13$st_PowerIndex1<-scale(Matched13$st_PowerIndex)
View(Matched13)
Power1<- glm(nelda33 ~ PeaceActivities+harass+eco_growth+compete+riots+monitors+PKO+multiparty, data = Matched13, family = binomial(link = "logit"), weights = match.weight)
Power2<- glm(nelda33 ~ factor(group)+harass+eco_growth+compete+riots+monitors+PKO+multiparty, data = Matched13, family = binomial(link = "logit"), weights = match.weight)
Power3<- glm(nelda33 ~ PeaceActivities*factor(group)+harass+eco_growth+compete+riots+monitors+PKO+multiparty, data = Matched13, family = binomial(link = "logit"), weights = match.weight)
stargazer::stargazer(Power1,Power2,Power3, title = "Table 8: The moderation effect of Power on NIRCs' Peace Activiies on Electoral Violence", dep.var.caption = "DV: Significant Electoral Violence in Africa, 1992-2019", covariate.labels =  c("Peace Activities"), notes.label = "Significance levels", type = "text", out = "C:/Users/nakab/Documents/UNDESA UNV/H2BOVER.htm")

sjPlot::plot_model(Power3, type = "int")+theme_bw() 

###########
myvars3<- c("PeaceActivities", "st_PowerIndex1", "riots", "compete", "harass", "eco_growth", "PKO", "monitors")
newdata3<- Matched13[myvars3]
newdatacori3<-ginv(cor(newdata3))
colnames(newdatacori3)<-colnames(newdata3)
rownames(newdatacori3)<-colnames(newdata3)
newdatacori3
corrplot::corrplot(corr = newdatacori3, method = "number", is.corr = FALSE, addCoef.col="black", number.cex=1)
```

interactions::cat_plot(Power3, pred = PeaceActivities, modx = Power, point.shape = TRUE, geom = "bar", colors="Rainbow")
hist(st_PowerIndex)


library(haven)
NELDA <- read_sav("C:/Users/nakab/Documents/Uppsala_University/METHODSII/R_Working_Directory/RPR/NELDA.sav")
###########################PRESENCE OF NIRCS ON ELECTORAL VIOLENCE RESTRAINT####################
#NELDA to dataframe
N_FRAME<- data.frame(NELDA$country,NELDA$year,NELDA$NIRCPresent,NELDA$PKO,NELDA$multiparty,NELDA$harass,NELDA$eco_growth,NELDA$compete,NELDA$riots,NELDA$fraud,NELDA$nelda33,NELDA$monitors)
is.na(N_FRAME)
# RENAME VARIABLES
names(N_FRAME)[names(N_FRAME)=="NELDA.NIRCPresent"]<-"NIRCPresent"
names(N_FRAME)[names(N_FRAME)=="NELDA.country"]<-"country"
names(N_FRAME)[names(N_FRAME)=="NELDA.year"]<-"Year"
names(N_FRAME)[names(N_FRAME)=="NELDA.PKO"]<-"PKO"
names(N_FRAME)[names(N_FRAME)=="NELDA.multiparty"]<-"multiparty"
names(N_FRAME)[names(N_FRAME)=="NELDA.harass"]<-"harass"
names(N_FRAME)[names(N_FRAME)=="NELDA.eco_growth"]<-"eco_growth"
names(N_FRAME)[names(N_FRAME)=="NELDA.compete"]<-"compete"
names(N_FRAME)[names(N_FRAME)=="NELDA.riots"]<-"riots"
names(N_FRAME)[names(N_FRAME)=="NELDA.fraud"]<-"fraud"
names(N_FRAME)[names(N_FRAME)=="NELDA.vioDem"]<-"vioDem"
names(N_FRAME)[names(N_FRAME)=="NELDA.nelda33"]<-"nelda33"
names(N_FRAME)[names(N_FRAME)=="NELDA.monitors"]<-"monitors"
sum(is.na(N_FRAME))

#New clean dataset
##86 Missing values will be omitted: https://www.youtube.com/watch?v=q8eR2suCyGk
RPR <- na.omit(N_FRAME)
sum(is.na(RPR))


#Recode MNAR values to NA that will be passed in regression and propensity score matching
library(dplyr)
RPR[RPR==-9]<-NA
sum(is.na(RPR))

interactions::cat_plot(Power3, pred = Interventions, modx = Power, point.shape = TRUE,geom = "line", colors="Rainbow")

##TRYING MICE
#https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
#https://www.youtube.com/watch?v=MpnxwNXGV-E

set.seed(123)

library(tidyverse)
#Run both together
library(mice)
mice::md.pattern(RPR)

library(VIM)
aggr_plot <- aggr(N_FRAME, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(RPR), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
aggr_plot <- aggr(RPR, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(RPR), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

library(mice)
RPR0<-mice::mice(RPR, m=5, meth="rf")

summary(RPR0)
RPR0$imp$PKO
RPR0$imp$harass
RPR0$imp$monitors
RPR0$imp$compete

RPR1<- mice::complete(RPR0,1)
View(RPR1)
sum(is.na(RPR1))
aggr_plot <- aggr(RPR1, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(RPR1), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


#######RPR DESCRIPTIVES##########

summary(RPR1$nelda33)
summary(RPR1$NIRCPresent)
summary(RPR1$compete)
summary(RPR1$monitors)
summary(RPR1$PKO)
summary(RPR1$riots)
summary(RPR1$harass)
summary(RPR1$eco_growth)

#CREATING FACTOR VARIABLES
RPR1$NIRCPresent <- ifelse(RPR1$NIRCPresent==1, TRUE, FALSE)
RPR1$ElectoralViolence <- ifelse(RPR1$nelda33==1, TRUE, FALSE)
RPR1$multiparty <- ifelse(RPR1$multiparty==1, TRUE, FALSE)
RPR1$PKO <- ifelse(RPR1$PKO==1, TRUE, FALSE)
RPR1$harass <- ifelse(RPR1$harass==1, TRUE, FALSE)
RPR1$eco_growth <- ifelse(RPR1$eco_growth==1, TRUE, FALSE)
RPR1$compete <- ifelse(RPR1$compete==1, TRUE, FALSE)
RPR1$riots <- ifelse(RPR1$riots==1, TRUE, FALSE)
RPR1$monitors <- ifelse(RPR1$monitors==1, TRUE, FALSE)
RPR1$TypeCoded <- ifelse(RPR1$TypeCoded==1, TRUE, FALSE)
View(RPR1)


### POWER ##########################
rm(NELDA1)
NELDA1 <- read_sav("NELDA1.sav")
View(NELDA1)

#NELDA1 to dataframe
N_FRAME2<- data.frame(NELDA1$harass,NELDA1$eco_growth,NELDA1$PI1,NELDA1$PI2,NELDA1$compete,NELDA1$riots,NELDA1$monitors,NELDA1$TypeCoded,NELDA1$nelda33,NELDA1$multiparty,NELDA1$PKO,NELDA1$PowerIndex1,NELDA1$PowerIndex2,NELDA1$Web,NELDA1$LegPower,NELDA1$SocialMedia,NELDA1$SubsidiaryOrg,NELDA1$Affiliation,NELDA1$PeaceActivities,NELDA1$ExpertPower)
View(N_FRAME2)
is.na(N_FRAME2)
# Number of unclear values. NB: NA values at this stage are merely unclear
sum(is.na(N_FRAME2))

#New clean dataset
##86 Missing values will be omitted: https://www.youtube.com/watch?v=q8eR2suCyGk
Chi <- na.omit(N_FRAME2)
View(Chi)
sum(is.na(Chi))

#Recode MNAR values to NA that will be passed in regression and propensity score matching
library(dplyr)
Chi[Chi==-9]<-NA
View(Chi)
sum(is.na(Chi))
ChiData_1 <- na.omit(Chi)
sum(is.na(ChiData_1))
# RENAME VARIABLES
names(ChiData_1)[names(ChiData_1)=="NELDA1.PeaceActivities"]<-"PeaceActivities"
names(ChiData_1)[names(ChiData_1)=="NELDA1.harass"]<-"harass"
names(ChiData_1)[names(ChiData_1)=="NELDA1.eco_growth"]<-"eco_growth"
names(ChiData_1)[names(ChiData_1)=="NELDA1.compete"]<-"compete"
names(ChiData_1)[names(ChiData_1)=="NELDA1.fraud"]<-"fraud"
names(ChiData_1)[names(ChiData_1)=="NELDA1.riots"]<-"riots"
names(ChiData_1)[names(ChiData_1)=="NELDA1.monitors"]<-"monitors"
names(ChiData_1)[names(ChiData_1)=="NELDA1.TypeCoded"]<-"TypeCoded"
names(ChiData_1)[names(ChiData_1)=="NELDA1.nelda33"]<-"nelda33"
names(ChiData_1)[names(ChiData_1)=="NELDA1.multiparty"]<-"multiparty"
names(ChiData_1)[names(ChiData_1)=="NELDA1.PKO"]<-"PKO"
names(ChiData_1)[names(ChiData_1)=="NELDA1.PowerIndex1"]<-"PowerIndex1"
names(ChiData_1)[names(ChiData_1)=="NELDA1.PowerIndex2"]<-"PowerIndex2"
names(ChiData_1)[names(ChiData_1)=="NELDA1.PI2"]<-"PI2"
names(ChiData_1)[names(ChiData_1)=="NELDA1.PI1"]<-"PI1"
st_PowerIndex
View(ChiData_1)
names(Matched13)[names(Matched13)=="st_PowerIndex"]<-"Power_Index"


#Run both together
library(VIM)
aggr_plot <- aggr(Chi, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(Chi), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
aggr_plot <- aggr(ChiData_1, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(ChiData_1), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


#CREATING FACTOR VARIABLES INCLUDING NA as false
Electoral_Violence<-ifelse(RPR1$nelda33==1,"PRESENT", "ABSENT")
RPR1$NIRCs<-ifelse(RPR1$NIRCPresent==1,"PRESENT", "ABSENT")
ChiData_1$multiparty <- ifelse(ChiData_1$multiparty==1, TRUE, FALSE)
ChiData_1$PKO <- ifelse(ChiData_1$PKO==1, TRUE, FALSE)
ChiData_1$harass <- ifelse(ChiData_1$harass==1, TRUE, FALSE)
ChiData_1$eco_growth <- ifelse(ChiData_1$eco_growth==1, TRUE, FALSE)
ChiData_1$compete <- ifelse(ChiData_1$compete==1, TRUE, FALSE)
ChiData_1$riots <- ifelse(ChiData_1$riots==1, TRUE, FALSE)
ChiData_1$monitors <- ifelse(ChiData_1$monitors==1, TRUE, FALSE)
ChiData_1$TypeCoded <- ifelse(ChiData_1$TypeCoded==1, TRUE, FALSE)
ChiData_1$Peacebuilding_Interventions <- ifelse(ChiData_1$PeaceActivities==1, "PRESENT", "ABSENT")
table(Electoral_Violence)
table(RPR1$NIRCs)
table(ChiData_1$PowerIndex1)

######################  CONTROL VARIABLE DESCRIPTIVES  ############
Matched13$group <- as.factor(ifelse(Matched13$PowerIndex1 == 2.2, 'VH', 
                                    ifelse(Matched13$PowerIndex1 == 2.0, 'H', 
                                           ifelse(Matched13$PowerIndex1 == 1.8, 'M',
                                                  ifelse(Matched13$PowerIndex1 == 1.6,'L', 'VL')))))
View(NELDA)
library(haven)
require(ggplot2 + ggtit)
install.packages("ggthemes")
install.packages(ggtit)
require(ggthemes)
library(ggplot2)
ggplot2::ggplot(data = RPR1) + geom_bar(aes(x=NIRCs),fill="orange") + labs(title = "Figure 4: Histogram of NIRCs (1992-2019)") + theme(text = element_text(size = 18))

ggplot2::ggplot(data = RPR1) + geom_bar(aes(x=NIRCs)) + labs(title = "Figure 4: Histogram of NIRCs (1992-2019)", subtitle="434 Electoral Rounds", 
                                                             caption="Source: NELDA V.6 /Joanna Grace Nakabiito") + theme(text = element_text(size = 18)) + theme(text = element_text(size = 18))
ggplot(data = RPR1) + geom_bar(aes(x=Electoral_Violence, fill=NIRCs)) + labs(title = "Bar Plot of Electoral Violence & Presence of NIRCs ")
labs(title="Weight histogram plot",x="Weight(kg)", y = "Count")
ggplot2::ggplot(data = RPR1) + geom_bar(aes(x=Electoral_Violence)) + labs(title = "Figure 3: Histogram of Significant Electoral Violence (1992-2019)",subtitle="434 Electoral Rounds", 
                                                                          caption="Source: NELDA V.6") + theme(text = element_text(size = 18))
                                                                                                                                                                 
ggplot2::ggplot(data = RPR1) + geom_bar(aes(x=harass)) + labs(title = "Figure 3: Histogram of Opposition Harassment")
ggplot2::ggplot(data = RPR1) + geom_bar(aes(x=eco_growth)) + labs(title = "Figure 3: Histogram of Economic Growth")
ggplot2::ggplot(data = RPR1) + geom_bar(aes(x=compete)) + labs(title = "Figure 3: Histogram of Electoral Competition")
ggplot2::ggplot(data = RPR1) + geom_bar(aes(x=riots)) + labs(title = "Figure 3: Histogram of Riots and Protests")
ggplot2::ggplot(data = RPR1) + geom_bar(aes(x=monitors)) + labs(title = "Figure 3: Histogram of International Monitors")
ggplot2::ggplot(data = RPR1) + geom_bar(aes(x=PKO)) + labs(title = "Figure 3: Histogram of PeaceKeeping Administrative Units")
ggplot2::ggplot(data = ChiData_1) + geom_bar(aes(x=Power_Cat)) + labs(title = "Figure 3: Histogram of NIRCs'Power")

ggplot2::ggplot(data = ChiData_1) + geom_bar(aes(x=PowerIndex1)) + labs(title = "Figure 3: Histogram of NIRCs' Power")

#include number of observations following subsetting
ggplot2::ggplot(data = ChiData_1) + geom_bar(aes(x=Peacebuilding_Interventions)) + labs(title = "Figure 5: Histogram of Electoral-Related Peacebuilding Interventions",subtitle="171 Electoral Rounds", 
                                                                             caption="Source: Joanna Grace Nakabiito/ NELDA V.6") + theme(text = element_text(size = 18))
ggplot2::ggplot(data = Matched13) + geom_bar(aes(x=PowerIndex)) + labs(title = "Histogram of NIRCs' Social Power",subtitle="171 Electoral Rounds", caption="Source: Joanna Grace Nakabiito/ NELDA V.6") + theme(text = element_text(size = 18))

#Checking skewness 
install.packages("moments")
library(moments)
skewness(ChiData_1$PowerIndex2)
kurtosis(ChiData_1$PowerIndex2)
# However, PowerIndex1 it is turned into a categorical variable.

summary(ChiData_1$PowerIndex1)
hist(ChiData_1$PowerIndex1)
Power<-3
summary(ChiData_1$PowerIndex1)
Power[ChiData_1$PowerIndex1>=2.000]<-2
Power[ChiData_1$PowerIndex1>=1.600]<-1
Power[ChiData_1$PowerIndex1<=1.400]<-0
hist(ChiData_1$Power)

Power_Cat<-factor(ChiData_1$Power,c(0,1,2),labels=c('Low','Medium','High'))
plot(Power_Cat)

############# DESCRIPTIVE STATISTICS ############


summary(ChiData_1$NELDA1.SubsidiaryOrg)
summary(ChiData_1$PeaceActivities)
summary(ChiData_1$PowerIndex1)
summary(ChiData_1$NELDA1.LegPower)
summary(ChiData_1$NELDA1.ExpertPower)
summary(ChiData_1$NELDA1.Affiliation)
summary(ChiData_1$NELDA1.LegPower)
summary(ChiData_1$NELDA1.Web)
summary(ChiData_1$NELDA1.SocialMedia)
summary(ChiData_1$NELDA1.SubsidiaryOrg)


library(survival)
#FULL MATCHING
match.out10 <- MatchIt::matchit(NIRCPresent~PKO + monitors + riots + compete + eco_growth + harass + multiparty, RPR1, method = "full")
summary(match.out10)
Matched10<-MatchIt::match.data(match.out10, "all")
match.weight1<-MatchIt::match.data(match.out10, "all")$weights
View(Matched10)

#Plot the propensity scores
plot(match.out10, type = "hist")
plot(match.out10, type = "jitter")

#binary regression after matching results for Significant Violence ######## MAIN REGRESSION

NIRCPresent<-as.factor(Matched10$NIRCPresent)
eco_growth<-as.factor(Matched10$eco_growth)
riots<-as.factor(Matched10$riots)
compete<-as.factor(Matched10$compete)
monitors<-as.factor(Matched10$monitors)
PKO<-as.factor(Matched10$PKO)
multiparty<-as.factor(Matched10$multiparty)
nelda33<-as.factor(Matched10$nelda33)


Mod1<- glm(formula = nelda33 ~ NIRCPresent, family = binomial(link = "logit"), data = RPR1)
Mod2<- glm(formula = nelda33 ~ NIRCPresent + PKO + compete + harass + riots + monitors + eco_growth+multiparty, family = binomial(link = "logit"), data = RPR1)
Mod3<- glm(formula = nelda33 ~ NIRCPresent, family = binomial(link = "logit"), data = Matched10, weights = match.weight1)
Mod4<- glm(formula = nelda33 ~ NIRCPresent + PKO + compete + harass + riots + monitors + eco_growth + multiparty, family = binomial(link = "logit"), data = Matched10, weights = match.weight1)
stargazer::stargazer(Mod1, Mod2, Mod4, type = "text")

stargazer::stargazer(Mod1, Mod2, Mod4, title = "Baseline: Likelihood of Electoral Violence given NIRC Presence", dep.var.caption = "DV: Electoral Violence in Africa, 1990-2019", covariate.labels =  c("NIRC Presence"), notes.label = "Significance levels", type = "text", 
                     out = "C:/Users/nakab/Documents/UNDESA UNV/H1FINAL7.htm")

#ODDS RATIO FOR RPR1
#Confidence Intervals for the Regression Slopes
confint.default(Mod4)
#Goodness of fit for Models Power6 and Power 8
AIC(Mods,Mod4)

anova(Mod4, test = "LRT")
anova(Mod1, Mod2, Mod4, test = "LRT")
#Odds Ratio
exp(Mod4$coefficients[-1])
# interpretation is that the odds of electoral rounds with peace activities having significant electoral violence are smaller by a factor of 0.456 that electoral rounds with activities acbsent.
#Odds ratio by a percentage
(exp(Mod4$coefficients[-1])-1)*100
# interpretation is that the odds of electoral rounds with peace activities having significant electoral violence are 54% smaller than electoral rounds with activities absent.
Mods<- glm(formula = nelda33 ~ 1, family = binomial(link = "logit"), data = Matched10, weights = match.weight1)
summary(Mods)
anova(Mods, Mod4, test = "LRT")


d8<- robustbase::glmrob(formula = nelda33 ~ NIRCPresent + PKO + compete + harass + riots + monitors + eco_growth, family = binomial(link = "logit"), data = Matched10, weights = match.weight1)

car::vif(Mod4)

## ROBUSTNESS CHECK WITH ELIMINATION OF CONTROLS
#NO PKO
match.out11 <- MatchIt::matchit(NIRCPresent~riots + eco_growth + harass + monitors + eco_growth + compete, RPR1, method = "full")
Matched11<-MatchIt::match.data(match.out11, "all")
match.weight11<-MatchIt::match.data(match.out11, "all")$weights

Mod5<- glm(formula = nelda33 ~ NIRCPresent + compete + harass + riots + monitors + eco_growth + multiparty, family = binomial(link = "logit"), data = Matched11, weights = match.weight11)

#No Monitors
match.out17 <- MatchIt::matchit(NIRCPresent~riots + eco_growth + harass + eco_growth + compete, RPR1, method = "full")
Matched17<-MatchIt::match.data(match.out17, "all")
match.weight17<-MatchIt::match.data(match.out17, "all")$weights
Mod6<- glm(formula = nelda33 ~ NIRCPresent + compete + harass + riots + eco_growth + multiparty, family = binomial(link = "logit"), data = Matched17, weights = match.weight17)

#No multiparty regime
match.out18 <- MatchIt::matchit(NIRCPresent~riots + eco_growth + harass + eco_growth, RPR1, method = "full")
Matched18<-MatchIt::match.data(match.out18, "all")
match.weight18<-MatchIt::match.data(match.out18, "all")$weights
Mod7<- glm(formula = nelda33 ~ NIRCPresent + compete + harass + riots + eco_growth, family = binomial(link = "logit"), data = Matched18, weights = match.weight18)
stargazer::stargazer(Mod4, Mod5, Mod6, Mod7, type = "text")

stargazer::stargazer(Mod4, Mod5, Mod6, Mod7, title = "Baseline: Likelihood of Electoral Violence given NIRC Presence", dep.var.caption = "DV: Electoral Violence in Africa, 1990-2019", covariate.labels =  c("NIRC Presence"), notes.label = "Significance levels", type = "text"
                     , out = "C:/Users/nakab/Documents/UNDESA UNV/H1ROBCTRL.htm")
AIC(Mod4, Mod5, Mod6, Mod7)

############ DIAGNOSTIC TEST ##########
library(car)
plot(Mod4)
residualPlots(Mod4)
influencePlot(Mod4)
influenceIndexPlot(Mod4)
outlierTest(Mod4)
rm(NO_Outliers2)
library(dplyr)
NO_Outliers2<-RPR1 %>% 
  dplyr::slice(-c(500,351,50))
match.out12 <- MatchIt::matchit(NIRCPresent~PKO + monitors + riots + compete + eco_growth + harass + multiparty, NO_Outliers2, distance = "logit", method = "full")
Matched12<-MatchIt::match.data(match.out12, "all")
match.weight12<-MatchIt::match.data(match.out12, "all")$weights

Mod9<-glm(formula = nelda33 ~ NIRCPresent, family = binomial(link = "logit"), data = NO_Outliers2)
Mod10<-glm(formula = nelda33 ~ NIRCPresent + PKO + compete + harass + riots + monitors + eco_growth + multiparty, family = binomial(link = "logit"), data = NO_Outliers2)
Mod11<-glm(formula = nelda33 ~ NIRCPresent, family = binomial(link = "logit"), data = Matched12, weights = match.weight12)
Mod12<-glm(formula = nelda33 ~ NIRCPresent + PKO + compete + harass + riots + monitors + eco_growth + multiparty, family = binomial(link = "logit"), data = Matched12, weights = match.weight12)
stargazer::stargazer(Mod9, Mod10, Mod11, Mod12, type = "text")
stargazer::stargazer(Mod9, Mod10, Mod12, title = "Baseline: Likelihood of Electoral Violence given NIRC Presence", dep.var.caption = 
                       "DV: Electoral Violence in Africa, 1990-2019", covariate.labels =  c("NIRC Presence"), notes.label = "Significance levels", type = "text"
                     , out = "C:/Users/nakab/Documents/UNDESA UNV/H1ROBUST.htm")
#FOR ROBUSTNESS NO SM
Matched13$Powerf <- as.factor(ifelse(Matched13$st_PowerNoSM <= -0.9354469, 'A', 'B'))


# MULTICOLLINEARITY TEST AND PLOT
library(sjmisc)
library(sjPlot)
install.packages("car") 
install.packages("corrplot")
library("AER")
library(MASS)
myvars<- c("NIRCPresent", "riots", "multiparty", "compete", "harass", "eco_growth", "PKO", "monitors")
newdata<- RPR1[myvars]
#calculating inverted correlation matrix...IMPUTATION REMOVES MULTICOLLINEARITY
#https://www.tutorialspoint.com/how-to-change-the-size-of-correlation-coefficient-value-in-correlation-matrix-plot-using-corrplot-in-r
# VIDEO to conduct it: https://www.youtube.com/watch?v=bMKeGQvWdFE
newdatacori<-ginv(cor(newdata))
colnames(newdatacori)<-colnames(newdata)
rownames(newdatacori)<-colnames(newdata)
newdatacori
corrplot::corrplot(corr = newdatacori, method = "number", is.corr = FALSE, addCoef.col="black", number.cex=1)

##CHECKING MULTICOLLINEARITY WITH VIF: LOOKS GOOD
car::vif(Mod4)











############# POWER ANALYSIS ######################
#Creating Categorical Variable

# https://www.youtube.com/watch?v=TJKskHT_zQs

stargazer::stargazer(Power1,Power2,Power3, title = "Table 8: The moderation effect of Power on NIRCs' Peace Activiies on Electoral Violence", dep.var.caption = "DV: Significant Electoral Violence in Africa, 1992-2019", covariate.labels =  c("Peace Activities"), notes.label = "Significance levels", type = "text"
                     , out = "C:/Users/nakab/Documents/UNDESA UNV/GU.htm")

summary(ChiData_1$PeaceActivities)
summary(ChiData_1$Power_Cat)
summary(ChiData_1$NELDA1.LegPower)
summary(ChiData_1$NELDA1.ExpertPower)
summary(ChiData_1$NELDA.Affiliation)
summary(ChiData_1$NELDA.LegPower)
summary(ChiData_1$NELDA.Web)
summary(ChiData_1$NELDA1.SocialMedia)
summary(ChiData_1$NELDA1.SubsidiaryOrg)
Activities<-factor(ChiData_1$PeaceActivities,c(0,1),labels=c('Absent','Present'))

###PEACE REGRESSION########
match.out13 <- MatchIt::matchit(PeaceActivities~harass+eco_growth+compete+riots+monitors+PKO+TypeCoded, ChiData_1, method = "full")
Matched13<-MatchIt::match.data(match.out13, "all")
match.weight<-MatchIt::match.data(match.out13, "all")$weights



#Before Matching
Peace1 <- glm(nelda33 ~ Activities, data = ChiData_1, family = binomial(link = "logit"))
Peace2<- glm(nelda33 ~ Activities+harass+eco_growth+compete+riots+monitors+PKO+TypeCoded, data = ChiData_1, family = binomial(link = "logit"))
#After Matching
Peace3 <- glm(nelda33 ~ Activities, data = Matched13, family = binomial(link = "logit"), weights = match.weight)
Peace4 <- glm(nelda33 ~ Activities+harass+eco_growth+compete+riots+monitors+PKO+TypeCoded, data = Matched13, family = binomial(link = "logit"), weights = match.weight)
stargazer::stargazer(Peace1,Peace2, Peace3, Peace4, title = "Table 7: The Effect of NIRCs' Peacebuilding Activities on Electoral Violence Restraint", dep.var.caption = "DV: Significant Electoral Violence in Africa, 1992-2020", covariate.labels =  c("Peace Activities"), notes.label = "Significance levels", type = "text")


interactions::cat_plot(Power3, pred = PeaceActivities, modx = Power, point.shape = TRUE,geom = "line", colors="Rainbow")

##POWER REGRESSION#########

#Wilcoxon rank sum test with continuity correction
ChiData_1$Power=ifelse(ChiData_1$PowerIndex1>=2.00,2,
                       ifelse(ChiData_1$PowerIndex1<=1.400,0,1))
ChiData_1$Power<-as.factor(ChiData_1$Power)
ChiData_1$PeaceActivities<-as.numeric(ChiData_1$PeaceActivities)
ChiData_1$nelda33<-as.numeric(ChiData_1$nelda33)
wilcox.test(Power_Index~PeaceActivities, ChiData_1)
# data:  Power_Cat by PeaceActivities
# W = 1800.5, p-value = 5.267e-07
# alternative hypothesis: true location shift is not equal to 0
wilcox.test(Power_Index~nelda33, ChiData_1)
# alternative hypothesis: true location shift is not equal to 0

#MOSAIC PLOT
#Recoding PowerIndex1
summary(PowerIndex1)
Matched13$Power=ifelse(Matched13$Power=="A",0,1)
table(ChiData_1$Power)
plot(ChiData_1$Power)
print(Power)
hist(ChiData_1$PowerIndex1)
summary(ChiData_1$PowerIndex1)

# Recoding variables to factors and labeling them
Power_Cat<-factor(ChiData_1$Power,c(0,1,2),labels=c('Low','Medium','High'))
Peace_Activities<-factor(ChiData_1$PeaceActivities,c(0,1),labels=c('Absent','Present'))
SignificantViolence<-factor(ChiData_1$nelda33,c(0,1),labels=c('Low','High'))

#Mosaic plot power by peace activities
Power_PeaceActivities <- mosaic::tally(Power_Cat~ Peace_Activities, data = ChiData_1)
mosaicplot(Power_PeaceActivities, color = TRUE)

#Mosaic plot power by significant violence
PowerxSignificantViolence <- mosaic::tally(Power_Cat~ SignificantViolence, data = ChiData_1)
mosaicplot(PowerxSignificantViolence, color = TRUE)


#Before Matching
Matched13$Power<-as.factor(Matched13$Power)
match.out13 <- MatchIt::matchit(Power~harass+eco_growth+compete+riots+monitors+PKO+TypeCoded, ChiData_1, method = "full")
Matched13<-MatchIt::match.data(match.out13, "all")
match.weight<-MatchIt::match.data(match.out13, "all")$weights
Power0<- glm(nelda33 ~ PeaceActivities+harass+eco_growth+compete+riots+monitors+PKO+multiparty, data = Matched13, family = binomial(link = "logit"), weights = match.weight)
#Peace Activity and Interaction Term
Power0.1<- glm(nelda33 ~ Power*PeaceActivities+harass+eco_growth+compete+riots+monitors+PKO+multiparty, data = Matched13, family = binomial(link = "logit"), weights = match.weight)

stargazer::stargazer(Power0,Power0.1, title = "Table 8: Religion, Power & Restraint", dep.var.caption = "DV: Significant Electoral Violence in Africa, 1992-2020", covariate.labels =  c("Peace Activities"), notes.label = "Significance levels", type = "text")

rm(mtcars)
data(mtcars)
view(mtcars)
summary(mtcars$disp)
mtcars$disp=ifelse(mtcars$disp>326,2,
                       ifelse(mtcars$disp<120.8,0,1))

view(mtcars$disp)
plot(mtcars$disp)
hist(mtcars$disp)

sjPlot::plot_model(Power3, type = "int")+theme_bw() 


summary(ChiData_1$PowerIndex1)
hist(ChiData_1$PowerIndex1)
#Creating Categorical Variable
PI3<-3
summary(PI3)
PI3[ChiData_1$PowerIndex2<=2.200]<-3
PI3[ChiData_1$PowerIndex2<=1.800]<-2
PI3[ChiData_1$PowerIndex2<=1.400]<-1

PIndex1<-factor(PI3,c(1,2,3),labels=c('Low','Medium','High'))
#Peace Activity full
Power7<- glm(nelda33 ~ Activities+harass+eco_growth+compete+riots+monitors+PKO+TypeCoded, data = Matched13, family = binomial(link = "logit"), weights = match.weight)
#Peace Activity and power Full
Power8 <- glm(nelda33 ~ Activities+PIndex1+harass+eco_growth+compete+riots+monitors+PKO+TypeCoded, data = Matched13, family = binomial(link = "logit"), weights = match.weight)
#Peace Activity and Interaction Term
Power9<- glm(nelda33 ~ Activities*PIndex1+harass+eco_growth+compete+riots+monitors+PKO+TypeCoded, data = Matched13, family = binomial(link = "logit"), weights = match.weight)
stargazer::stargazer(Power7, Power9, title = "Table 8: Religion, Power & Restraint", dep.var.caption = "DV: Significant Electoral Violence in Africa, 1992-2020", covariate.labels =  c("Peace Activities"), notes.label = "Significance levels", type = "text")





summary(ChiData_1$PowerIndex1)
hist(ChiData_1$PowerIndex1)
#Creating Categorical Variable
PI3<-3
PI3[ChiData_1$PowerIndex1<=2.2]<-2
PI3[ChiData_1$PowerIndex1<=1.8]<-1
PI3[ChiData_1$PowerIndex1<=1.4]<-0

PIndex1<-factor(PI3,c(0,1,2),labels=c('Low','Medium','High'))
#Peace Activity full
Power7<- glm(nelda33 ~ Activities+harass+eco_growth+compete+riots+monitors+PKO+TypeCoded, data = Matched13, family = binomial(link = "logit"), weights = match.weight)
#Peace Activity and power Full
Power8 <- glm(nelda33 ~ Activities+PIndex1+harass+eco_growth+compete+riots+monitors+PKO+TypeCoded, data = Matched13, family = binomial(link = "logit"), weights = match.weight)
#Peace Activity and Interaction Term
Power9<- glm(nelda33 ~ Activities*PIndex1+harass+eco_growth+compete+riots+monitors+PKO+TypeCoded, data = Matched13, family = binomial(link = "logit"), weights = match.weight)
stargazer::stargazer(Power7,Power8, Power9, title = "Table 8: Religion, Power & Restraint", dep.var.caption = "DV: Significant Electoral Violence in Africa, 1992-2020", covariate.labels =  c("Peace Activities"), notes.label = "Significance levels", type = "text")





Powerr<- glm(nelda33 ~ PeaceActivities*group, data = Matched13, family = binomial(link = "logit"), weights = match.weight)
interactions::cat_plot(Powerr, pred = PeaceActivities, modx = group, geom = "line", point.shape = TRUE,
                       colors = "Set2", interval=TRUE)




#Confidence Intervals for the Regression Slopes
confint.default(Power8)
#Goodness of fit for Models Power6 and Power 8
anova(Power6, test = "LRT")
anova(Power6, Power8, test = "LRT")
#Odds Ratio
exp(Power8$coefficients[-1])
# interpretation is that the odds of electoral rounds with peace activities having significant electoral violence are smaller by a factor of 0.456 that electoral rounds with activities acbsent.
#Odds ratio by a percentage
(exp(Power8$coefficients[-1])-1)*100
# interpretation is that the odds of electoral rounds with peace activities having significant electoral violence are 54% smaller than electoral rounds with activities absent.

#Interaction between Power and Peace Activities
#Confidence Intervals for the Regression Slopes
confint.default(Power10)
#Goodness of fit for Models Power6 and Power 8
anova(Power10, test = "LRT")
anova(Power8, Power10, test = "LRT")
#Odds Ratio
exp(Power10$coefficients[-1])
# interpretation is that the odds of electoral rounds with peace activities having significant electoral violence are smaller by a factor of 0.456 that electoral rounds with activities acbsent.
#Odds ratio by a percentage
(exp(Power10$coefficients[-1])-1)*100

Powerr<- glm(nelda33 ~ PeaceActivities*group, data = Matched13, family = binomial(link = "logit"), weights = match.weight)
interactions::cat_plot(Powerr, pred = PeaceActivities, modx = group, geom = "line", point.shape = TRUE,
                       colors = "Set2", interval=TRUE)
#Probe interaction
interactions::cat_plot(Power2, pred = PeaceActivities, modx = Power_Cat, mod2 = NULL, alpha=-2.02, mod2.values = NULL, jtools::get_colors(colors, length(modx.labels), gradient = NULL))

#Categorical interaction. Variables must be categorical
#https://www.rdocumentation.org/packages/sjPlot/versions/2.8.10/topics/tab_model
#https://yury-zablotski.netlify.app/post/multiple-logistic-regression-with-interactions/
Matched13$Power_Cat<-as.factor(Matched13$Power_Cat)
Matched13$PeaceActivities<-as.factor(Matched113$PeaceActivities)
Power_NIRCs<-factor(Matched13$Power_Cat,c(0,1,2),labels=c('Low','Medium', 'High'))
Peace_Activities<-factor(Matched13$PeaceActivities,c(0,1),labels=c('Activity Absent','Activity Present'))
Exp_Power<-factor(Matched13$ExpertPower,c(1,2),labels=c('Low','High'))

Power81 <- glm(nelda33 ~ Peace_Activities*Power_NIRCs, data = Matched13, family = binomial(), weights = match.weight)
jtools::summ(Power81)
sjPlot::tab_model(Power10, p.style = "numeric_stars", transform = NULL, p.threshold = c(0.1, 0.05, 0.01, 0.001))
interactions::cat_plot(Power81, pred = PeaceActivities, modx = Power0, geom = "line", plot.shape = T)
sjPlot::plot_model(Power10, transform = NULL, show.intercept = T, show.values = TRUE, value.offset = .3, p.style = "asterix" , p.threshold = .1,insig.color = "#F8766D", sig.color = "#00BFC4")
install.packages(effects)
emmeans::emmip(Power10, Peace_Activities ~ Power_NIRCs, CIs=TRUE, plotit=T)+ggplot2::theme_bw()

anova(Power10, test = "Chisq")

#Variables must be numericfor this Johnson Neyman probe
Matched131$Power_Cat<-as.numeric(Matched131$Power_Cat)
Matched13$PeaceActivities<-as.numeric(Matched13$PeaceActivities)
Power81 <- glm(nelda33 ~ PeaceActivities*Power_Cat, data = Matched13, family = binomial(), weights = match.weight)
ss<-interactions::sim_slopes(Power81, pred =PeaceActivities,modx =Power_Cat,johnson_neyman =TRUE, control.fdr =TRUE,cond.int = TRUE, robust ="HC3", centered = "all", jnalpha =0.05)
interactions::johnson_neyman(Power81,pred =PeaceActivities, modx =Power_Cat,control.fdr =FALSE, alpha =.05, mod.range = NULL, df = "residual", insig.color = "#F8766D", sig.color = "#00BFC4")
interactions::johnson_neyman(Power81,pred =PeaceActivities, modx =Power_Cat)

#INSTRUMENTAL VARIABLE FOR ENDOGENEITY:
#Using IV GLM: https://www.quantargo.com/help/r/latest/packages/ivtools/2.3.0/ivglm
#L is the control variable
#Below, Z, X, and Y are the instrument, the exposure, and the outcome, respectively. L 
#is a vector of covariates that we wish to control for in the analysis; these would typically be confounders for the instrument and the outcome.

library(ivtools)
attach(RPR1)
fitX.LZ <- glm(formula = NIRCPresent ~ BRD_BEST + Rel_PolRes, family = binomial(link = "logit"), data = RPR1)
fitY.LX <- glm(formula = nelda33 ~ BRD_BEST + NIRCPresent, data = RPR1)
fitIV <- ivtools::ivglm(estmethod = "g", fitX.LZ = fitX.LZ, fitY.LX = fitY.LX, data = RPR1, ctrl = TRUE)
summary(fitIV)
print(fitIV)
stargazer::stargazer(fitIV, se=list, type = "text")
confint(fitIV)

Matched13$Poweri <- as.factor(ifelse(Matched13$st_PowerIndex1 <= -0.9368670, 'B', 'A'))


Power1<- glm(nelda33 ~ PeaceActivities+harass+eco_growth+compete+riots+monitors+PKO+multiparty, data = Matched13, family = binomial(link = "logit"), weights = match.weight)
Power2<- glm(nelda33 ~ PeaceActivities*Poweri+harass+eco_growth+compete+riots+monitors+PKO+multiparty, data = Matched13, family = binomial(link = "logit"), weights = match.weight)
Power3<- glm(nelda33 ~ PeaceActivities*Poweri+harass+eco_growth+compete+riots+monitors+PKO+multiparty, data = Matched13, family = binomial(link = "logit"), weights = match.weight)
stargazer::stargazer(Power1,Power2,Power3, title = "Table 8: The moderation effect of Power on NIRCs' Peace Activiies on Electoral Violence", dep.var.caption = "DV: Significant Electoral Violence in Africa, 1992-2019", covariate.labels =  c("Peace Activities"), notes.label = "Significance levels", type = "text")

#Because the weights are not integers I cannot use the binomial family. I have to use Quasi binomial...nope

#LINKS
#https://www.youtube.com/watch?v=ARfXDSkQf1Y
#https://www.youtube.com/watch?v=mAD5vwdtx0o Odds ratios

#Robustness check
#Page 133

#https://books.google.se/books?id=CC0NEAAAQBAJ&pg=PA133&dq=robustness+check+methods&hl=en&sa=X&ved=2ahUKEwi_t6G_3-X4AhUgQfEDHa2NAzgQuwV6BAgJEAY#v=onepage&q=robustness%20check%20methods&f=false
