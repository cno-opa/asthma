# Asthma Analytics Project

# Packages Used
library(car)
library(gdata)
library(psych)
library(plyr)
library(dplyr)
library(lmtest)
library(ROCR)

# Importing csv file
household = read.csv("household.csv",na.strings=c(-9,-6,"-9","-6","'-9'","'-6'"),stringsAsFactors = FALSE)

# Keeping selected variables
ahs2015 <- subset(household,select=c(CONTROL,ASTHMA,HHRACE,HHGRAD,HINCP,FS,PERPOVLVL,POVLVLINC))
ahs2015 <- na.omit(ahs2015)

# Printing data
head(ahs2015, n=10)

# Recoding Asthma diagnosis
ahs2015$asthma.diag[ahs2015$ASTHMA =="'2'"] <- '0'
ahs2015$asthma.diag[ahs2015$ASTHMA =="'1'"] <- '1'

# Collapsing Race into White vs. Non-White
ahs2015$race[ahs2015$HHRACE=="'01'"] <- "0"
ahs2015$race[ahs2015$HHRACE!="'01'"] <- "1"

# Collapsing Education into Five Categories
ahs2015$educ[ahs2015$HHGRAD =="'3'1"] <- "1"
ahs2015$educ[ahs2015$HHGRAD =="'32'"] <- "1"
ahs2015$educ[ahs2015$HHGRAD =="'33'"] <- "1"
ahs2015$educ[ahs2015$HHGRAD =="'34'"] <- "1"
ahs2015$educ[ahs2015$HHGRAD =="'35'"] <- "1"
ahs2015$educ[ahs2015$HHGRAD =="'36'"] <- "1"
ahs2015$educ[ahs2015$HHGRAD =="'37'"] <- "1"
ahs2015$educ[ahs2015$HHGRAD =="'38'"] <- "1"
ahs2015$educ[ahs2015$HHGRAD =="'39'"] <- "2"
ahs2015$educ[ahs2015$HHGRAD =="'40'"] <- "3"
ahs2015$educ[ahs2015$HHGRAD =="'41'"] <- "3"
ahs2015$educ[ahs2015$HHGRAD =="'42'"] <- "4"
ahs2015$educ[ahs2015$HHGRAD =="'43'"] <- "4"
ahs2015$educ[ahs2015$HHGRAD =="'44'"] <- "4"
ahs2015$educ[ahs2015$HHGRAD =="'45'"] <- "5"
ahs2015$educ[ahs2015$HHGRAD =="'46'"] <- "5"
ahs2015$educ[ahs2015$HHGRAD =="'47'"] <- "5"

# Food Stamps Recipiency
ahs2015$food[ahs2015$FS =="'2'"] <- '0'
ahs2015$food[ahs2015$FS =="'1'"] <- '1'

# Recoding Total Household Income according to ACS 2016 
incgrp <- cut(ahs2015$HINCP, 
breaks = c(-Inf, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 
45000, 50000, 60000, 75000, 100000, 125000, 150000, 200000, Inf), 
labels = c("<10,000","10,000-14,999","15,000-19,999","20,000-24,999",
"25,000-29,999","30,000-34,999","35,000-39,999","40,000-44,999",
"45,000-49,999","50,000-59,999","60,000-74,999","75,000-99,999",
"100,000-124,999","125,000-149,999","150,000-199,999",">200,000"), 
right = FALSE)

# Checking data
# head(ahs2015, n=10)
# str(ahs2015)

# Summary Statistics for Continuous Variables
describe(ahs2015$HINCP)
t.test(ahs2015$HINCP~asthma.diag)
describe(ahs2015$PERPOVLVL)
t.test(ahs2015$PERPOVLVL~asthma.diag)

# 2-Way Frequency Tables for Categorical Variables
attach(ahs2015)
raceByASTHMA <- table(race,asthma.diag)
incgrpByASTHMA <- table(incgrp,asthma.diag)
educationByASTHMA <- table(educ,asthma.diag)
foodstampsByASTHMA <- table(food,asthma.diag)

# Null model
null <- glm(asthma.diag~., family=binomial, data=ahs2015)

# One predictor model with race of householder (race)
race.binary <-glm(asthma.diag~race, family=binomial) 

# One predictor model with education
education <-glm(asthma.diag~educ, family=binomial) 

# One predictor model with income group
income <-glm(asthma.diag~incgrp, family=binomial) 

# One predictor model with food stamps
foodstamps <-glm(asthma.diag~food, family=binomial)  
 
# FME model
fme <-glm(asthma.diag~race+educ+food, family=binomial) 

# Model selection
forward = step(null, scope = list(lower = formula(null), upper = formula(sat)), direction = "forward", data=ahs2015)
backwards = step(sat)
nondirectional = step(null, list(lower=formula(null), upper=formula(sat)), direction="both", trace=0)

# Selected model
selected <-glm(asthma.diag~race+food, family=binomial) 
summary(selected)
vcov(selected)
logLik(selected)

# Saturated model
saturated = glm(asthma.diag~race+food+race*food, family=binomial)

# Goodness of Fit test
lrtest(selected, saturated)

# Probability calculation
prob <- predict(selected, type = 'response')
summary(prob)

# Confusion matrix
table(ahs2015$asthma.diag, prob>0.16)

# ROC curve
ROCpred <- prediction(prob, ahs2015$asthma.diag)
ROC <- performance(ROCpred,'tpr','fpr')
plot(ROC, text.adj = c(-0.2,1.7))

# Calculating AUC
auc <- performance(ROCpred, 'auc')
auc <- unlist(slot(auc, "y.values"))
minauc<-min(round(auc, digits = 2))
maxauc<-max(round(auc, digits = 2))
minauct <- paste(c("min(AUC)  = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
legend(0.3,1.1,c(minauct,maxauct,"\n"),border="white",cex=0.8,box.col = "white")

# Lift chart
LiftChart <- performance(ROCpred,'tpr','rpp')
