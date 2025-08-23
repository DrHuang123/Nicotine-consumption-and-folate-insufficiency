setwd('C:/Users/Weiyi/Downloads/survey')
library(haven)
library("survey")
library("jtools")
library("remotes")
library("svrepmisc")
library(graphPAF)
library(marginaleffects)
library(dplyr)  
library(purrr) 
library(EValue)


orgdata<-read_sas('final.sas7bdat')

orgdata$agecat <- NA
orgdata$agecat[orgdata$RIDAGEYR<25]<-1
orgdata$agecat[orgdata$RIDAGEYR>=25 & orgdata$RIDAGEYR<=34]<-2
orgdata$agecat[orgdata$RIDAGEYR>=35]<-3

orgdata$racecat <- NA
orgdata$racecat[orgdata$RIDRETH1==1]<-1
orgdata$racecat[orgdata$RIDRETH1==2]<-2
orgdata$racecat[orgdata$RIDRETH1==3]<-3
orgdata$racecat[orgdata$RIDRETH1==4]<-4
orgdata$racecat[orgdata$RIDRETH1==5]<-5

orgdata$educat <- NA
orgdata$educat[orgdata$DMDEDUC2==1|orgdata$DMDEDUC2==2|orgdata$DMDEDUC2==3]<-1
orgdata$educat[orgdata$DMDEDUC2==4|orgdata$DMDEDUC2==5]<-2

orgdata$incomecat <- NA
orgdata$incomecat[orgdata$INDFMPIR<1]<-1
orgdata$incomecat[orgdata$INDFMPIR>=1]<-2

orgdata$maritalcat <- NA
orgdata$maritalcat[orgdata$marital_cat==1]<-1
orgdata$maritalcat[orgdata$marital_cat==2|orgdata$marital_cat==3]<-2

orgdata$smoking <- NA
orgdata$smoking[orgdata$LBXCOT<1]<-1
orgdata$smoking[orgdata$LBXCOT>=1 & orgdata$LBXCOT<=10]<-2
orgdata$smoking[orgdata$LBXCOT>10]<-3

orgdata$RBCfolate <- NA
orgdata$RBCfolate[orgdata$LBDRBF>=400]<-0
orgdata$RBCfolate[orgdata$LBDRBF<400]<-1

orgdata$Serumfolate <- NA
orgdata$Serumfolate[orgdata$LBDFOLSI>=25.5]<-0
orgdata$Serumfolate[orgdata$LBDFOLSI<25.5]<-1

orgdata$smoking<-factor(orgdata$smoking)
orgdata$RBCfolate<-factor(orgdata$RBCfolate)

orgdata$agecat<-factor(orgdata$agecat)
orgdata$racecat<-factor(orgdata$racecat)
orgdata$educat<-factor(orgdata$educat)
orgdata$incomecat<-factor(orgdata$incomecat)
orgdata$maritalcat<-factor(orgdata$maritalcat)


dataset<-data.frame(orgdata$SEQN, orgdata$WTINT2YR, orgdata$SDMVSTRA, orgdata$SDMVPSU, orgdata$agecat, orgdata$racecat, orgdata$educat, orgdata$incomecat, orgdata$maritalcat, orgdata$smoking, orgdata$LBXCOT, orgdata$RBCfolate, orgdata$Serumfolate)

completedata<- na.omit(dataset)


########

table(completedata$orgdata.RBCfolate)
table(completedata$orgdata.agecat, completedata$orgdata.RBCfolate)
table(completedata$orgdata.racecat, completedata$orgdata.RBCfolate)
table(completedata$orgdata.maritalcat, completedata$orgdata.RBCfolate)
table(completedata$orgdata.educat, completedata$orgdata.RBCfolate)
table(completedata$orgdata.incomecat, completedata$orgdata.RBCfolate)


table(completedata$orgdata.smoking)
table(completedata$orgdata.agecat, completedata$orgdata.smoking)
table(completedata$orgdata.racecat, completedata$orgdata.smoking)
table(completedata$orgdata.maritalcat, completedata$orgdata.smoking)
table(completedata$orgdata.educat, completedata$orgdata.smoking)
table(completedata$orgdata.incomecat, completedata$orgdata.smoking)



nhc <- svydesign(id=~orgdata.SDMVPSU, weights=~orgdata.WTINT2YR, strata=~orgdata.SDMVSTRA, nest=TRUE, survey.lonely.psu = "adjust", data=completedata)


tab1<-svytable(~orgdata.agecat+orgdata.smoking, nhc)
prop.table(tab1, 1)
svychisq(~orgdata.agecat+orgdata.smoking, nhc, statistic="adjWald")


tab2<-svytable(~orgdata.racecat+orgdata.smoking, nhc)
prop.table(tab2, 1)
svychisq(~orgdata.racecat+orgdata.smoking, nhc, statistic="adjWald")


tab3<-svytable(~orgdata.maritalcat+orgdata.smoking, nhc)
prop.table(tab3, 1)
svychisq(~orgdata.maritalcat+orgdata.smoking, nhc, statistic="adjWald")

tab4<-svytable(~orgdata.educat+orgdata.smoking, nhc)
prop.table(tab4, 1)
svychisq(~orgdata.educat+orgdata.smoking, nhc, statistic="adjWald")


tab5<-svytable(~orgdata.incomecat+orgdata.smoking, nhc)
prop.table(tab5, 1)
svychisq(~orgdata.incomecat+orgdata.smoking, nhc, statistic="adjWald")


tab6<-svytable(~orgdata.agecat+orgdata.RBCfolate, nhc)
prop.table(tab6, 1)
svychisq(~orgdata.agecat+orgdata.RBCfolate, nhc, statistic="adjWald")


tab7<-svytable(~orgdata.racecat+orgdata.RBCfolate, nhc)
prop.table(tab7, 1)
svychisq(~orgdata.racecat+orgdata.RBCfolate, nhc, statistic="adjWald")


tab8<-svytable(~orgdata.maritalcat+orgdata.RBCfolate, nhc)
prop.table(tab8, 1)
svychisq(~orgdata.maritalcat+orgdata.RBCfolate, nhc, statistic="adjWald")


tab9<-svytable(~orgdata.educat+orgdata.RBCfolate, nhc)
prop.table(tab9, 1)
svychisq(~orgdata.educat+orgdata.RBCfolate, nhc, statistic="adjWald")


tab10<-svytable(~orgdata.incomecat+orgdata.RBCfolate, nhc)
prop.table(tab10, 1)
svychisq(~orgdata.incomecat+orgdata.RBCfolate, nhc, statistic="adjWald")



########

logit1 <- svyglm(orgdata.RBCfolate~orgdata.LBXCOT, family=quasibinomial, design=nhc, na.action = na.omit)
summary(logit1)
exp(logit1$coefficients*12.5)
exp(confint(logit1)*12.5)


logit2 <- svyglm(orgdata.RBCfolate~orgdata.LBXCOT+orgdata.agecat+orgdata.racecat+orgdata.educat+orgdata.maritalcat+orgdata.incomecat, family=quasibinomial, design=nhc, na.action = na.omit)
summary(logit2)
exp(logit2$coefficients*12.5)
exp(confint(logit2)*12.5)


########

logit3 <- svyglm(orgdata.RBCfolate~orgdata.smoking, family=quasibinomial, design=nhc, na.action = na.omit)
summary(logit3)
exp(logit3$coefficients)
exp(confint(logit3))


logit4 <- svyglm(orgdata.RBCfolate~orgdata.smoking+orgdata.agecat+orgdata.racecat+orgdata.educat+orgdata.incomecat+orgdata.maritalcat, family=quasibinomial, design=nhc, na.action = na.omit)
summary(logit4)
exp(logit4$coefficients)
exp(confint(logit4))


########


completedata$smoking_new[completedata$orgdata.LBXCOT<1]=0
completedata$smoking_new[completedata$orgdata.LBXCOT>10]=1

completedata2 <- na.omit(completedata)

nhc2 <- svydesign(id=~orgdata.SDMVPSU, weights=~orgdata.WTINT2YR,strata=~orgdata.SDMVSTRA, nest=TRUE, survey.lonely.psu = "adjust", data=completedata2)

logit5 <- svyglm(orgdata.RBCfolate~smoking_new+orgdata.agecat+orgdata.racecat+orgdata.educat+orgdata.incomecat+orgdata.maritalcat, family=quasibinomial, design=nhc2, na.action = na.omit)
avg_comparisons(model=logit5,comparison = 'lnratioavg',transform = exp, wts = weights(logit5))

evalues.RR(est = 1.31, lo = 1.12, hi = 1.54)
svyciprop(~I(smoking_new==1), nhc2, method="likelihood")
paf_levin(conf_prev=c(0.0915, 0.15), conf_RR=c(1.122, 1.535))


logit6 <- svyglm(orgdata.RBCfolate~smoking_new, family=quasibinomial, design=nhc2, na.action = na.omit)
avg_comparisons(model=logit6,comparison = 'lnratioavg',transform = exp, wts = weights(logit6))
paf_levin(conf_prev=c(0.0915, 0.15), conf_RR=c(1.322, 1.57))


######## unadjusted RR (folate ~ covariate)


logit7 <- svyglm(orgdata.RBCfolate~ orgdata.agecat, family=quasibinomial, design=nhc2, na.action = na.omit)
avg_comparisons(model=logit7,comparison = 'lnratioavg',transform = exp, wts = weights(logit7))


logit8 <- svyglm(orgdata.RBCfolate~ orgdata.racecat, family=quasibinomial, design=nhc2, na.action = na.omit)
avg_comparisons(model=logit8,comparison = 'lnratioavg',transform = exp, wts = weights(logit8))


logit9 <- svyglm(orgdata.RBCfolate~ orgdata.educat, family=quasibinomial, design=nhc2, na.action = na.omit)
avg_comparisons(model=logit9,comparison = 'lnratioavg',transform = exp, wts = weights(logit9))


logit10 <- svyglm(orgdata.RBCfolate~ orgdata.incomecat, family=quasibinomial, design=nhc2, na.action = na.omit)
avg_comparisons(model=logit10,comparison = 'lnratioavg',transform = exp, wts = weights(logit10))


logit11 <- svyglm(orgdata.RBCfolate~orgdata.maritalcat, family=quasibinomial, design=nhc2, na.action = na.omit)
avg_comparisons(model=logit11,comparison = 'lnratioavg',transform = exp, wts = weights(logit11))



########  unadjusted RR (somking ~ covariate)


logit12 <- svyglm(smoking_new~ orgdata.agecat, family=quasibinomial, design=nhc2, na.action = na.omit)
avg_comparisons(model=logit12,comparison = 'lnratioavg',transform = exp, wts = weights(logit12))


logit13 <- svyglm(smoking_new~ orgdata.racecat, family=quasibinomial, design=nhc2, na.action = na.omit)
avg_comparisons(model=logit13,comparison = 'lnratioavg',transform = exp, wts = weights(logit13))


logit14 <- svyglm(smoking_new~ orgdata.educat, family=quasibinomial, design=nhc2, na.action = na.omit)
avg_comparisons(model=logit14,comparison = 'lnratioavg',transform = exp, wts = weights(logit14))


logit15 <- svyglm(smoking_new~ orgdata.incomecat, family=quasibinomial, design=nhc2, na.action = na.omit)
avg_comparisons(model=logit15,comparison = 'lnratioavg',transform = exp, wts = weights(logit15))


logit16 <- svyglm(smoking_new~orgdata.maritalcat, family=quasibinomial, design=nhc2, na.action = na.omit)
avg_comparisons(model=logit16,comparison = 'lnratioavg',transform = exp, wts = weights(logit16))



######## dietary folate intake (subset analysis)

setwd("C:/Users/Weiyi/Downloads")
DR1TFOLA1 <- read_xpt("folate/DR1TOT_C.xpt")
DR1TFOLA2 <- read_xpt("folate/DR1TOT_D.xpt")
DR1TFOLA3 <- read_xpt("folate/DR1TOT_E.xpt")
DR1TFOLA4 <- read_xpt("folate/DR1TOT_F.xpt")
DR1TFOLA5 <- read_xpt("folate/DR1TOT_G.xpt")
DR1TFOLA6 <- read_xpt("folate/DR1TOT_H.xpt")
DR1TFOLA7 <- read_xpt("folate/DR1TOT_I.xpt")
DR1TFOLA8 <- read_xpt("folate/P_DR1TOT.xpt")


DR2TFOLA1 <- read_xpt("folate2/DR2TOT_C.xpt")
DR2TFOLA2 <- read_xpt("folate2/DR2TOT_D.xpt")
DR2TFOLA3 <- read_xpt("folate2/DR2TOT_E.xpt")
DR2TFOLA4 <- read_xpt("folate2/DR2TOT_F.xpt")
DR2TFOLA5 <- read_xpt("folate2/DR2TOT_G.xpt")
DR2TFOLA6 <- read_xpt("folate2/DR2TOT_H.xpt")
DR2TFOLA7 <- read_xpt("folate2/DR2TOT_I.xpt")
DR2TFOLA8 <- read_xpt("folate2/P_DR2TOT.xpt")


folate_files <- c(
  "folate/DR1TOT_C.xpt",
  "folate/DR1TOT_D.xpt",
  "folate/DR1TOT_E.xpt",
  "folate/DR1TOT_F.xpt",
  "folate/DR1TOT_G.xpt",
  "folate/DR1TOT_H.xpt",
  "folate/DR1TOT_I.xpt",
  "folate/P_DR1TOT.xpt"
)


folate2_files <- c(
  "folate2/DR2TOT_C.xpt",
  "folate2/DR2TOT_D.xpt",
  "folate2/DR2TOT_E.xpt",
  "folate2/DR2TOT_F.xpt",
  "folate2/DR2TOT_G.xpt",
  "folate2/DR2TOT_H.xpt",
  "folate2/DR2TOT_I.xpt",
  "folate2/P_DR2TOT.xpt"
)


folate1_all <- map_dfr(
  folate_files,
  ~ read_xpt(.x) %>%
    select(SEQN, DR1TFOLA)
) %>%
  mutate(SEQN = as.numeric(SEQN)) %>%
  distinct(SEQN, .keep_all = TRUE)  


folate2_all <- map_dfr(
  folate2_files,
  ~ read_xpt(.x) %>%
    select(SEQN, DR2TFOLA)
) %>%
  mutate(SEQN = as.numeric(SEQN)) %>%
  distinct(SEQN, .keep_all = TRUE)


folate_all <- folate1_all %>%
  full_join(folate2_all, by = "SEQN")


if (!"SEQN" %in% names(completedata) && "orgdata.SEQN" %in% names(completedata)) {
  completedata <-completedata %>% rename_with(~ sub("^orgdata\\.", "", .x))
}


completedata <- completedata %>%
  mutate(SEQN = as.numeric(SEQN)) %>%
  left_join(folate_all, by = "SEQN")


##

completedata3 <- completedata %>%
  dplyr::filter(!is.na(DR1TFOLA) & !is.na(DR2TFOLA)) %>%    
  dplyr::mutate(
    Folate_mean   = (DR1TFOLA + DR2TFOLA) / 2,             
    Folate_mean_z = as.numeric(scale(Folate_mean))  
  )


nhc3 <- svydesign(id=~SDMVPSU, weights=~WTINT2YR,strata=~SDMVSTRA, nest=TRUE, survey.lonely.psu = "adjust", data=completedata3)


logit17 <- svyglm(smoking_new~ Folate_mean_z, family=quasibinomial, design=nhc3, na.action = na.omit)
avg_comparisons(model=logit17, comparison = 'lnratioavg',transform = exp, wts = weights(logit17))



logit18 <- svyglm(smoking_new~ Folate_mean_z + agecat + racecat + educat + incomecat + maritalcat, family=quasibinomial, design=nhc3, na.action = na.omit)
avg_comparisons(model=logit18, comparison = 'lnratioavg',transform = exp, wts = weights(logit18))



logit19 <- svyglm(RBCfolate~ Folate_mean_z, family=quasibinomial, design=nhc3, na.action = na.omit)
avg_comparisons(model=logit19, comparison = 'lnratioavg',transform = exp, wts = weights(logit19))



logit20 <- svyglm(RBCfolate~ Folate_mean_z + agecat + racecat + educat + incomecat + maritalcat, family=quasibinomial, design=nhc3, na.action = na.omit)
avg_comparisons(model=logit20, comparison = 'lnratioavg',transform = exp, wts = weights(logit20))


#################


