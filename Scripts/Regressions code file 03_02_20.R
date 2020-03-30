library(haven)
library(psych)
library(dplyr)
library(ggplot2)
library(survey)
library(foreign)
library(broom)
library(psych)
#library(jtools)
#library(ggstance)
#library(corrplot)
#library(GGally)

NHANES <- read.csv("~/GitHub/NHANES_CoR/Data/NHANES_030220", header=TRUE)

BA <- read_sav("~/GitHub/NHANES_CoR/Data/NHANES_BA_Talia.sav")

df <- merge (NHANES, BA, by = "SEQN", all = TRUE)
colnames(df)[colnames(df)=="PhenoAge"] <- "LM"

df$livebirths2 <- df$livebirths^2

#Make race/ethnicity factor variable
df$RIDRETH1 <- as.factor(df$RIDRETH1)
df <- within(df, RIDRETH1 <- relevel(RIDRETH1, ref = "0"))

#Make smoking factor variable
df$smoking <- as.factor(df$smoking)
df <- within(df, smoking <- relevel(smoking, ref = "0"))

#Make education factor variable
df$DMDEDUC2 <- as.factor(df$DMDEDUC2)
df <- within(df, DMDEDUC2 <- relevel(DMDEDUC2, ref = "5"))

#Make pregnancy status numeric
df$RIDEXPRG <- as.numeric(df$RIDEXPRG)

#Make menopause factor
df$menopause <- as.factor(df$menopause)
df <- within(df, menopause <- relevel(menopause, ref = "0"))

#Make livebirths_dichot factor
df$livebirths_dichot <- as.factor(df$livebirths_dichot)
df <- within(df, livebirths_dichot <- relevel(livebirths_dichot, ref = "0"))

#Make LM age acceleration variable
df$LM_acceleration <- df$LM-df$RIDAGEYR

#Make KDM age acceleration variable
df$KDM_acceleration <- df$KDM-df$RIDAGEYR

#Make HD resid variable
lm1 <- lm(LOG_HD~RIDAGEYR, data = df)
resids <- broom::augment(lm1)[,c(1,6)]
colnames(resids)[colnames(resids)==".rownames"] <- "SEQN"
df <- merge (df, resids, by = "SEQN", all = TRUE)
colnames(df)[colnames(df)==".resid"] <- "HD_age_resid"

#write to .dta
#write.dta(df, "NHANES_repro_file_030420.dta")

#Set survey specs

base <- svydesign(id      = ~SDMVPSU,
                  strata  = ~SDMVSTRA,
                  weights = ~WTMEC2YR,
                  nest    = TRUE,                  
                  data    = df)

nhanesDesign1<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIAGENDR == 2 & livebirths > -1 & RIDEXPRG == 2)

#### Primary analyses -- all covariates 
summary(LM1 <- svyglm(LM ~ livebirths + livebirths2 + livebirths:menopause + RIDAGEYR + BMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(HD1 <- svyglm(LOG_HD ~ livebirths + livebirths2 + livebirths:menopause + RIDAGEYR + BMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(KDM1 <- svyglm(KDM ~ livebirths + livebirths2 + livebirths:menopause + RIDAGEYR + BMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))

summary(LM2 <- svyglm(LM ~ livebirths_dichot + livebirths_dichot:menopause + RIDAGEYR + BMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(HD2 <- svyglm(LOG_HD ~ livebirths_dichot + livebirths_dichot:menopause + RIDAGEYR + BMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(KDM2 <- svyglm(KDM ~ livebirths_dichot + livebirths_dichot:menopause + RIDAGEYR + BMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))

#### Secondary analyses -- age covariate only
summary(LM3 <- svyglm(LM ~ livebirths + livebirths2 + livebirths:menopause + RIDAGEYR, design = nhanesDesign1, data = df))
summary(HD3 <- svyglm(LOG_HD ~ livebirths + livebirths2 + livebirths:menopause + RIDAGEYR, design = nhanesDesign1, data = df))
summary(KDM3 <- svyglm(KDM ~ livebirths + livebirths2 + livebirths:menopause + RIDAGEYR, design = nhanesDesign1, data = df))

summary(LM4 <- svyglm(LM ~ livebirths_dichot + livebirths_dichot:menopause + RIDAGEYR, design = nhanesDesign1, data = df))
summary(HD4 <- svyglm(LOG_HD ~ livebirths_dichot + livebirths_dichot:menopause + RIDAGEYR, design = nhanesDesign1, data = df))
summary(KDM4 <- svyglm(KDM ~ livebirths_dichot + livebirths_dichot:menopause + RIDAGEYR, design = nhanesDesign1, data = df))


#### Following up on significant interactions

nhanesDesign2<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIAGENDR == 2 & livebirths > -1 & RIDEXPRG == 2 & menopause == 0)
nhanesDesign3<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIAGENDR == 2 & livebirths > -1 & RIDEXPRG == 2 & menopause == 1)

summary(LM5 <- svyglm(LM ~ livebirths + livebirths2 + RIDAGEYR + BMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign2, data = df))
summary(LM6 <- svyglm(LM ~ livebirths + livebirths2 + RIDAGEYR + BMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign3, data = df))

summary(LM7 <- svyglm(LM ~ livebirths_dichot + RIDAGEYR + BMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign2, data = df))
summary(LM8 <- svyglm(LM ~ livebirths_dichot + RIDAGEYR + BMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign3, data = df))
