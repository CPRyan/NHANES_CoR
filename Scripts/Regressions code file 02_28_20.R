library(haven)
library(psych)
library(dplyr)
library(ggplot2)
library(survey)
library(jtools)
library(ggstance)
library(corrplot)
library(GGally)
library(stargazer)

NHANES <- read.csv("~/GitHub/NHANES_CoR/Data/NHANES_022420")

BA <- read_sav("~/GitHub/NHANES_CoR/Data/NHANES_BA_Talia.sav")
BA2 <- BA[which(BA$WAVE<3),]

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

base <- svydesign(id      = ~SDMVPSU,
                  strata  = ~SDMVSTRA,
                  weights = ~WTMEC2YR,
                  nest    = TRUE,                  
                  data    = df)

nhanesDesign1<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIAGENDR == 2 & livebirths > -1 & RIDEXPRG == 2)

#### Primary analyses -- all covariates 
summary(LM1 <- svyglm(LM ~ livebirths + livebirths2 + livebirths:menopause + RIDAGEYR + BMXBMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(HD1 <- svyglm(HD ~ livebirths + livebirths2 + livebirths:menopause + RIDAGEYR + BMXBMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(KDM1 <- svyglm(KDM ~ livebirths + livebirths2 + livebirths:menopause + RIDAGEYR + BMXBMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))

summary(LM2 <- svyglm(LM ~ livebirths_dichot + livebirths_dichot:menopause + RIDAGEYR + BMXBMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(HD2 <- svyglm(HD ~ livebirths_dichot + livebirths_dichot:menopause + RIDAGEYR + BMXBMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(KDM2 <- svyglm(KDM ~ livebirths_dichot + livebirths_dichot:menopause + RIDAGEYR + BMXBMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))

#### Secondary analyses -- age covariate only
summary(LM3 <- svyglm(LM ~ livebirths + livebirths2 + livebirths:menopause + RIDAGEYR, design = nhanesDesign1, data = df))
summary(HD3 <- svyglm(HD ~ livebirths + livebirths2 + livebirths:menopause + RIDAGEYR, design = nhanesDesign1, data = df))
summary(KDM3 <- svyglm(KDM ~ livebirths + livebirths2 + livebirths:menopause + RIDAGEYR, design = nhanesDesign1, data = df))

summary(LM4 <- svyglm(LM ~ livebirths_dichot + livebirths_dichot:menopause + RIDAGEYR, design = nhanesDesign1, data = df))
summary(HD4 <- svyglm(HD ~ livebirths_dichot + livebirths_dichot:menopause + RIDAGEYR, design = nhanesDesign1, data = df))
summary(KDM4 <- svyglm(KDM ~ livebirths_dichot + livebirths_dichot:menopause + RIDAGEYR, design = nhanesDesign1, data = df))


#### Following up on significant interactions

nhanesDesign2<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIAGENDR == 2 & livebirths > -1 & RIDEXPRG == 2 & menopause == 0)
nhanesDesign3<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIAGENDR == 2 & livebirths > -1 & RIDEXPRG == 2 & menopause == 1)

summary(LM5 <- svyglm(LM ~ livebirths + livebirths2 + RIDAGEYR + BMXBMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign2, data = df))
summary(LM6 <- svyglm(LM ~ livebirths + livebirths2 + RIDAGEYR + BMXBMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign3, data = df))

summary(LM7 <- svyglm(LM ~ livebirths_dichot + RIDAGEYR + BMXBMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign2, data = df))
summary(LM8 <- svyglm(LM ~ livebirths_dichot + RIDAGEYR + BMXBMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign3, data = df))
