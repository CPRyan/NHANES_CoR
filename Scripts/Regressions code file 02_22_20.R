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

base <- svydesign(id      = ~SDMVPSU,
                  strata  = ~SDMVSTRA,
                  weights = ~WTMEC2YR,
                  nest    = TRUE,
                  data    = df)

nhanesDesign1<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIDEXPRG ==2 & livebirths > -1 & menopause == 0 & RIAGENDR == 2)
nhanesDesign2<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIDEXPRG ==2 & livebirths > -1 & menopause == 1 & RIAGENDR == 2)

#### PRIMARY MODELS -- # OF BIRTHS, ALL COVARIATES, NO INTERACTIONS ####

### Premenopausal
summary(LM1_pre <- svyglm(LM ~ livebirths + livebirths2 + RIDAGEYR + BMXBMI + smoking + DMDEDUC2 + INDFMPIR + RIDRETH1 + SDDSRVYR, design = nhanesDesign1, data = df))
summary(HD1_pre <- svyglm(HD ~ livebirths + livebirths2 + RIDAGEYR + BMXBMI + smoking + DMDEDUC2 + INDFMPIR + RIDRETH1 + SDDSRVYR, design = nhanesDesign1))
summary(KDM1_pre <- svyglm(KDM ~ livebirths + livebirths2 + RIDAGEYR + BMXBMI + smoking + DMDEDUC2 + INDFMPIR + RIDRETH1 + SDDSRVYR, design = nhanesDesign1))

### Postmenopausal
summary(LM1_post <- svyglm(LM ~ livebirths + livebirths2 + RIDAGEYR + BMXBMI + smoking + DMDEDUC2 + INDFMPIR + RIDRETH1 + SDDSRVYR, design = nhanesDesign2))
summary(HD1_post <- svyglm(HD ~ livebirths + livebirths2 + RIDAGEYR + BMXBMI + smoking + DMDEDUC2 + INDFMPIR + RIDRETH1 + SDDSRVYR, design = nhanesDesign2))
summary(KDM1_post <- svyglm(KDM ~ livebirths + livebirths2 + RIDAGEYR + BMXBMI + smoking + DMDEDUC2 + INDFMPIR + RIDRETH1 + SDDSRVYR, design = nhanesDesign2))

#### SECONDARY MODELS -- # OF BIRTHS, NO COVARIATES, NO INTERACTIONS ####

### Premenopausal
summary(LM2_pre <- svyglm(LM ~ livebirths + livebirths2 + RIDAGEYR + SDDSRVYR, design = nhanesDesign1))
summary(HD2_pre <- svyglm(HD ~ livebirths + livebirths2 + RIDAGEYR + SDDSRVYR, design = nhanesDesign1))
summary(KDM2_pre <- svyglm(KDM ~ livebirths + livebirths2 + RIDAGEYR + SDDSRVYR, design = nhanesDesign1))

### Postmenopausal
summary(LM2_post <- svyglm(LM ~ livebirths + livebirths2 + RIDAGEYR + SDDSRVYR, design = nhanesDesign2))
summary(HD2_post <- svyglm(HD ~ livebirths + livebirths2 + RIDAGEYR + SDDSRVYR, design = nhanesDesign2))
summary(KDM2_post <- svyglm(KDM ~ livebirths + livebirths2 + RIDAGEYR + SDDSRVYR, design = nhanesDesign2))

#### PLOTS FOR LIVE BIRTHS ####

#LM
LMplot1<-plot_summs(LM1_pre, LM2_pre, LM1_post, LM2_post,
                    coefs = c("# Live Births (linear)" = "livebirths", "# Live births (quadratic)" = "livebirths2", "Current Age" = "RIDAGEYR"),
                    scale = TRUE) + ggtitle("Levine Method")

#HD
HDplot1<-plot_summs(HD1_pre, HD2_pre, HD1_post, HD2_post,
                    coefs = c("# Live Births (linear)" = "livebirths", "# Live births (quadratic)" = "livebirths2", "Current Age" = "RIDAGEYR"),
                    scale = TRUE) + ggtitle("HD")

#KDM
KDMplot1<-plot_summs(KDM1_pre, KDM2_pre, KDM1_post, KDM2_post,
                    coefs = c("# Live Births (linear)" = "livebirths", "# Live births (quadratic)" = "livebirths2", "Current Age" = "RIDAGEYR"),
                    scale = TRUE) + ggtitle("KDM")


#### TERTIARY MODELS -- # OF BIRTHS, YES COVARIATES, YES INTERACTIONS ####
#(these will be in ESM)

### Premenopausal
summary(a1 <- svyglm(LM ~ livebirths + livebirths2 + RIDAGEYR + BMXBMI + smoking + DMDEDUC2*livebirths + INDFMPIR*livebirths + RIDRETH1*livebirths + SDDSRVYR, design = nhanesDesign1))
summary(a1 <- svyglm(HD ~ livebirths + livebirths2 + RIDAGEYR + BMXBMI + smoking + DMDEDUC2*livebirths + INDFMPIR*livebirths + RIDRETH1*livebirths + SDDSRVYR, design = nhanesDesign1))
summary(a1 <- svyglm(KDM ~ livebirths + livebirths2 + RIDAGEYR + BMXBMI + smoking + DMDEDUC2*livebirths + INDFMPIR*livebirths + RIDRETH1*livebirths + SDDSRVYR, design = nhanesDesign1))

### Postmenopausal
summary(a1 <- svyglm(LM ~ livebirths + livebirths2 + RIDAGEYR + BMXBMI + smoking + DMDEDUC2*livebirths + INDFMPIR*livebirths + RIDRETH1*livebirths + SDDSRVYR, design = nhanesDesign2))
summary(a1 <- svyglm(HD ~ livebirths + livebirths2 + RIDAGEYR + BMXBMI + smoking + DMDEDUC2*livebirths + INDFMPIR*livebirths + RIDRETH1*livebirths + SDDSRVYR, design = nhanesDesign2))
summary(a1 <- svyglm(KDM ~ livebirths + livebirths2 + RIDAGEYR + BMXBMI + smoking + DMDEDUC2*livebirths + INDFMPIR*livebirths + RIDRETH1*livebirths + SDDSRVYR, design = nhanesDesign2))

#### Tables for primary models -- # OF BIRTHS ####

stargazer(LM1_pre, HD1_pre, KDM1_pre, single.row = F, align = T, intercept.bottom = T, no.space = T, dep.var.caption = "", type = "html", out = "PREmenopausal_livebirthsnumber.html", report = "vc*",   star.char = c("+", "*", "**", "***"), digits = 3, notes = "+p<0.1;*p<0.05;**p<0.01;***p<0.001 ", notes.append = TRUE)
stargazer(LM1_post, HD1_post, KDM1_post, single.row = F, align = T, intercept.bottom = T, no.space = T, dep.var.caption = "", type = "html", out = "POSTmenopausal_livebirthsnumber.html", report = "vc*",   star.char = c("+", "*", "**", "***"), digits = 3, notes = "+p<0.1;*p<0.05;**p<0.01;***p<0.001 ", notes.append = TRUE)

#### PRIMARY MODELS -- EVER BIRTH, ALL COVARIATES, NO INTERACTIONS ####

### Premenopausal
summary(LM3_pre <- svyglm(LM ~ livebirths_dichot + RIDAGEYR + BMXBMI + smoking + DMDEDUC2 + INDFMPIR + RIDRETH1 + SDDSRVYR, design = nhanesDesign1))
summary(HD3_pre <- svyglm(HD ~ livebirths_dichot + RIDAGEYR + BMXBMI + smoking + DMDEDUC2 + INDFMPIR + RIDRETH1 + SDDSRVYR, design = nhanesDesign1))
summary(KDM3_pre <- svyglm(HD ~ livebirths_dichot + RIDAGEYR + BMXBMI + smoking + DMDEDUC2 + INDFMPIR + RIDRETH1 + SDDSRVYR, design = nhanesDesign1))

### Postmenopausal
summary(LM3_post <- svyglm(LM ~ livebirths_dichot + RIDAGEYR + BMXBMI + smoking + DMDEDUC2 + INDFMPIR + RIDRETH1 + SDDSRVYR, design = nhanesDesign2))
summary(HD3_post <- svyglm(HD ~ livebirths_dichot + RIDAGEYR + BMXBMI + smoking + DMDEDUC2 + INDFMPIR + RIDRETH1 + SDDSRVYR, design = nhanesDesign2))
summary(KDM3_post <- svyglm(KDM ~ livebirths_dichot + RIDAGEYR + BMXBMI + smoking + DMDEDUC2 + INDFMPIR + RIDRETH1 + SDDSRVYR, design = nhanesDesign2))

#### SECONDARY MODELS -- EVER BIRTH, NO COVARIATES, NO INTERACTIONS ####

### Premenopausal
summary(LM4_pre <- svyglm(LM ~ livebirths_dichot + RIDAGEYR + SDDSRVYR, design = nhanesDesign1))
summary(HD4_pre <- svyglm(HD ~ livebirths_dichot + RIDAGEYR + SDDSRVYR, design = nhanesDesign1))
summary(KDM4_pre <- svyglm(KDM ~ livebirths_dichot + RIDAGEYR + SDDSRVYR, design = nhanesDesign1))

### Postmenopausal
summary(LM4_post <- svyglm(LM ~ livebirths_dichot + RIDAGEYR + SDDSRVYR, design = nhanesDesign2))
summary(HD4_post <- svyglm(HD ~ livebirths_dichot + RIDAGEYR + SDDSRVYR, design = nhanesDesign2))
summary(KDM4_post <- svyglm(KDM ~ livebirths_dichot + RIDAGEYR + SDDSRVYR, design = nhanesDesign2))

#### PLOTS FOR PARITY ####

#LM
LMplot2<-plot_summs(LM3_pre, LM4_pre, LM3_post, LM4_post,
                    coefs = c("Parity" = "livebirths_dichot", "Current Age" = "RIDAGEYR"),
                    scale = TRUE) + ggtitle("Levine Method")

#HD
HDplot2<-plot_summs(HD3_pre, HD4_pre, HD3_post, HD4_post,
                    coefs = c("Parity" = "livebirths_dichot", "Current Age" = "RIDAGEYR"),
                    scale = TRUE) + ggtitle("HD")

#KDM
KDMplot2<-plot_summs(KDM3_pre, KDM4_pre, KDM3_post, KDM4_post,
                 coefs = c("Parity" = "livebirths_dichot", "Current Age" = "RIDAGEYR"),
                 scale = TRUE) + ggtitle("KDM")

#### TERTIARY MODELS -- EVER BIRTH, YES COVARIATES, YES INTERACTIONS ####
#(these will be in ESM)

### Premenopausal
summary(a1 <- svyglm(LM ~ livebirths_dichot + RIDAGEYR + BMXBMI + smoking + DMDEDUC2*livebirths_dichot + INDFMPIR*livebirths_dichot + RIDRETH1*livebirths_dichot + SDDSRVYR, design = nhanesDesign1))
summary(a1 <- svyglm(HD ~ livebirths_dichot + RIDAGEYR + BMXBMI + smoking + DMDEDUC2*livebirths_dichot + INDFMPIR*livebirths_dichot + RIDRETH1*livebirths_dichot + SDDSRVYR, design = nhanesDesign1))
summary(a1 <- svyglm(KDM ~ livebirths_dichot + RIDAGEYR + BMXBMI + smoking + DMDEDUC2*livebirths_dichot + INDFMPIR*livebirths_dichot + RIDRETH1*livebirths_dichot + SDDSRVYR, design = nhanesDesign1))

### Postmenopausal
summary(a1 <- svyglm(LM ~ livebirths_dichot + RIDAGEYR + BMXBMI + smoking + DMDEDUC2*livebirths_dichot + INDFMPIR*livebirths_dichot + RIDRETH1*livebirths_dichot + SDDSRVYR, design = nhanesDesign2))
summary(a1 <- svyglm(HD ~ livebirths_dichot + RIDAGEYR + BMXBMI + smoking + DMDEDUC2*livebirths_dichot + INDFMPIR*livebirths_dichot + RIDRETH1*livebirths_dichot + SDDSRVYR, design = nhanesDesign2))
summary(a1 <- svyglm(KDM ~ livebirths_dichot + RIDAGEYR + BMXBMI + smoking + DMDEDUC2*livebirths_dichot + INDFMPIR*livebirths_dichot + RIDRETH1*livebirths_dichot + SDDSRVYR, design = nhanesDesign2))

#### Tables for primary models -- EVER-PARITY ####

stargazer(LM3_pre, HD3_pre, KDM3_pre, single.row = F, align = T, intercept.bottom = T, no.space = T, dep.var.caption = "", type = "html", out = "PREmenopausal_everparity.html", report = "vc*",   star.char = c("+", "*", "**", "***"), digits = 3, notes = "+p<0.1;*p<0.05;**p<0.01;***p<0.001 ", notes.append = TRUE)
stargazer(LM3_post, HD3_post, KDM3_post, single.row = F, align = T, intercept.bottom = T, no.space = T, dep.var.caption = "", type = "html", out = "POSTmenopausal_everparity.html", report = "vc*",   star.char = c("+", "*", "**", "***"), digits = 3, notes = "+p<0.1;*p<0.05;**p<0.01;***p<0.001 ", notes.append = TRUE)

#### Descriptives ####
vars<-names(df)[c(5,6,7,8,14,21,22,23,24,41,42,44)]
svymean(make.formula(vars), nhanesDesign1 , na.rm= TRUE)
svymean(make.formula(vars), nhanesDesign2 , na.rm= TRUE)
