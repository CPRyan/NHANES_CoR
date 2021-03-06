library(haven)
library(psych)
library(dplyr)
library(ggplot2)
library(survey)
library(foreign)
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
df2 <- df %>% filter(complete.cases(livebirths, menopause, RIDAGEYR, BMI, INDFMPIR, RIDRETH1, smoking, DMDEDUC2))
df3 <- filter(df2, RIDAGEYR>17 & RIDAGEYR <= 84 & RIDEXPRG == 2 & RIAGENDR == 2 & livebirths >-1 & livebirths <7)
lm2 <- lm(LOG_HD~RIDAGEYR, data = df3)
resids <- as.data.frame(lm2$residuals)
resids <- cbind(df3, resids)
resids2 <- resids[,c(1,46)]
df <- merge (df, resids2, by = "SEQN", all = TRUE)
colnames(df)[colnames(df)=="lm2$residuals"] <- "HD_age_resid"

#write to .dta
#write.dta(df, "NHANES_repro_file_041320.dta")

#Set survey specs

base <- svydesign(id      = ~SDMVPSU,
                  strata  = ~SDMVSTRA,
                  weights = ~WTMEC2YR,
                  nest    = TRUE,                  
                  data    = df)

#### Excluding 7+ live births
nhanesDesign1<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIAGENDR == 2 & livebirths > -1 & livebirths < 7 & RIDEXPRG == 2)

summary(LM1 <- svyglm(LM ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR + BMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(HD1 <- svyglm(LOG_HD ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR + BMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(KDM1 <- svyglm(KDM ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR + BMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))

summary(LM1 <- svyglm(LM ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df))
summary(HD1 <- svyglm(LOG_HD ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df))
summary(KDM1 <- svyglm(KDM ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df))

#### Not excluding 7+ live births
nhanesDesign1<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIAGENDR == 2 & livebirths > -1 & RIDEXPRG == 2)

#### Primary analyses -- all covariates 
summary(LM1 <- svyglm(LM ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR + BMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(HD1 <- svyglm(LOG_HD ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR + BMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(KDM1 <- svyglm(KDM ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR + BMI + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))

#### Secondary analyses -- age covariate only
summary(LM1 <- svyglm(LM ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df))
summary(HD1 <- svyglm(LOG_HD ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df))
summary(KDM1 <- svyglm(KDM ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df))

#### Number of live births histogram

df2 <- df %>% filter(complete.cases(livebirths, menopause, RIDAGEYR, BMI, INDFMPIR, RIDRETH1, smoking, DMDEDUC2))
df3 <- filter(df2, RIDAGEYR>17 & RIDAGEYR <= 84 & RIDEXPRG == 2 & RIAGENDR == 2 & livebirths >-1 & livebirths <7)

#write to csv for waylon to calculate biomarker stats for final sample
#write.csv(df3, "final_analytical_sample_041920.csv")

df3$livebirths<-as.factor(df3$livebirths)

ggplot(df3, aes(x=livebirths, fill = menopause)) +
  geom_histogram(position = "dodge", stat = "count", color = "black") +
  theme_minimal() + scale_fill_grey() + 
  ylab("Frequency") + xlab("Live births") + 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18))

#### Sample size funnel (starting with 92,062)
df2 <- filter(df, LM >0)
df3 <- filter(df2, RIAGENDR ==2)
df4 <- filter(df3, RIDAGEYR>17 & RIDAGEYR <= 84 & RIDEXPRG == 2)
