library(haven)
library(psych)
library(dplyr)
library(ggplot2)
library(survey)
library(foreign)
library(psych)

#Remove "/Documents". I had to add this to get it to run on my office Mac -WJH
NHANES <- read.csv("~/Documents/GitHub/NHANES_CoR/Data/NHANES_051720.csv", header=TRUE)
BA <- read_sav("~/Documents/GitHub/NHANES_CoR/Data/NHANES_BA_Talia.sav")

df <- merge (NHANES, BA, by = "SEQN", all = TRUE)
colnames(df)[colnames(df)=="PhenoAge"] <- "LM"

df$livebirths2 <- df$livebirths^2
df$BMI2 <- df$BMI^2

df$monthssincebirth <- as.numeric(df$monthssincebirth)

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

#Make LM resid variable
df2 <- df %>% filter(complete.cases(livebirths, menopause, RIDAGEYR, BMI, INDFMPIR, RIDRETH1, smoking, DMDEDUC2,LM,KDM,LOG_HD))
df3 <- filter(df2, RIDAGEYR>17 & RIDAGEYR <= 84 & RIDEXPRG == 2 & RIAGENDR == 2 & livebirths >-1 & livebirths <7)
lm2 <- lm(LM~RIDAGEYR, data = df3)
resids <- as.data.frame(lm2$residuals)
resids <- cbind(df3, resids)
resids2 <- resids[,c(1,48)]
df <- merge (df, resids2, by = "SEQN", all = TRUE)
colnames(df)[colnames(df)=="lm2$residuals"] <- "LM_age_resid"

#Make KDM resid variable
lm2 <- lm(KDM~RIDAGEYR, data = df3)
resids <- as.data.frame(lm2$residuals)
resids <- cbind(df3, resids)
resids2 <- resids[,c(1,48)]
df <- merge (df, resids2, by = "SEQN", all = TRUE)
colnames(df)[colnames(df)=="lm2$residuals"] <- "KDM_age_resid"

#Make HD resid variable
lm2 <- lm(LOG_HD~RIDAGEYR, data = df3)
resids <- as.data.frame(lm2$residuals)
resids <- cbind(df3, resids)
resids2 <- resids[,c(1,48)]
df <- merge (df, resids2, by = "SEQN", all = TRUE)
colnames(df)[colnames(df)=="lm2$residuals"] <- "HD_age_resid"

#write to .dta
#write.dta(df, "NHANES_repro_file_051820.dta")

#Set survey specs

base <- svydesign(id      = ~SDMVPSU,
                  strata  = ~SDMVSTRA,
                  weights = ~WTMEC2YR,
                  nest    = TRUE,                  
                  data    = df)

#### Excluding 7+ live births
nhanesDesign1<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIAGENDR == 2 & livebirths > -1 & livebirths < 7 & RIDEXPRG == 2)

#### Primary analyses -- all covariates 
summary(LM1 <- svyglm(LM_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(HD1 <- svyglm(HD_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(KDM1 <- svyglm(KDM_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))

#### Secondary analyses -- age covariate only
summary(LM1 <- svyglm(LM_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df))
summary(HD1 <- svyglm(HD_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df))
summary(KDM1 <- svyglm(KDM_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df))

#### Not excluding 7+ live births
nhanesDesign1<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIAGENDR == 2 & livebirths > -1 & RIDEXPRG == 2)

#### Primary analyses -- all covariates 
summary(LM1 <- svyglm(LM_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(HD1 <- svyglm(HD_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(KDM1 <- svyglm(KDM_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))

#### Secondary analyses -- age covariate only
summary(LM1 <- svyglm(LM_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df))
summary(HD1 <- svyglm(HD_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df))
summary(KDM1 <- svyglm(KDM_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df))

#### Number of live births histogram

df2 <- df %>% filter(complete.cases(livebirths, menopause, RIDAGEYR, BMI, INDFMPIR, RIDRETH1, smoking, DMDEDUC2,LM,KDM,LOG_HD))
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
df5 <- filter(df4, livebirths <7)
df5 <- df5 %>% filter(complete.cases(RIDAGEYR, BMI, INDFMPIR, smoking, DMDEDUC2, RIDRETH1, livebirths, menopause,LM,KDM,LOG_HD))

##Analyses: Time since last birth

#### Excluding 7+ live births
nhanesDesign1<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIAGENDR == 2 & livebirths > -1 & livebirths < 7 & RIDEXPRG == 2)

#### YEARS
#### Primary analyses -- all covariates 
summary(LM1 <- svyglm(LM_age_resid ~ livebirths*menopause + livebirths2*menopause + livebirths*yearssincelastbirth + livebirths2*yearssincelastbirth + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(HD1 <- svyglm(HD_age_resid ~ livebirths*menopause + livebirths2*menopause + livebirths*yearssincelastbirth + livebirths2*yearssincelastbirth + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(KDM1 <- svyglm(KDM_age_resid ~ livebirths*menopause + livebirths2*menopause + livebirths*yearssincelastbirth + livebirths2*yearssincelastbirth + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))


#### MONTHS
summary(LM1 <- svyglm(LM_age_resid ~ livebirths*monthssincebirth + livebirths2*monthssincebirth + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(HD1 <- svyglm(HD_age_resid ~ livebirths*monthssincebirth + livebirths2*monthssincebirth + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))
summary(KDM1 <- svyglm(KDM_age_resid ~ livebirths*monthssincebirth + livebirths2*monthssincebirth + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df))

###Correlation plot-Raw Measures
dfcor<-df5[c("RIDAGEYR", "LM", "LOG_HD", "KDM")]
dfcor <- dfcor %>% rename(
  "Age" = RIDAGEYR,
  "HD (log)" = LOG_HD
)
             
pairs.panels(dfcor, stars= TRUE, method = "pearson", hist.col = "gray", cex.labels=1.9, ellipses = FALSE) 

##Correlation plot-Age Adjusted Measures 
dfcor<-df5[c("RIDAGEYR", "LM_age_resid", "HD_age_resid", "KDM_age_resid")]
dfcor <- dfcor %>% rename(
  "Age" = RIDAGEYR,
  "HD (adjusted)" = HD_age_resid,
  "LM (adjusted)" = LM_age_resid,
  "KDM (adjusted)" = KDM_age_resid
)

pairs.panels(dfcor, stars= TRUE, method = "pearson", hist.col = "gray", cex.labels=1.9, ellipses = FALSE) 
