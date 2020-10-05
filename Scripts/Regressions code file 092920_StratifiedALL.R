library(haven)
library(psych)
library(dplyr)
library(ggplot2)
library(survey)
library(foreign)
library(psych)
library(jtools)
library(car)

#Remove "/Documents". I had to add this to get it to run on my office Mac -WJH
NHANES <- read.csv("~/Documents/GitHub/NHANES_CoR/Data/NHANES_090920.csv", header=TRUE)
load("~/Documents/GitHub/NHANES_CoR/Data/NHANES_BA_Talia.rda")

NHANES <- read.csv("~/GitHub/NHANES_CoR/Data/NHANES_090920.csv", header=TRUE)
load("~/GitHub/NHANES_CoR/Data/NHANES_BA_Talia.rda")

df <- merge (NHANES, BA_NHANES_Parity, by = "SEQN", all = TRUE)
colnames(df)[colnames(df)=="PhenoAge"] <- "LM"

df$livebirths2 <- df$livebirths^2
df$BMI2 <- df$BMI^2

df$monthssincebirth <- as.numeric(df$monthssincebirth)

#Make categorical BMI variable
df$BMI_cat <- car::recode(df$BMI, "
                          0:18.499 = 'underweight';
                          18.50:24.99 = 'normal';
                          25.0:29.99 = 'overweight';
                          30.0:500 = 'obesity'")

#Make race/ethnicity factor variable
df$RIDRETH1 <- as.factor(df$RIDRETH1)
df <- within(df, RIDRETH1 <- relevel(RIDRETH1, ref = "0"))

#Make smoking factor variable
df$smoking <- as.factor(df$smoking)
df <- within(df, smoking <- relevel(smoking, ref = "0"))

#Make education factor variable
df$DMDEDUC2 <- as.factor(df$DMDEDUC2)
df <- within(df, DMDEDUC2 <- relevel(DMDEDUC2, ref = "5"))

#Make menopause factor
df$menopause <- as.factor(df$menopause)
df <- within(df, menopause <- relevel(menopause, ref = "0"))

#Make livebirths_dichot factor
df$livebirths_dichot <- as.factor(df$livebirths_dichot)
df <- within(df, livebirths_dichot <- relevel(livebirths_dichot, ref = "0"))

#Update RIDEXPRG variable

df <- df %>%
  mutate(RIDEXPRG = case_when(RIDEXPRG == 3 ~3, RIAGENDR == 2 & RIDAGEYR > 59 & RIDAGEYR <= 84 ~ 2, RIDEXPRG == 1 ~ 1, RIDEXPRG == 2 ~ 2)) %>%
  mutate_at(vars(RIDEXPRG), funs(as.factor))

#Make LM resid variable
df2 <- filter(df, RIDAGEYR>17 & RIDAGEYR <= 84 & RIDEXPRG == 2 & RIAGENDR == 2 & livebirths >-1)
df3 <- df2 %>% filter(complete.cases(livebirths, menopause, RIDAGEYR, LM))
lm2 <- lm(LM~RIDAGEYR, data = df3)
resids <- as.data.frame(lm2$residuals)
resids <- cbind(df3, resids)
resids2 <- resids[,c(1,52)]
df <- merge (df, resids2, by = "SEQN", all = TRUE)
colnames(df)[colnames(df)=="lm2$residuals"] <- "LM_age_resid"

#Make KDM resid variable
lm2 <- lm(KDM~RIDAGEYR, data = df3)
resids <- as.data.frame(lm2$residuals)
resids <- cbind(df3, resids)
resids2 <- resids[,c(1,52)]
df <- merge (df, resids2, by = "SEQN", all = TRUE)
colnames(df)[colnames(df)=="lm2$residuals"] <- "KDM_age_resid"

#Make HD resid variable
lm2 <- lm(LOG_HD~RIDAGEYR, data = df3)
resids <- as.data.frame(lm2$residuals)
resids <- cbind(df3, resids)
resids2 <- resids[,c(1,52)]
df <- merge (df, resids2, by = "SEQN", all = TRUE)
colnames(df)[colnames(df)=="lm2$residuals"] <- "HD_age_resid"

#Make AL resid variable
lm2 <- lm(AL~RIDAGEYR, data = df3)
resids <- as.data.frame(lm2$residuals)
resids <- cbind(df3, resids)
resids2 <- resids[,c(1,52)]
df <- merge (df, resids2, by = "SEQN", all = TRUE)
colnames(df)[colnames(df)=="lm2$residuals"] <- "AL_age_resid"

#Create correct sample weights; see https://wwwn.cdc.gov/nchs/nhanes/tutorials/module3.aspx
df <- df %>%
  mutate(WTMEC12YR = NA,
         WTMEC12YR = ifelse(SDDSRVYR == 1 | SDDSRVYR == 2, (WTMEC4YR/3), WTMEC12YR),
         WTMEC12YR = ifelse(SDDSRVYR == 3 | SDDSRVYR == 4 | SDDSRVYR == 5 | SDDSRVYR == 6, (WTMEC2YR/6), WTMEC12YR))

#write to .dta
#write.dta(df, "NHANES_repro_file_092920.dta")

#Set survey specs

base <- svydesign(id      = ~SDMVPSU,
                  strata  = ~SDMVSTRA,
                  weights = ~WTMEC12YR,
                  nest    = TRUE,                  
                  data    = df)

#### Excluding 8+ live births
nhanesDesign1<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIAGENDR == 2 & livebirths > -1 & livebirths < 8 & RIDEXPRG == 2)

#### Primary analyses -- all covariates 
summ(LM1 <- svyglm(LM_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(HD1 <- svyglm(HD_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(KDM1 <- svyglm(KDM_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(AL1 <- svyglm(AL_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)

#### Secondary analyses -- age covariate only

summ(LM1 <- svyglm(LM_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(HD1 <- svyglm(HD_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(KDM1 <- svyglm(KDM_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(AL1 <- svyglm(AL_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)

### Age as covariate only, but with primary analytical sample
nhanesDesign1<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIAGENDR == 2 & livebirths > -1 & livebirths < 8 & RIDEXPRG == 2 & BMI > 0 & INDFMPIR > -1 & !(is.na(smoking)) & !(is.na(DMDEDUC2)))

summ(LM1 <- svyglm(LM_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(HD1 <- svyglm(HD_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(KDM1 <- svyglm(KDM_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(AL1 <- svyglm(AL_age_resid ~ livebirths*menopause + livebirths2*menopause + RIDAGEYR, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)

#### Pre-menopausal women 
nhanesDesign1<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIAGENDR == 2 & livebirths > -1 & livebirths < 8 & RIDEXPRG == 2 & menopause ==0)
summ(LM1 <- svyglm(LM_age_resid ~ livebirths + livebirths2 + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(LM1 <- svyglm(LM_age_resid ~ livebirths + livebirths2 + RIDAGEYR, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(HD1 <- svyglm(HD_age_resid ~ livebirths + livebirths2 + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(HD1 <- svyglm(HD_age_resid ~ livebirths + livebirths2 + RIDAGEYR, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(KDM1 <- svyglm(KDM_age_resid ~ livebirths + livebirths2 + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(KDM1 <- svyglm(KDM_age_resid ~ livebirths + livebirths2 + RIDAGEYR, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(AL1 <- svyglm(AL_age_resid ~ livebirths + livebirths2 + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(AL1 <- svyglm(AL_age_resid ~ livebirths + livebirths2 + RIDAGEYR, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)

#### Post-menopausal women 
nhanesDesign1<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIAGENDR == 2 & livebirths > -1 & livebirths < 8 & RIDEXPRG == 2 & menopause ==1)
summ(LM1 <- svyglm(LM_age_resid ~ livebirths + livebirths2 + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(LM1 <- svyglm(LM_age_resid ~ livebirths + livebirths2 + RIDAGEYR, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(HD1 <- svyglm(HD_age_resid ~ livebirths + livebirths2 + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(HD1 <- svyglm(HD_age_resid ~ livebirths + livebirths2 + RIDAGEYR, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(KDM1 <- svyglm(KDM_age_resid ~ livebirths + livebirths2 + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(KDM1 <- svyglm(KDM_age_resid ~ livebirths + livebirths2 + RIDAGEYR, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(AL1 <- svyglm(AL_age_resid ~ livebirths + livebirths2 + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(AL1 <- svyglm(AL_age_resid ~ livebirths + livebirths2 + RIDAGEYR, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)

#### Number of live births histogram

df2 <- df %>% filter(complete.cases(livebirths, menopause, RIDAGEYR, BMI, INDFMPIR, RIDRETH1, smoking, DMDEDUC2,LM,KDM,LOG_HD))
df3 <- filter(df2, RIDAGEYR>17 & RIDAGEYR <= 84 & RIDEXPRG == 2 & RIAGENDR == 2 & livebirths >-1 & livebirths < 8)

#write to csv for waylon to calculate biomarker stats for final sample
#write.csv(df3, "final_analytical_sample_041920.csv")

df3$livebirths<-as.factor(df3$livebirths)

ggplot(df3, aes(x=livebirths, fill = menopause)) +
  geom_histogram(position = "dodge", stat = "count", color = "black") +
  theme_minimal() + scale_fill_grey() + 
  ylab("Frequency") + xlab("Live births") + 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  theme(legend.position = "none")

#### Sample size funnel (starting with 62,160)
df2 <- filter(df, RIAGENDR == 2 & RIDAGEYR > 17 & RIDAGEYR <= 84 & RIDEXPRG == 2)
df3 <- filter(df2, LM > 0)
df4 <- filter(df3, livebirths < 8)
df5 <- df4 %>% filter(complete.cases(RIDAGEYR, BMI, INDFMPIR, smoking, DMDEDUC2, RIDRETH1, livebirths, menopause, LM))

#Number of pregnant women among those with full biomarker panel
blah <- filter(df, RIAGENDR == 2& RIDAGEYR > 17 & RIDAGEYR <= 84 & RIDEXPRG == 1)

##Analyses: Time since last birth

#### Excluding 8+ live births
nhanesDesign1<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIAGENDR == 2 & livebirths > -1 & livebirths < 8 & RIDEXPRG == 2)

#### YEARS
#### Primary analyses -- all covariates 
summ(LM1 <- svyglm(LM_age_resid ~ livebirths*menopause + livebirths2*menopause + livebirths*yearssincelastbirth + livebirths2*yearssincelastbirth + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(HD1 <- svyglm(HD_age_resid ~ livebirths*menopause + livebirths2*menopause + livebirths*yearssincelastbirth + livebirths2*yearssincelastbirth + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(KDM1 <- svyglm(KDM_age_resid ~ livebirths*menopause + livebirths2*menopause + livebirths*yearssincelastbirth + livebirths2*yearssincelastbirth + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(AL1 <- svyglm(AL_age_resid ~ livebirths*menopause + livebirths2*menopause + livebirths*yearssincelastbirth + livebirths2*yearssincelastbirth + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 4)

# Premenopausal women only for HD
nhanesDesign1<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIAGENDR == 2 & livebirths > -1 & livebirths < 8 & RIDEXPRG == 2 & menopause == 0)
summ(HD1 <- svyglm(HD_age_resid ~ livebirths + livebirths2 + livebirths*yearssincelastbirth + livebirths2*yearssincelastbirth + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)

# Postmenopausal women only for HD
nhanesDesign1<- subset(base, RIDAGEYR > 17 & RIDAGEYR <=84 & RIAGENDR == 2 & livebirths > -1 & livebirths < 8 & RIDEXPRG == 2 & menopause == 1)
summ(HD1 <- svyglm(HD_age_resid ~ livebirths + livebirths2 + livebirths*yearssincelastbirth + livebirths2*yearssincelastbirth + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)

#### MONTHS
summ(LM1 <- svyglm(LM_age_resid ~ livebirths*monthssincebirth + livebirths2*monthssincebirth + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(HD1 <- svyglm(HD_age_resid ~ livebirths*monthssincebirth + livebirths2*monthssincebirth + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(KDM1 <- svyglm(KDM_age_resid ~ livebirths*monthssincebirth + livebirths2*monthssincebirth + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 3)
summ(AL1 <- svyglm(AL_age_resid ~ livebirths*monthssincebirth + livebirths2*monthssincebirth + RIDAGEYR + BMI + BMI2 + INDFMPIR + smoking + DMDEDUC2 + RIDRETH1, design = nhanesDesign1, data = df), confint = TRUE, digits = 4)

###Correlation plot-Raw Measures
dfcor<-df5[c("RIDAGEYR", "LM", "LOG_HD", "KDM", "AL")]
dfcor <- dfcor %>% rename(
  "Age" = RIDAGEYR,
  "HD (log)" = LOG_HD
)
             
pairs.panels(dfcor, stars= TRUE, method = "pearson", hist.col = "gray", cex.labels=1.9, ellipses = FALSE) 

##Correlation plot-Age Adjusted Measures 
dfcor<-df5[c("RIDAGEYR", "LM_age_resid", "HD_age_resid", "KDM_age_resid", "AL_age_resid")]
dfcor <- dfcor %>% rename(
  "Age" = RIDAGEYR,
  "HD (adjusted)" = HD_age_resid,
  "LM (adjusted)" = LM_age_resid,
  "KDM (adjusted)" = KDM_age_resid,
  "AL (adjusted)" = AL_age_resid
)

pairs.panels(dfcor, stars= TRUE, method = "pearson", hist.col = "gray", cex.labels=1.9, ellipses = FALSE)