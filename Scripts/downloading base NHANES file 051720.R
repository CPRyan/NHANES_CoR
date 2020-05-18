#Download demo and covariate data 
library(purrr)
library(RNHANES)
library(devtools)
library(dplyr)
library(car)

#### Demographic files ####

demo_a <- nhanes_load_data("DEMO", "1999-2000")
demo_a_short <- demo_a[c("SEQN", "SDDSRVYR", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "DMDEDUC2", "INDFMPIR", "RIDEXPRG", "WTMEC2YR", "SDMVPSU", "SDMVSTRA")]

demo_b <- nhanes_load_data("DEMO", "2001-2002")
demo_b_short <- demo_b[c("SEQN", "SDDSRVYR", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "DMDEDUC2", "INDFMPIR", "RIDEXPRG", "WTMEC2YR", "SDMVPSU", "SDMVSTRA")]

demo_c <- nhanes_load_data("DEMO", "2003-2004")
demo_c_short <- demo_c[c("SEQN", "SDDSRVYR", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "DMDEDUC2", "INDFMPIR", "RIDEXPRG", "WTMEC2YR", "SDMVPSU", "SDMVSTRA")]

demo_d <- nhanes_load_data("DEMO", "2005-2006")
demo_d_short <- demo_d[c("SEQN", "SDDSRVYR", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "DMDEDUC2", "INDFMPIR", "RIDEXPRG", "WTMEC2YR", "SDMVPSU", "SDMVSTRA")]

demo_e <- nhanes_load_data("DEMO", "2007-2008")
demo_e_short <- demo_e[c("SEQN", "SDDSRVYR", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "DMDEDUC2", "INDFMPIR", "RIDEXPRG", "WTMEC2YR", "SDMVPSU", "SDMVSTRA")]

demo_f <- nhanes_load_data("DEMO", "2009-2010")
demo_f_short <- demo_f[c("SEQN", "SDDSRVYR", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "DMDEDUC2", "INDFMPIR", "RIDEXPRG", "WTMEC2YR", "SDMVPSU", "SDMVSTRA")]

demo_g <- nhanes_load_data("DEMO", "2011-2012")
demo_g_short <- demo_g[c("SEQN", "SDDSRVYR", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "DMDEDUC2", "INDFMPIR", "RIDEXPRG", "WTMEC2YR", "SDMVPSU", "SDMVSTRA")]

demo_h <- nhanes_load_data("DEMO", "2013-2014")
demo_h_short <- demo_h[c("SEQN", "SDDSRVYR", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "DMDEDUC2", "INDFMPIR", "RIDEXPRG", "WTMEC2YR", "SDMVPSU", "SDMVSTRA")]

devtools::install_github("silentspringinstitute/RNHANES")
demo_i <- nhanes_load_data("DEMO_I", "2015-2016")
demo_i_short <- demo_i[c("SEQN", "SDDSRVYR", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "DMDEDUC2", "INDFMPIR", "RIDEXPRG", "WTMEC2YR", "SDMVPSU", "SDMVSTRA")]

DEMO <- rbind(demo_a_short,demo_b_short, demo_c_short, demo_d_short, demo_e_short, demo_f_short, demo_g_short, demo_h_short, demo_i_short)

#Recode so that less than high school is grouped together (so combining DMDEDUC2 1 and 2)
DEMO$DMDEDUC2 <- car:::recode(DEMO$DMDEDUC2, '1 = 2')

#### Smoking files ####
smq_a <- nhanes_load_data("SMQ", "1999-2000", demographics = FALSE)
smq_a_short <- smq_a[c("SMQ020", "SMQ040", "SEQN")]

smq_b <- nhanes_load_data("SMQ", "2001-2002", demographics = FALSE)
smq_b_short <- smq_b[c("SMQ020", "SMQ040", "SEQN")]

smq_c <- nhanes_load_data("SMQ", "2003-2004", demographics = FALSE)
smq_c_short <- smq_c[c("SMQ020", "SMQ040", "SEQN")]

smq_d <- nhanes_load_data("SMQ", "2005-2006", demographics = FALSE)
smq_d_short <- smq_d[c("SMQ020", "SMQ040", "SEQN")]

smq_e <- nhanes_load_data("SMQ", "2007-2008", demographics = FALSE)
smq_e_short <- smq_e[c("SMQ020", "SMQ040", "SEQN")]

smq_f <- nhanes_load_data("SMQ", "2009-2010", demographics = FALSE)
smq_f_short <- smq_f[c("SMQ020", "SMQ040", "SEQN")]

smq_g <- nhanes_load_data("SMQ", "2011-2012", demographics = FALSE)
smq_g_short <- smq_g[c("SMQ020", "SMQ040", "SEQN")]

smq_h <- nhanes_load_data("SMQ", "2013-2014", demographics = FALSE)
smq_h_short <- smq_h[c("SMQ020", "SMQ040", "SEQN")]

smq_i <- nhanes_load_data("SMQ", "2015-2016", demographics = FALSE)
smq_i_short <- smq_i[c("SMQ020", "SMQ040", "SEQN")]

SMQ <- rbind(smq_a_short,smq_b_short, smq_c_short, smq_d_short, smq_e_short, smq_f_short, smq_g_short, smq_h_short, smq_i_short)

SMQ$smoking <- 2 #current smoker
SMQ$smoking[SMQ$SMQ020==2]<-0 #never smoker
SMQ$smoking[SMQ$SMQ040==3]<-1 #past smoker

#### Reproductive health questions #### 
rhq_a <- nhanes_load_data("RHQ", "1999-2000", demographics = FALSE)
rhq_a_short <- rhq_a[c("RHQ160", "RHD170", "RHQ030", "RHQ040", "SEQN", "RHD130", "RHQ190")]
colnames(rhq_a_short) <- c("timespreg", "livebirths", "mensregularity", "irregreason", "SEQN", "everpreg", "agelastbirth")
rhq_a_short$menopause <- 0
rhq_a_short$menopause[rhq_a_short$irregreason == 5] <- 1
rhq_a_short$monthssincebirth <- NA

rhq_b <- nhanes_load_data("RHQ", "2001-2002", demographics = FALSE)
rhq_b_short <- rhq_b[c("RHQ160", "RHD170", "RHQ030", "RHQ040", "SEQN", "RHD130", "RHQ190")]
colnames(rhq_b_short) <- c("timespreg", "livebirths", "mensregularity", "irregreason", "SEQN", "everpreg", "agelastbirth")
rhq_b_short$menopause <- 0
rhq_b_short$menopause[rhq_b_short$irregreason == 5] <- 1
rhq_b_short$monthssincebirth <- NA

rhq_c <- nhanes_load_data("RHQ", "2003-2004", demographics = FALSE)
rhq_c_short <- rhq_c[c("RHQ160", "RHD170", "RHQ031", "RHD042", "SEQN", "RHQ131", "RHQ190")]
colnames(rhq_c_short) <- c("timespreg", "livebirths", "mensregularity", "irregreason", "SEQN", "everpreg", "agelastbirth")
rhq_c_short$menopause <- 0
rhq_c_short$menopause[rhq_c_short$irregreason == 7] <- 1
rhq_c_short$monthssincebirth <- NA

rhq_d <- nhanes_load_data("RHQ", "2005-2006", demographics = FALSE)
rhq_d_short <- rhq_d[c("RHQ160", "RHQ171", "RHQ031", "RHD042", "SEQN", "RHQ131", "RHQ190")]
colnames(rhq_d_short) <- c("timespreg", "livebirths", "mensregularity", "irregreason", "SEQN", "everpreg", "agelastbirth")
rhq_d_short$menopause <- 0
rhq_d_short$menopause[rhq_d_short$irregreason == 7] <- 1
rhq_d_short$monthssincebirth <- NA

rhq_e <- nhanes_load_data("RHQ", "2007-2008", demographics = FALSE)
rhq_e_short <- rhq_e[c("RHQ160", "RHQ171", "RHQ031", "RHD042", "SEQN", "RHQ131", "RHD190", "RHQ197")]
colnames(rhq_e_short) <- c("timespreg", "livebirths", "mensregularity", "irregreason", "SEQN", "everpreg", "agelastbirth", "monthssincebirth")
rhq_e_short$menopause <- 0
rhq_e_short$menopause[rhq_e_short$irregreason == 7] <- 1

rhq_f <- nhanes_load_data("RHQ", "2009-2010", demographics = FALSE)
rhq_f_short <- rhq_f[c("RHQ160", "RHQ171", "RHQ031", "RHD042", "SEQN", "RHQ131", "RHD190", "RHQ197")]
colnames(rhq_f_short) <- c("timespreg", "livebirths", "mensregularity", "irregreason", "SEQN", "everpreg", "agelastbirth", "monthssincebirth")
rhq_f_short$menopause <- 0
rhq_f_short$menopause[rhq_f_short$irregreason == 7] <- 1

rhq_g <- nhanes_load_data("RHQ", "2011-2012", demographics = FALSE)
rhq_g_short <- rhq_g[c("RHQ160", "RHQ171", "RHQ031", "RHD042", "SEQN", "RHQ131", "RHD190", "RHQ197")]
colnames(rhq_g_short) <- c("timespreg", "livebirths", "mensregularity", "irregreason", "SEQN", "everpreg", "agelastbirth", "monthssincebirth")
rhq_g_short$menopause <- 0
rhq_g_short$menopause[rhq_g_short$irregreason == 7] <- 1

rhq_h <- nhanes_load_data("RHQ", "2013-2014", demographics = FALSE)
rhq_h_short <- rhq_h[c("RHQ160", "RHQ171", "RHQ031", "RHD043", "SEQN", "RHQ131", "RHD190", "RHQ197")]
colnames(rhq_h_short) <- c("timespreg", "livebirths", "mensregularity", "irregreason", "SEQN", "everpreg", "agelastbirth", "monthssincebirth")
rhq_h_short$menopause <- 0
rhq_h_short$menopause[rhq_h_short$irregreason == 7 | rhq_h_short$irregreason == 3] <- 1

rhq_i <- nhanes_load_data("RHQ", "2015-2016", demographics = FALSE)
rhq_i_short <- rhq_i[c("RHQ160", "RHQ171", "RHQ031", "RHD043", "SEQN", "RHQ131", "RHD190", "RHQ197")]
colnames(rhq_i_short) <- c("timespreg", "livebirths", "mensregularity", "irregreason", "SEQN", "everpreg", "agelastbirth", "monthssincebirth")
rhq_i_short$menopause <- 0
rhq_i_short$menopause[rhq_i_short$irregreason == 7 | rhq_i_short$irregreason == 3] <- 1

RHQ <- rbind (rhq_a_short, rhq_b_short, rhq_c_short, rhq_d_short, rhq_e_short, rhq_f_short, rhq_g_short, rhq_h_short, rhq_i_short)

RHQ$livebirths <- na_if(RHQ$livebirths, 77)
RHQ$livebirths <- na_if(RHQ$livebirths, 99)
RHQ$mensregularity <- na_if(RHQ$mensregularity, 7)
RHQ$mensregularity <- na_if(RHQ$mensregularity, 9)
RHQ$agelastbirth <- na_if(RHQ$agelastbirth, 777)
RHQ$agelastbirth <- na_if(RHQ$agelastbirth, 999)
RHQ$agelastbirth <- car:::recode(RHQ$agelastbirth, '0:14 = NA; 45:99 = NA')

RHQ$livebirths[RHQ$livebirths > 11] <- 11 # INTERPRETED AS 11 OR MORE because top-coded at 11 for some cycles

##### Merge all ####

NHANES <- merge(DEMO, RHQ, by = "SEQN", all = TRUE)
NHANES <- merge (NHANES, SMQ, by = "SEQN", all = TRUE)

##### Recoding ####

NHANES$everpreg <- na_if (NHANES$everpreg, 7)
NHANES$everpreg <- na_if (NHANES$everpreg, 9)

NHANES$DMDEDUC2 <- na_if (NHANES$DMDEDUC2, 7)
NHANES$DMDEDUC2 <- na_if (NHANES$DMDEDUC2, 9)

NHANES$menopause[NHANES$RIDAGEYR > 61 & NHANES$RIAGENDR == 2] <- 1
NHANES$menopause[NHANES$RIDAGEYR < 41 & NHANES$RIAGENDR == 2] <- 0

NHANES$livebirths[NHANES$everpreg == 2] <- 0
NHANES$livebirths_dichot[NHANES$livebirths == 0] <- 0
NHANES$livebirths_dichot[NHANES$livebirths > 0] <- 1

NHANES$RIDRETH1[NHANES$RIDRETH1 == 3] <- 0 #NH white
NHANES$RIDRETH1[NHANES$RIDRETH1 == 1 | NHANES$RIDRETH1 == 2 ] <-1 #hispanic
NHANES$RIDRETH1[NHANES$RIDRETH1 == 4] <- 2 #NH black
NHANES$RIDRETH1[NHANES$RIDRETH1 == 5] <- 3 #other

NHANES$yearssincelastbirth <- NHANES$RIDAGEYR - NHANES$agelastbirth

write.csv (NHANES, "NHANES_051720.csv")
