//NHANES BIOLOGICAL AGING PARITY PROJECT 

// Set survey parameters

svyset [w= WTMEC12YR], psu ( SDMVPSU ) strata ( SDMVSTRA )

// Primary analyses -- all covariates
svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm LM_age_resid c.livebirths##menopause c.livebirths#c.livebirths c.livebirths#c.livebirths#menopause RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm HD_age_resid c.livebirths##menopause c.livebirths#c.livebirths c.livebirths#c.livebirths#menopause RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm KDM_age_resid c.livebirths##menopause c.livebirths#c.livebirths c.livebirths#c.livebirths#menopause RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm AL_age_resid c.livebirths##menopause c.livebirths#c.livebirths c.livebirths#c.livebirths#menopause RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

// Secondary analyses -- age covariate only
svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm LM_age_resid c.livebirths##menopause c.livebirths#c.livebirths c.livebirths#c.livebirths#menopause RIDAGEYR, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm HD_age_resid c.livebirths##menopause c.livebirths#c.livebirths c.livebirths#c.livebirths#menopause RIDAGEYR, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm KDM_age_resid c.livebirths##menopause c.livebirths#c.livebirths c.livebirths#c.livebirths#menopause RIDAGEYR, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm AL_age_resid c.livebirths##menopause c.livebirths#c.livebirths c.livebirths#c.livebirths#menopause RIDAGEYR, base

// PLOTTING MAIN MODELS
svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm LM_age_resid c.livebirths##menopause c.livebirths#c.livebirths c.livebirths#c.livebirths#menopause RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

margins menopause, at(livebirths=(0(1)7)) subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8) vce(unconditional) vsquish
marginsplot

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm HD_age_resid c.livebirths##menopause c.livebirths#c.livebirths c.livebirths#c.livebirths#menopause RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

margins menopause, at(livebirths=(0(1)7)) subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8) vce(unconditional) vsquish
marginsplot

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm KDM_age_resid c.livebirths##menopause c.livebirths#c.livebirths c.livebirths#c.livebirths#menopause RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

margins menopause, at(livebirths=(0(1)7)) subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8) vce(unconditional) vsquish
marginsplot

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm AL_age_resid c.livebirths##menopause c.livebirths#c.livebirths c.livebirths#c.livebirths#menopause RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

margins menopause, at(livebirths=(0(1)7)) subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8) vce(unconditional) vsquish
marginsplot

// DEMOGRAPHIC INFO
xi: svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths!=. & smoking!=. & BMI!=. &  DMDEDUC2!=. & INDFMPIR!=. & RIDRETH1!=. & menopause!=. & LM!=. & livebirths <8): mean RIDAGEYR BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1 i.menopause livebirths i.livebirths_dichot
char RIDRETH1 [omit] 2 
xi: svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths!=. & smoking!=. & BMI!=. &  DMDEDUC2!=. & INDFMPIR!=. & RIDRETH1!=. & menopause!=. & LM!=. & livebirths <8): mean RIDAGEYR BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1 i.menopause livebirths i.livebirths_dichot
char DMDEDUC2 [omit] 2
xi: svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths!=. & smoking!=. & BMI!=. &  DMDEDUC2!=. & INDFMPIR!=. & RIDRETH1!=. & menopause!=. & LM!=. & livebirths <8): mean RIDAGEYR BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1 i.menopause livebirths i.livebirths_dichot
char menopause [omit] 2
xi: svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths!=. & smoking!=. & BMI!=. &  DMDEDUC2!=. & INDFMPIR!=. & RIDRETH1!=. & menopause!=. & LM!=. & livebirths <8): mean RIDAGEYR BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1 i.menopause livebirths i.livebirths_dichot
char livebirths_dichot [omit] 2
xi: svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths!=. & smoking!=. & BMI!=. &  DMDEDUC2!=. & INDFMPIR!=. & RIDRETH1!=. & menopause!=. & LM!=. & livebirths <8): mean RIDAGEYR BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1 i.menopause livebirths i.livebirths_dichot
char smoking [omit] 3
xi: svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths!=. & smoking!=. & BMI!=. &  DMDEDUC2!=. & INDFMPIR!=. & RIDRETH1!=. & menopause!=. & LM!=. & livebirths <8): mean RIDAGEYR BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1 i.menopause livebirths i.livebirths_dichot 

// Sensitivity analyses for months since last birth -- all covariates
svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm LM_age_resid c.livebirths##c.monthssincebirth c.livebirths#c.livebirths c.livebirths#c.livebirths#c.monthssincebirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm HD_age_resid c.livebirths##c.monthssincebirth c.livebirths#c.livebirths c.livebirths#c.livebirths#c.monthssincebirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm KDM_age_resid c.livebirths##c.monthssincebirth c.livebirths#c.livebirths c.livebirths#c.livebirths#c.monthssincebirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm AL_age_resid c.livebirths##c.monthssincebirth c.livebirths#c.livebirths c.livebirths#c.livebirths#c.monthssincebirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

// Sensitivity analyses for years since last birth -- all covariates
svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm LM_age_resid c.livebirths##menopause c.livebirths#c.livebirths c.livebirths#c.livebirths#menopause c.livebirths##c.yearssincelastbirth c.livebirths#c.livebirths#c.yearssincelastbirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm HD_age_resid c.livebirths##menopause c.livebirths#c.livebirths c.livebirths#c.livebirths#menopause c.livebirths##c.yearssincelastbirth c.livebirths#c.livebirths#c.yearssincelastbirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm KDM_age_resid c.livebirths##menopause c.livebirths#c.livebirths c.livebirths#c.livebirths#menopause c.livebirths##c.yearssincelastbirth c.livebirths#c.livebirths#c.yearssincelastbirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  glm AL_age_resid c.livebirths##menopause c.livebirths#c.livebirths c.livebirths#c.livebirths#menopause c.livebirths##c.yearssincelastbirth c.livebirths#c.livebirths#c.yearssincelastbirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base