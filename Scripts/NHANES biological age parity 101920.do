//NHANES BIOLOGICAL AGING PARITY PROJECT 

// Set survey parameters

svyset [w= WTMEC12YR], psu ( SDMVPSU ) strata ( SDMVSTRA )

// Primary analyses and margins -- premenopausal

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==1):  glm LM_age_resid2_premeno c.livebirths c.livebirths#c.livebirths RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

margins, at(livebirths=(0(1)7)) subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==1) vce(unconditional) vsquish

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==1):  glm HD_age_resid2_premeno c.livebirths c.livebirths#c.livebirths RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

margins, at(livebirths=(0(1)7)) subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==1) vce(unconditional) vsquish

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==1):  glm KDM_age_resid2_premeno c.livebirths c.livebirths#c.livebirths RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

margins, at(livebirths=(0(1)7)) subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==1) vce(unconditional) vsquish

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==1):  glm AL_age_resid2_premeno c.livebirths c.livebirths#c.livebirths RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

margins, at(livebirths=(0(1)7)) subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==1) vce(unconditional) vsquish

// Primary analyses and margins -- postmenopausal

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==2):  glm LM_age_resid2_postmeno c.livebirths c.livebirths#c.livebirths RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

margins, at(livebirths=(0(1)7)) subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==2) vce(unconditional) vsquish

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==2):  glm HD_age_resid2_postmeno c.livebirths c.livebirths#c.livebirths RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

margins, at(livebirths=(0(1)7)) subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==2) vce(unconditional) vsquish

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==2):  glm KDM_age_resid2_postmeno c.livebirths c.livebirths#c.livebirths RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

margins, at(livebirths=(0(1)7)) subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==2) vce(unconditional) vsquish

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==2):  glm AL_age_resid2_postmeno c.livebirths c.livebirths#c.livebirths RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

margins, at(livebirths=(0(1)7)) subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==2) vce(unconditional) vsquish

// Secondary analyses -- age covariate only, premenopausal

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==1):  glm LM_age_resid3_premeno c.livebirths c.livebirths#c.livebirths RIDAGEYR, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==1):  glm HD_age_resid3_premeno c.livebirths c.livebirths#c.livebirths RIDAGEYR, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==1):  glm KDM_age_resid3_premeno c.livebirths c.livebirths#c.livebirths RIDAGEYR, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==1):  glm AL_age_resid3_premeno c.livebirths c.livebirths#c.livebirths RIDAGEYR, base

// Secondary analyses -- age covariate only, postmenopausal

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==2):  glm LM_age_resid3_postemeno c.livebirths c.livebirths#c.livebirths RIDAGEYR, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==2):  glm HD_age_resid3_postmeno c.livebirths c.livebirths#c.livebirths RIDAGEYR, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==2):  glm KDM_age_resid3_postmeno c.livebirths c.livebirths#c.livebirths RIDAGEYR, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause==2):  glm AL_age_resid3_postmeno c.livebirths c.livebirths#c.livebirths RIDAGEYR, base

// Secondary analyses -- years since last live birth, premenopausal

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause ==1):  glm LM_age_resid4_premeno c.livebirths c.livebirths#c.livebirths c.livebirths##c.yearssincelastbirth c.livebirths#c.livebirths#c.yearssincelastbirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause ==1):  glm HD_age_resid4_premeno c.livebirths c.livebirths#c.livebirths c.livebirths##c.yearssincelastbirth c.livebirths#c.livebirths#c.yearssincelastbirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause ==1):  glm KDM_age_resid4_premeno c.livebirths c.livebirths#c.livebirths c.livebirths##c.yearssincelastbirth c.livebirths#c.livebirths#c.yearssincelastbirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause ==1):  glm AL_age_resid4_premeno c.livebirths c.livebirths#c.livebirths c.livebirths##c.yearssincelastbirth c.livebirths#c.livebirths#c.yearssincelastbirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

// Secondary analyses -- years since last live birth, postmenopausal

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause ==2):  glm LM_age_resid4_postmeno c.livebirths c.livebirths#c.livebirths c.livebirths##c.yearssincelastbirth c.livebirths#c.livebirths#c.yearssincelastbirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause ==2):  glm HD_age_resid4_postmeno c.livebirths c.livebirths#c.livebirths c.livebirths##c.yearssincelastbirth c.livebirths#c.livebirths#c.yearssincelastbirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause ==2):  glm KDM_age_resid4_postmeno c.livebirths c.livebirths#c.livebirths c.livebirths##c.yearssincelastbirth c.livebirths#c.livebirths#c.yearssincelastbirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause ==2):  glm AL_age_resid4_postmeno c.livebirths c.livebirths#c.livebirths c.livebirths##c.yearssincelastbirth c.livebirths#c.livebirths#c.yearssincelastbirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

// Secondary analyses --months since last live birth (premenopausal only)

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause ==1):  glm LM_age_resid4_premeno c.livebirths c.livebirths#c.livebirths c.livebirths##c.monthssincebirth c.livebirths#c.livebirths#c.monthssincebirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause ==1):  glm HD_age_resid4_premeno c.livebirths c.livebirths#c.livebirths c.livebirths##c.monthssincebirth c.livebirths#c.livebirths#c.monthssincebirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause ==1):  glm KDM_age_resid4_premeno c.livebirths c.livebirths#c.livebirths c.livebirths##c.monthssincebirth c.livebirths#c.livebirths#c.monthssincebirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8 & menopause ==1):  glm AL_age_resid4_premeno c.livebirths c.livebirths#c.livebirths c.livebirths##c.monthssincebirth c.livebirths#c.livebirths#c.monthssincebirth RIDAGEYR BMI c.BMI#c.BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1, base

// DEMOGRAPHIC INFO -- PREMENOPAUSAL 
xi: svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths!=. & smoking!=. & BMI!=. &  DMDEDUC2!=. & INDFMPIR!=. & RIDRETH1!=. & menopause== 1 & LM!=. & livebirths <8): mean RIDAGEYR BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1 i.menopause livebirths i.livebirths_dichot LM KDM LOG_HD AL LM_age_resid KDM_age_resid HD_age_resid AL_age_resid

// DEMOGRAPHIC INFO -- POSTMENOPAUSAL 
xi: svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths!=. & smoking!=. & BMI!=. &  DMDEDUC2!=. & INDFMPIR!=. & RIDRETH1!=. & menopause== 2 & LM!=. & livebirths <8): mean RIDAGEYR BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1 i.menopause livebirths i.livebirths_dichot LM KDM LOG_HD AL LM_age_resid KDM_age_resid HD_age_resid AL_age_resid


// Tests between groups
svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  regress RIDAGEYR menopause

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  regress BMI menopause

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  regress INDFMPIR menopause

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  regress livebirths menopause

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  tab smoking menopause

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  tab DMDEDUC2 menopause

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  tab RIDRETH1 menopause

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths <8):  tab livebirths_dichot menopause

