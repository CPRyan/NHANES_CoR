//NHANES BIOLOGICAL AGING PARITY PROJECT 

// Set survey parameters

svyset [w= WTMEC2YR], psu ( SDMVPSU ) strata ( SDMVSTRA )

// Primary analyses -- all covariates
svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2):  glm LM livebirths c.livebirths##c.livebirths c.livebirths#ib1.menopause RIDAGEYR BMI ib1.DMDEDUC2 INDFMPIR ib1.smoking  ib1.RIDRETH1, base
svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2):  glm HD livebirths c.livebirths##c.livebirths c.livebirths#ib1.menopause RIDAGEYR BMI ib1.DMDEDUC2 INDFMPIR ib1.smoking  ib1.RIDRETH1, base
svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2):  glm KDM livebirths c.livebirths##c.livebirths c.livebirths#ib1.menopause RIDAGEYR BMI ib1.DMDEDUC2 INDFMPIR ib1.smoking  ib1.RIDRETH1, base


// Secondary analyses -- age covariate only
svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2):  glm LM livebirths c.livebirths##c.livebirths c.livebirths#ib0.menopause RIDAGEYR, base
svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2):  glm HD livebirths c.livebirths##c.livebirths c.livebirths#ib0.menopause RIDAGEYR, base
svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2):  glm KDM livebirths c.livebirths##c.livebirths c.livebirths#ib0.menopause RIDAGEYR, base

// Following up on significant interactions
svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & menopause ==0 & RIDEXPRG ==2):  glm LM livebirths livebirths2 RIDAGEYR BMI ib1.DMDEDUC2  INDFMPIR ib1.smoking  ib1.RIDRETH1, base
svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & menopause ==1 & RIDEXPRG ==2):  glm LM livebirths livebirths2 RIDAGEYR BMI ib1.DMDEDUC2  INDFMPIR ib1.smoking  ib1.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & menopause ==0 & RIDEXPRG ==2):  glm LM livebirths_dichot RIDAGEYR BMI ib1.DMDEDUC2  INDFMPIR ib1.smoking  ib1.RIDRETH1, base
svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & menopause ==1 & RIDEXPRG ==2):  glm LM livebirths_dichot RIDAGEYR BMI ib1.DMDEDUC2  INDFMPIR ib1.smoking  ib1.RIDRETH1, base

// PLOTTING
svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2):  glm LM_acceleration livebirths c.livebirths##c.livebirths c.livebirths#menopause RIDAGEYR BMI ib1.DMDEDUC2 INDFMPIR ib1.smoking  ib1.RIDRETH1, base
margins menopause, at(livebirths=(0(1)10)) subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2) vce(unconditional) vsquish
marginsplot

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2):  glm HD_age_resid livebirths c.livebirths##c.livebirths c.livebirths#menopause RIDAGEYR BMI ib1.DMDEDUC2 INDFMPIR ib1.smoking  ib1.RIDRETH1, base
margins menopause, at(livebirths=(0(1)10)) subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2) vce(unconditional) vsquish
marginsplot

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2):  glm KDM_acceleration livebirths c.livebirths##c.livebirths c.livebirths#menopause RIDAGEYR BMI ib1.DMDEDUC2 INDFMPIR ib1.smoking  ib1.RIDRETH1, base
margins menopause, at(livebirths=(0(1)10)) subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2) vce(unconditional) vsquish
marginsplot

// DEMOGRAPHIC INFO
xi: svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths!=. & smoking!=. & BMI!=. &  DMDEDUC2!=. & INDFMPIR!=. & RIDRETH1!=. & menopause!=. & LM!=.): mean RIDAGEYR BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1 i.menopause livebirths i.livebirths_dichot
char RIDRETH1 [omit] 2 
xi: svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths!=. & smoking!=. & BMI!=. &  DMDEDUC2!=. & INDFMPIR!=. & RIDRETH1!=. & menopause!=. & LM!=.): mean RIDAGEYR BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1 i.menopause livebirths i.livebirths_dichot
char DMDEDUC2 [omit] 2
xi: svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths!=. & smoking!=. & BMI!=. &  DMDEDUC2!=. & INDFMPIR!=. & RIDRETH1!=. & menopause!=. & LM!=.): mean RIDAGEYR BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1 i.menopause livebirths i.livebirths_dichot
char menopause [omit] 2
xi: svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths!=. & smoking!=. & BMI!=. &  DMDEDUC2!=. & INDFMPIR!=. & RIDRETH1!=. & menopause!=. & LM!=.): mean RIDAGEYR BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1 i.menopause livebirths i.livebirths_dichot
char livebirths_dichot [omit] 2
xi: svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2 & livebirths!=. & smoking!=. & BMI!=. &  DMDEDUC2!=. & INDFMPIR!=. & RIDRETH1!=. & menopause!=. & LM!=.): mean RIDAGEYR BMI INDFMPIR i.smoking i.DMDEDUC2 i.RIDRETH1 i.menopause livebirths i.livebirths_dichot
