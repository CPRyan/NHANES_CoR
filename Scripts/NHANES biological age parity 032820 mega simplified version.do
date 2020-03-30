//NHANES BIOLOGICAL AGING PARITY PROJECT -- mega simplified version of code 

// Set survey parameters

svyset [w= WTMEC2YR], psu ( SDMVPSU ) strata ( SDMVSTRA )

// Primary analyses -- all covariates
svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2):  glm LM livebirths c.livebirths##c.livebirths c.livebirths#ib1.menopause c.livebirths##c.livebirths#ib1.menopause RIDAGEYR BMI ib1.DMDEDUC2 INDFMPIR ib1.smoking  ib1.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2):  glm HD livebirths c.livebirths##c.livebirths c.livebirths#ib1.menopause c.livebirths##c.livebirths#ib1.menopause RIDAGEYR BMI ib1.DMDEDUC2 INDFMPIR ib1.smoking  ib1.RIDRETH1, base

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2):  glm KDM livebirths c.livebirths##c.livebirths c.livebirths#ib1.menopause c.livebirths##c.livebirths#ib1.menopause RIDAGEYR BMI ib1.DMDEDUC2 INDFMPIR ib1.smoking  ib1.RIDRETH1, base

// PLOTTING
svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2):  glm LM_acceleration livebirths c.livebirths##c.livebirths c.livebirths#menopause c.livebirths##c.livebirths#menopause RIDAGEYR BMI ib1.DMDEDUC2 INDFMPIR ib1.smoking  ib1.RIDRETH1, base
margins menopause, at(livebirths=(0(1)10)) subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2) vce(unconditional) vsquish
marginsplot

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2):  glm HD_age_resid livebirths c.livebirths##c.livebirths c.livebirths#menopause c.livebirths##c.livebirths#menopause RIDAGEYR BMI ib1.DMDEDUC2 INDFMPIR ib1.smoking  ib1.RIDRETH1, base
margins menopause, at(livebirths=(0(1)10)) subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2) vce(unconditional) vsquish
marginsplot

svy, subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2):  glm KDM_acceleration livebirths c.livebirths##c.livebirths c.livebirths#menopause c.livebirths##c.livebirths#menopause RIDAGEYR BMI ib1.DMDEDUC2 INDFMPIR ib1.smoking  ib1.RIDRETH1, base
margins menopause, at(livebirths=(0(1)10)) subpop(if RIDAGEYR > 17 & RIDAGEYR <= 84 & RIAGENDR ==2 & livebirths > -1 & RIDEXPRG ==2) vce(unconditional) vsquish
marginsplot