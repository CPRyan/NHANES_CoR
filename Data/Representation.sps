SET DECIMAL=DOT.

DATA LIST FILE= "~/Documents/GitHub/NHANES_CoR/Data/Representation.txt"  free (",")
ENCODING="Locale"
/ X SEQN SDDSRVYR RIAGENDR RIDAGEYR RIDRETH1 DMDEDUC2 INDFMPIR RIDEXPRG 
 WTMEC2YR SDMVPSU SDMVSTRA WTMEC4YR timespreg livebirths 
 mensregularity irregreason everpreg agelastbirth menopause 
 monthssincebirth SMQ020 SMQ040 smoking livebirths_dichot 
 yearssincelastbirth SAMPLE 
  .

VARIABLE LABELS
X "X" 
 SEQN "SEQN" 
 SDDSRVYR "SDDSRVYR" 
 RIAGENDR "RIAGENDR" 
 RIDAGEYR "RIDAGEYR" 
 RIDRETH1 "RIDRETH1" 
 DMDEDUC2 "DMDEDUC2" 
 INDFMPIR "INDFMPIR" 
 RIDEXPRG "RIDEXPRG" 
 WTMEC2YR "WTMEC2YR" 
 SDMVPSU "SDMVPSU" 
 SDMVSTRA "SDMVSTRA" 
 WTMEC4YR "WTMEC4YR" 
 timespreg "timespreg" 
 livebirths "livebirths" 
 mensregularity "mensregularity" 
 irregreason "irregreason" 
 everpreg "everpreg" 
 agelastbirth "agelastbirth" 
 menopause "menopause" 
 monthssincebirth "monthssincebirth" 
 SMQ020 "SMQ020" 
 SMQ040 "SMQ040" 
 smoking "smoking" 
 livebirths_dichot "livebirths_dichot" 
 yearssincelastbirth "yearssincelastbirth" 
 SAMPLE "SAMPLE" 
 .
VARIABLE LEVEL X, SEQN, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2, INDFMPIR, 
 RIDEXPRG, WTMEC2YR, SDMVPSU, SDMVSTRA, WTMEC4YR, timespreg, 
 livebirths, mensregularity, irregreason, everpreg, agelastbirth, 
 menopause, monthssincebirth, SMQ020, SMQ040, smoking, 
 livebirths_dichot, yearssincelastbirth, SAMPLE 
 (scale).

EXECUTE.
