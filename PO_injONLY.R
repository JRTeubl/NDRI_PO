dfAll <- read.csv("/Users/jrteubl/Desktop/heroin_data/2016-06-29-database-merge-umass-N539.csv", header=T, sep=",")

inj <- dfAll$FirstInj_Note1.FirstInj_1
# Have you ever injected a drug for non-medical reasons?
# 0: No
# 1: Yes
# 77: Not Applicable
# 88: Don't know
# 99: Refuse to answer
injGrp <- rep(0, nrow(dfAll))
injGrp[inj == 1] <- 1

poInjEver <- dfAll$LifetimeSU_Note1.LifetimeSU_3
poInjEverGrp <- rep(0, nrow(dfAll))
poInjEverGrp[poInjEver == 1]<- 1

poRegInj <- dfAll$LifetimeSU_Note1.LifetimeSU_4
poRegInjGrp <- rep(0, nrow(dfAll))
poRegInjGrp[poRegInj > 0] <- 1

poInj <- rep(NaN, nrow(dfAll))
poInj[poInjEverGrp == 0] <- 1
poInj[poInjEverGrp == 1 & poRegInjGrp == 0] <- 2
poInj[poRegInjGrp == 1] <- 3
# 1: never injected POs
# 2: injected POs but not regularly
# 3: regularly injected POs


#### Demographics #### 

gender <- dfAll$Soc_Note1.Soc_10 # 1 = male, 2 = female
genderGrp <- rep(NaN, nrow(dfAll))
genderGrp[gender == 1] <- 0
genderGrp[gender == 2] <- 1


latino <- dfAll$Soc_Note1.Soc_4 #0 = no, 1 = yes
latinoGrp <- rep(NaN, nrow(dfAll))
latinoGrp[latino == 0] <- 0
latinoGrp[latino == 1] <- 1


Wht <- dfAll$Soc_Note1.Soc_5.5
race <- rep(NaN, nrow(dfAll))
race[Wht == TRUE] <- 1
race[Wht == FALSE] <- 0

homeless <- dfAll$Soc_Note1.Soc_11 #0 = no, 1 = yes
homelessGrp <- rep(NaN, nrow(dfAll))
homelessGrp[homeless == 0] <- 0
homelessGrp[homeless == 1] <- 1

gender <- dfAll$Soc_Note1.Soc_10 # 1 = male, 2 = female
genderGrp <- rep(NaN, nrow(dfAll))
genderGrp[gender == 1] <- 0
genderGrp[gender == 2] <- 1


latino <- dfAll$Soc_Note1.Soc_4 #0 = no, 1 = yes
latinoGrp <- rep(NaN, nrow(dfAll))
latinoGrp[latino == 0] <- 0
latinoGrp[latino == 1] <- 1


Wht <- dfAll$Soc_Note1.Soc_5.5
race <- rep(NaN, nrow(dfAll))
race[Wht == TRUE] <- 1
race[Wht == FALSE] <- 0


income <- dfAll$Soc_Note1.Soc_23
# 1- $0-$25,000
# 2- $26,000 - $50,000
# 3- $51,000 - $75,000
# 4- $76,000 - $100,000
# 5- $101,000 - $125,000
# 6- $126,000 - $150,000
# 7- $151,000 - $200,000
# 8- $201,000 - $250,000
# 9- $251,000 +
# 77 - NA
# 88 - Don't know
# 99 - Refused to answer


incomeGrp <- rep(NaN, nrow(dfAll))
incomeGrp[income == 1 | income == 2] <- 1
incomeGrp[income == 3 | income == 4] <- 2
incomeGrp[income == 5 | income == 6 | income == 7 | income == 8 | income == 9] <- 3

HepC <- dfAll$hiv_hcv.HepatitisCTestResults
# 0- Negative
# 1- Positive
# 77- NA
HepCGrp <- rep(NaN, nrow(dfAll))
HepCGrp[HepC == 0] <- 0
HepCGrp[HepC == 1] <- 1

overdose <- dfAll$Overdose_8
# Have you ever overdosed to the point where you lost consciousness, stopped breathing, or were unresponsive
# 0 = No 
# 1 = Yes
# 77 = Not Applicable
# 88 = Don't know
# 99 = Refuse to Answer
overdoseGrp <- rep(NaN, nrow(dfAll))
overdoseGrp[overdose == 1] <- 1
overdoseGrp[overdose == 0] <- 0

date <- as.Date(dfAll$Soc_Note1.Soc_1, "%m/%d/%y")
birthdate <- as.Date(dfAll$Soc_Note1.Soc_3, "%m/%d/%y")
age <- (date - birthdate)/365.25

df <- data.frame(injGrp, poInj, genderGrp, latinoGrp, race, homelessGrp, incomeGrp, HepCGrp,
                 overdoseGrp, age)
df <- df[injGrp == 1, ]

### Percentages of demographics ###
meanAge <- mean(df$age, na.rm = TRUE)
stdAge <- sd(df$age, na.rm = TRUE)

ageNP <- paste(round(meanAge, digits = 1), ' (+/-', round(stdAge, digits = 1), ')', sep = '')

genderGrp <- df$genderGrp
gen <- genderGrp[!is.na(genderGrp)]
genderFnum <- length(gen[gen == 1])
genderFper <- 100*(length(gen[gen == 1])/length(gen))
genderFnp <- paste(genderFnum, ' (', round(genderFper, digits = 2), ')', sep = '')
genderMnum <- length(gen[gen == 0])
genderMPer <- 100*(length(gen[gen == 0])/length(gen))
genderMnp <- paste(genderMnum, ' (', round(genderMPer, digits = 2), ')', sep = '')

homelessGrp <- df$homelessGrp
homelessNum <- length(homelessGrp[homelessGrp == 1])
homelessPer <- 100*(length(homelessGrp[homelessGrp == 1])/length(homelessGrp))
homelessNP <- paste(homelessNum, ' (', round(homelessPer, digits = 2), ')', sep = '')

latinoGrp <- df$latinoGrp
lat <- latinoGrp[!is.na(latinoGrp)]
latinoNum <- length(lat[lat == 1])
latinoPer <- 100*(length(lat[lat == 1])/length(lat))
latinoNP <- paste(latinoNum, ' (', round(latinoPer, digits = 2), ')', sep = '')

race <- df$race
raceNum <- length(race[race == 1])
racePer <- 100*(length(race[race == 1])/length(race))
raceNP <- paste(raceNum, ' (', round(racePer, digits = 2), ')', sep = '')

incomeGrp <- df$incomeGrp
inc <- incomeGrp[!is.na(incomeGrp)]
income1Num <- length(inc[inc == 1])
income1Per <- 100*(length(inc[inc == 1])/length(inc))
income1NP <- paste(income1Num, ' (', round(income1Per, digits = 2), ')', sep = '')
income2Num <- length(inc[inc == 2])
income2Per <- 100*(length(inc[inc == 2])/length(inc))
income2NP <- paste(income2Num, ' (', round(income2Per, digits = 2), ')', sep = '')
income3Num <- length(inc[inc == 3])
income3Per <- 100*(length(inc[inc == 3])/length(inc))
income3NP <- paste(income3Num, ' (', round(income3Per, digits = 2), ')', sep = '')

HepCGrp <- df$HepCGrp
Hep <- HepCGrp[!is.na(HepCGrp)]
hepCnum <- length(Hep[Hep == 1])
hepcPer <- 100*(length(Hep[HepCGrp == 1])/length(Hep))
hepCNP <- paste(hepCnum, ' (', round(hepcPer, digits = 2), ')', sep = '')

overdoseGrp <- df$overdoseGrp
od <- overdoseGrp[!is.na(overdoseGrp)]
odNum <- length(od[od == 1])
odPer <- 100*(length(od[od == 1])/length(od))
odNP <- paste(odNum, ' (', round(odPer, digits = 2), ')', sep = '')


nps <- rbind(ageNP, genderMnp, genderFnp, homelessNP, latinoNP, raceNP, income1NP, income2NP, income3NP, hepCNP, odNP)
colnames(nps) <- 'Sample n(%)'
rownames(nps) <- c('Age', 'Male', 'Female', 'Homeless', 'Latino', 'White', '$0 - $50.000', '$51.000 - $100.000', '$101.000 +',
                        'Hep C +', 'Overdose')

write.csv(nps, "/Users/jrteubl/Desktop/heroin_data/PO/sampleNP_INJONLY.csv", quote = FALSE)



####### Number and Percentages ##########

ndf <- nrow(df)
dfNev <- df[df$poInj == 1, ]
nNev <- nrow(dfNev)
perNev <- round(100 * nNev/ndf, digits = 2)
npNev <- paste(nNev, ' (', perNev, ')', sep = '')

dfSome <- df[df$poInj == 2, ]
nSome <- nrow(dfSome)
perSome <- round(100 * nSome/ndf, digits = 2)
npSome <- paste(nSome, ' (', perSome, ')', sep = '')

dfAlw <- df[df$poInj == 3, ]
nAlw <- nrow(dfAlw)
perAlw <- round(100 * nAlw/ndf, digits = 2)
npAlw <- paste(nAlw, ' (', perAlw, ')', sep = '')

totals <- cbind(npNev, npSome, npAlw)
colnames(totals) <- c('Never injected POs', 'Occasionally injects POs', 'Regularly injects POs')
rownames(totals) <- 'n (percent)'

numPerTable <- function(column, num, name){
  dftemp <- df[df$poInj == num, ]
  x <- table(dftemp[column])
  total <- x[1] + x[2]
  x1 <- x[1]
  per1 <- round(100 * x1/total, digits = 2)
  np1 <- paste(x1, ' (', per1, ')', sep = '')
  x2 <- x[2]
  per2 <- round(100 * x2/total, digits = 2)
  np2 <- paste(x2, ' (', per2, ')', sep = '')
  npX <- rbind(np1, np2)
  colnames(npX) <- name
  return(npX)
}

npHomeless1 <- numPerTable('homelessGrp', 1, 'Never injected POs')
npHomeless2 <- numPerTable('homelessGrp', 2, 'Occasionally injects POs')
npHomeless3 <- numPerTable('homelessGrp', 3, 'Regularly injects POs')
npHomeless <- cbind(npHomeless1, npHomeless2, npHomeless3)
rownames(npHomeless) <- c('Never homeless', 'homeless')

npGender1 <- numPerTable('genderGrp', 1, 'Never injected POs')
npGender2 <- numPerTable('genderGrp', 2, 'Occasionally injects POs')
npGender3 <- numPerTable('genderGrp', 3, 'Regularly injects POs')
npGender <- cbind(npGender1, npGender2, npGender3)
rownames(npGender) <- c('Male', 'Female')

npLatino1 <- numPerTable('latinoGrp', 1, 'Never injected POs')
npLatino2 <- numPerTable('latinoGrp', 2, 'Occasionally injects POs')
npLatino3 <- numPerTable('latinoGrp', 3, 'Regularly injects POs')
npLatino <- cbind(npLatino1, npLatino2, npLatino3)
rownames(npLatino) <- c('non-Latino', 'Latino')

npRace1 <- numPerTable('race', 1, 'Never injected POs')
npRace2 <- numPerTable('race', 2,'Occasionally injects POs')
npRace3 <- numPerTable('race', 3, 'Regularly injects POs')
npRace <- cbind(npRace1, npRace2, npRace3)
rownames(npRace) <- c('non-White', 'White')

npHepC1 <- numPerTable('HepCGrp', 1, 'Never injected POs')
npHepC2 <- numPerTable('HepCGrp', 2,'Occasionally injects POs')
npHepC3 <- numPerTable('HepCGrp', 3, 'Regularly injects POs')
npHepC <- cbind(npHepC1, npHepC2, npHepC3)
rownames(npHepC) <- c('HCV neg', 'HCV pos')

npOD1 <- numPerTable('overdoseGrp', 1, 'Never injected POs')
npOD2 <- numPerTable('overdoseGrp', 2,'Occasionally injects POs')
npOD3 <- numPerTable('overdoseGrp', 3, 'Regularly injects POs')
npOD <- cbind(npOD1, npOD2, npOD3)
rownames(npOD) <- c('Never OD', 'Experienced OD')

df1 <- df[df$poInj == 1, ]
x <- table(df1['incomeGrp'])
total <- x[1] + x[2] + x[3]
x1 <- x[1]
per1 <- round(100 * x1/total, digits = 2)
np1 <- paste(x1, ' (', per1, ')', sep = '')
x2 <- x[2]
per2 <- round(100 * x2/total, digits = 2)
np2 <- paste(x2, ' (', per2, ')', sep = '')
x3 <- x[3]
per3 <- round(100 * x3/total, digits = 2)
np3 <- paste(x3, ' (', per3, ')', sep = '')
npNev <- rbind(np1, np2, np3)
colnames(npNev) <- 'Never injected POs'

df2 <- df[df$poInj == 2, ]
x <- table(df2['incomeGrp'])
total <- x[1] + x[2] + x[3]
x1 <- x[1]
per1 <- round(100 * x1/total, digits = 2)
np1 <- paste(x1, ' (', per1, ')', sep = '')
x2 <- x[2]
per2 <- round(100 * x2/total, digits = 2)
np2 <- paste(x2, ' (', per2, ')', sep = '')
x3 <- x[3]
per3 <- round(100 * x3/total, digits = 2)
np3 <- paste(x3, ' (', per3, ')', sep = '')
npSome <- rbind(np1, np2, np3)
colnames(npSome) <- 'Occasionally injects POs'

df3 <- df[df$poInj == 3, ]
x <- table(df3['incomeGrp'])
total <- x[1] + x[2] + x[3]
x1 <- x[1]
per1 <- round(100 * x1/total, digits = 2)
np1 <- paste(x1, ' (', per1, ')', sep = '')
x2 <- x[2]
per2 <- round(100 * x2/total, digits = 2)
np2 <- paste(x2, ' (', per2, ')', sep = '')
x3 <- x[3]
per3 <- round(100 * x3/total, digits = 2)
np3 <- paste(x3, ' (', per3, ')', sep = '')
npReg <- rbind(np1, np2, np3)
colnames(npReg) <- 'Regularly injects POs'

npInc <- cbind(npNev, npSome, npReg)
rownames(npInc) <-  c('$0 - $50.000', '$51.000 - $100.000', '$101.000 +')

npTable <- rbind(totals, npGender, npHomeless, npLatino, npRace, npHepC, npOD, npInc)
write.csv(npTable, "/Users/jrteubl/Desktop/heroin_data/PO/CategoryNumPercent_INJONLY.csv", quote = FALSE, na = "0")


#### Chi square test ####

chiSqTable <- function(DepVar, name){
  tbl <- xtabs(~df$poInj+DepVar)
  csq <- chisq.test(tbl)
  vals <- cbind(csq$statistic, csq$p.value)
  colnames(vals) <- c("chi-sq value", "p-value")
  rownames(vals) <- name
  return(vals)
}

homelessTbl <- chiSqTable(df$homelessGrp, "Homeless")
genderTbl <- chiSqTable(df$genderGrp, "Gender")
latinoTbl <- chiSqTable(df$latinoGrp, "Ethnicity")
raceTbl <- chiSqTable(df$race, "Race")
incomeTbl <- chiSqTable(df$incomeGrp, "Income")
hcvTbl <- chiSqTable(df$HepCGrp, "HCV")
odTbl <- chiSqTable(df$overdoseGrp, "overdose")

csqTable <- rbind(homelessTbl, genderTbl, latinoTbl, raceTbl, incomeTbl, hcvTbl, odTbl)
write.csv(csqTable, "/Users/jrteubl/Desktop/heroin_data/PO/chisq_INJONLY.csv", quote = FALSE, na = "0")


#### Odds ratio ####

oddsratioWald.proc <- function(n00, n01, n10, n11, alpha = 0.05){
  #
  #  Compute the odds ratio between two binary variables, x and y,
  #  as defined by the four numbers nij:
  #
  #    n00 = number of cases where x = 0 and y = 0
  #    n01 = number of cases where x = 0 and y = 1
  #    n10 = number of cases where x = 1 and y = 0
  #    n11 = number of cases where x = 1 and y = 1
  #
  if(n00 == 0){n00 = .5}
  if(n01 == 0){n01 = .5}
  if(n10 == 0){n10 = .5}
  if(n11 == 0){n11 = .5}
  OR <- (n00 * n11)/(n01 * n10)
  #
  #  Compute the Wald confidence intervals:
  #
  siglog <- sqrt((1/n00) + (1/n01) + (1/n10) + (1/n11))
  zalph <- qnorm(1 - alpha/2)
  logOR <- log(OR)
  loglo <- logOR - zalph * siglog
  loghi <- logOR + zalph * siglog
  
  ORlo <- exp(loglo)
  ORhi <- exp(loghi)
  SE <- (loghi - loglo)/(2*1.96)
  z <- abs(logOR/SE)
  p <- exp(-.717*z-.416*z**2)
  
  oframe <- data.frame(LowerCI = ORlo, OR = OR, UpperCI = ORhi, alpha = alpha, pval = p)
  oframe
}

makeORtable <- function(df, col1, col2, num1, num2){
  var00 <- nrow(df[col1 == num1 & col2 == 0, ])
  var01 <- nrow(df[col1 == num1 & col2 == 1, ])
  var10 <- nrow(df[col1 == num2 & col2 == 0, ])
  var11 <- nrow(df[col1 == num2 & col2 == 1, ])
  or <- oddsratioWald.proc(var00, var01, var10, var11)
  return(or)
}

genderOdds12 <- makeORtable(df, df$poInj, df$genderGrp, 1, 2)
homelessOdds12 <- makeORtable(df, df$poInj, df$homelessGrp, 1, 2)
ethnicityOdds12 <- makeORtable(df, df$poInj, df$latinoGrp, 1, 2)
raceOdds12 <- makeORtable(df, df$poInj, df$race, 1, 2)
HepCOdds12 <- makeORtable(df, df$poInj, df$HepCGrp, 1, 2)
overdoseOdds12 <- makeORtable(df, df$poInj, df$overdoseGrp, 1, 2)
df12 <- rbind(genderOdds12, homelessOdds12, ethnicityOdds12, raceOdds12, HepCOdds12, overdoseOdds12)
rownames(df12) <- c('Gender', 'Homeless', 'Ethnicity', 'Race', 'Hep C +', 'Overdose')
df12$alpha <- NULL
write.csv(df12, '/Users/jrteubl/Desktop/heroin_data/PO/OR_Never_vs_Sporadic.csv', quote = FALSE, na = "0")


genderOdds13 <- makeORtable(df, df$poInj, df$genderGrp, 1, 3)
homelessOdds13 <- makeORtable(df, df$poInj, df$homelessGrp, 1, 3)
ethnicityOdds13 <- makeORtable(df, df$poInj, df$latinoGrp, 1, 3)
raceOdds13 <- makeORtable(df, df$poInj, df$race, 1, 3)
HepCOdds13 <- makeORtable(df, df$poInj, df$HepCGrp, 1, 3)
overdoseOdds13 <- makeORtable(df, df$poInj, df$overdoseGrp, 1, 3)
df13 <- rbind(genderOdds13, homelessOdds13, ethnicityOdds13, raceOdds13, HepCOdds13, overdoseOdds13)
rownames(df13) <- c('Gender', 'Homeless', 'Ethnicity', 'Race', 'Hep C +', 'Overdose')
df13$alpha <- NULL
write.csv(df13, '/Users/jrteubl/Desktop/heroin_data/PO/OR_Never_vs_Regular.csv', quote = FALSE, na = "0")

genderOdds23 <- makeORtable(df, df$poInj, df$genderGrp, 2, 3)
homelessOdds23 <- makeORtable(df, df$poInj, df$homelessGrp, 2, 3)
ethnicityOdds23 <- makeORtable(df, df$poInj, df$latinoGrp, 2, 3)
raceOdds23 <- makeORtable(df, df$poInj, df$race, 2, 3)
HepCOdds23 <- makeORtable(df, df$poInj, df$HepCGrp, 2, 3)
overdoseOdds23 <- makeORtable(df, df$poInj, df$overdoseGrp, 2, 3)
df23 <- rbind(genderOdds23, homelessOdds23, ethnicityOdds23, raceOdds23, HepCOdds23, overdoseOdds23)
rownames(df23) <- c('Gender', 'Homeless', 'Ethnicity', 'Race', 'Hep C +', 'Overdose')
df23$alpha <- NULL
write.csv(df23, '/Users/jrteubl/Desktop/heroin_data/PO/OR_Sporadic_vs_Regular.csv', quote = FALSE, na = "0")


inc11 <- nrow(df[df$poInj == 1 & df$incomeGrp == 1, ])
inc12 <- nrow(df[df$poInj == 1 & df$incomeGrp == 2, ])
inc13 <- nrow(df[df$poInj == 1 & df$incomeGrp == 3, ])
inc21 <- nrow(df[df$poInj == 2 & df$incomeGrp == 1, ])
inc22 <- nrow(df[df$poInj == 2 & df$incomeGrp == 2, ])
inc23 <- nrow(df[df$poInj == 2 & df$incomeGrp == 3, ])
inc31 <- nrow(df[df$poInj == 3 & df$incomeGrp == 1, ])
inc32 <- nrow(df[df$poInj == 3 & df$incomeGrp == 2, ])
inc33 <- nrow(df[df$poInj == 3 & df$incomeGrp == 3, ])

injInc1212 <- oddsratioWald.proc(inc11, inc12, inc21, inc22)
injInc1213 <- oddsratioWald.proc(inc11, inc12, inc31, inc32)
injInc1223 <- oddsratioWald.proc(inc21, inc23, inc31, inc32)

injNoneSome <- rbind(injInc1212,injInc1213,injInc1223)
rownames(injNoneSome) <- c('low vs med', 'low vs high', 'med vs high')
injNoneSome$alpha <- NULL
write.csv(injNoneSome, '/Users/jrteubl/Desktop/heroin_data/PO/OR_Income_Never_vs_Sporadic.csv', quote = FALSE, na = "0")


injInc2312 <- oddsratioWald.proc(inc12, inc13, inc22, inc23)
injInc2313 <- oddsratioWald.proc(inc12, inc13, inc32, inc33)
injInc2323 <- oddsratioWald.proc(inc22, inc23, inc32, inc33)

injSomeAlw <- rbind(injInc2312, injInc2313, injInc2323)
rownames(injSomeAlw) <-c('low vs med', 'low vs high', 'med vs high')
injSomeAlw$alpha <- NULL
write.csv(injSomeAlw, '/Users/jrteubl/Desktop/heroin_data/PO/OR_Income_Sporadic_vs_Regular.csv', quote = FALSE, na = "0")


injInc1312 <- oddsratioWald.proc(inc11, inc13, inc21, inc23)
injInc1323 <- oddsratioWald.proc(inc11, inc13, inc31, inc33)
injInc1313 <- oddsratioWald.proc(inc21, inc23, inc31, inc33)
injNoneAlw <- rbind(injInc1312, injInc1323, injInc1313)
rownames(injNoneAlw) <- c('low vs med', 'low vs high', 'med vs high')
injNoneAlw$alpha <- NULL
write.csv(injNoneAlw, '/Users/jrteubl/Desktop/heroin_data/PO/OR_Income_Never_vs_Regular.csv', quote = FALSE, na = "0")

