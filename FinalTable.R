library(gee)
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
# Have you ever injected POs non-medically?
poInj <- rep(0, nrow(dfAll))
poInj[poInjEver == 1]<- 1


#### Demographics #### 

gender <- dfAll$Soc_Note1.Soc_10 # 1 = male, 2 = female
genderGrp <- rep(NA, nrow(dfAll))
genderGrp[gender == 1] <- 0
genderGrp[gender == 2] <- 1


latino <- dfAll$Soc_Note1.Soc_4 #0 = no, 1 = yes
latinoGrp <- rep(NA, nrow(dfAll))
latinoGrp[latino == 0] <- 0
latinoGrp[latino == 1] <- 1


Wht <- dfAll$Soc_Note1.Soc_5.5
race <- rep(NA, nrow(dfAll))
race[Wht == TRUE] <- 1
race[Wht == FALSE] <- 0

homeless <- dfAll$Soc_Note1.Soc_11 #0 = no, 1 = yes
homelessGrp <- rep(NA, nrow(dfAll))
homelessGrp[homeless == 0] <- 0
homelessGrp[homeless == 1] <- 1

gender <- dfAll$Soc_Note1.Soc_10 # 1 = male, 2 = female
genderGrp <- rep(NA, nrow(dfAll))
genderGrp[gender == 1] <- 0
genderGrp[gender == 2] <- 1


latino <- dfAll$Soc_Note1.Soc_4 #0 = no, 1 = yes
latinoGrp <- rep(NA, nrow(dfAll))
latinoGrp[latino == 0] <- 0
latinoGrp[latino == 1] <- 1


Wht <- dfAll$Soc_Note1.Soc_5.5
race <- rep(NA, nrow(dfAll))
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


incomeGrp <- rep(NA, nrow(dfAll))
incomeGrp[income == 1 | income == 2] <- 1
incomeGrp[income == 3 | income == 4] <- 2
incomeGrp[income == 5 | income == 6 | income == 7 | income == 8 | income == 9] <- 3

HepC <- dfAll$hiv_hcv.HepatitisCTestResults
# 0- Negative
# 1- Positive
# 77- NA
HepCGrp <- rep(NA, nrow(dfAll))
HepCGrp[HepC == 0] <- 0
HepCGrp[HepC == 1] <- 1

overdose <- dfAll$Overdose_8
# Have you ever overdosed to the point where you lost consciousness, stopped breathing, or were unresponsive
# 0 = No 
# 1 = Yes
# 77 = Not Applicable
# 88 = Don't know
# 99 = Refuse to Answer
overdoseGrp <- rep(NA, nrow(dfAll))
overdoseGrp[overdose == 1] <- 1
overdoseGrp[overdose == 0] <- 0

date <- as.Date(dfAll$Soc_Note1.Soc_1, "%m/%d/%y")
birthdate <- as.Date(dfAll$Soc_Note1.Soc_3, "%m/%d/%y")
age <- (date - birthdate)/365.25

id <- dfAll$RDS.ID
df1 <- data.frame(id, injGrp, poInj, genderGrp, latinoGrp, race, homelessGrp, incomeGrp, HepCGrp,
                 overdoseGrp, age)
dfSeed <- read.csv("/Users/jrteubl/Desktop/heroin_data/539_seeds.csv", header=T, sep=",")
df <- merge(df1, dfSeed, by="id")
df <- df[df$injGrp == 1, ]
df['c1'] <- NULL
df['c2'] <- NULL
df['c3'] <- NULL
df['X'] <- NULL

df$incomeGrp <- as.factor(df$incomeGrp)

df <- df[order(df$seed), ]

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


ndf <- nrow(df)
dfNev <- df[df$poInj == 0, ]
nNev <- nrow(dfNev)
perNev <- round(100 * nNev/ndf, digits = 2)
npNev <- paste(nNev, ' (', perNev, ')', sep = '')

dfSome <- df[df$poInj == 1, ]
nSome <- nrow(dfSome)
perSome <- round(100 * nSome/ndf, digits = 2)
npSome <- paste(nSome, ' (', perSome, ')', sep = '')


totals <- cbind(npNev, npSome)
colnames(totals) <- c('Never injected POs', 'Occasionally/Regularly injects POs')
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

npHomeless1 <- numPerTable('homelessGrp', 0, 'Never injected POs')
npHomeless2 <- numPerTable('homelessGrp', 1, 'Occasionally/Regularly injects POs')

npHomeless <- cbind(npHomeless1, npHomeless2)
rownames(npHomeless) <- c('Never homeless', 'homeless')

npGender1 <- numPerTable('genderGrp', 0, 'Never injected POs')
npGender2 <- numPerTable('genderGrp', 1, 'Occasionally/Regularly injects POs')

npGender <- cbind(npGender1, npGender2)
rownames(npGender) <- c('Male', 'Female')

npLatino1 <- numPerTable('latinoGrp', 0, 'Never injected POs')
npLatino2 <- numPerTable('latinoGrp', 1, 'Occasionally/Regularly injects POs')
npLatino <- cbind(npLatino1, npLatino2)
rownames(npLatino) <- c('non-Latino', 'Latino')

npRace1 <- numPerTable('race', 0, 'Never injected POs')
npRace2 <- numPerTable('race', 1,'Occasionally/Regularly injects POs')

npRace <- cbind(npRace1, npRace2)
rownames(npRace) <- c('non-White', 'White')

npHepC1 <- numPerTable('HepCGrp', 0, 'Never injected POs')
npHepC2 <- numPerTable('HepCGrp', 1,'Occasionally/Regularly injects POs')

npHepC <- cbind(npHepC1, npHepC2)
rownames(npHepC) <- c('HCV neg', 'HCV pos')

npOD1 <- numPerTable('overdoseGrp', 0, 'Never injected POs')
npOD2 <- numPerTable('overdoseGrp', 1,'Occasionally/Regularly injects POs')

npOD <- cbind(npOD1, npOD2)
rownames(npOD) <- c('Never OD', 'Experienced OD')

df1 <- df[df$poInj == 0, ]
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

df2 <- df[df$poInj == 1, ]
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
colnames(npSome) <- 'Occasionally/Regularly injects POs'


npInc <- cbind(npNev, npSome)
rownames(npInc) <-  c('$0 - $50.000', '$51.000 - $100.000', '$101.000 +')

npTable <- rbind(totals, npGender, npHomeless, npLatino, npRace, npHepC, npOD, npInc)
write.csv(npTable, "/Users/jrteubl/Desktop/heroin_data/PO/NP_NevVsOccReg.csv", quote = FALSE)

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
  
  oframe <- data.frame(LowerCI = ORlo, OR = OR, UpperCI = ORhi, pval = p)
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

genderOdds12 <- makeORtable(df, df$poInj, df$genderGrp, 0, 1)
homelessOdds12 <- makeORtable(df, df$poInj, df$homelessGrp, 0, 1)
ethnicityOdds12 <- makeORtable(df, df$poInj, df$latinoGrp, 0, 1)
raceOdds12 <- makeORtable(df, df$poInj, df$race, 0, 1)
HepCOdds12 <- makeORtable(df, df$poInj, df$HepCGrp, 0, 1)
overdoseOdds12 <- makeORtable(df, df$poInj, df$overdoseGrp, 0, 1)

inc01 <- nrow(df[df$poInj == 0 & df$incomeGrp == 1, ])
inc02 <- nrow(df[df$poInj == 0 & df$incomeGrp == 2, ])
inc03 <- nrow(df[df$poInj == 0 & df$incomeGrp == 3, ])
inc11 <- nrow(df[df$poInj == 1 & df$incomeGrp == 1, ])
inc12 <- nrow(df[df$poInj == 1 & df$incomeGrp == 2, ])
inc13 <- nrow(df[df$poInj == 1 & df$incomeGrp == 3, ])


injInc1212 <- oddsratioWald.proc(inc01, inc02, inc11, inc12)
injInc1213 <- oddsratioWald.proc(inc01, inc03, inc11, inc13)


inj12 <- rbind(injInc1212,injInc1213)
rownames(inj12) <- c('low vs med', 'low vs high')


df12 <- rbind(genderOdds12, homelessOdds12, ethnicityOdds12, raceOdds12, HepCOdds12, overdoseOdds12,
              inj12)
rownames(df12) <- c('Gender', 'Homeless', 'Ethnicity', 'Race', 'Hep C +', 'Overdose',
                    'low vs med', 'low vs high')
df12$alpha <- NULL
write.csv(df12, '/Users/jrteubl/Desktop/heroin_data/PO/OR_Never_vs_OccReg.csv', quote = FALSE)


### GEE models ###

pvalOR <- function(OR, lo, hi, alpha = .05){
  logOR <- log(OR)
  loglo <- log(lo)
  loghi <- log(hi)
  SE <- (loghi - loglo)/(2*1.96)
  z <- abs(logOR/SE)
  p <- exp(-.717*z-.416*z**2)
  df <- data.frame(LowerCI = lo, OR = OR, UpperCI = hi, pval = p)
  return(df)
}
oddsRatio <- function(Mod, ModSum, varName, colName){
  se <- coef(ModSum)[varName, colName]
  OR <- exp(coef(Mod)[varName])
  OR95 <- exp(coef(Mod)[varName] + c(-1,1) * se * qnorm(.0975))
  df <- pvalOR(OR, OR95[2], OR95[1])
  return(df)
}

ORTable <- function(n, Mod, ModSum, colName){
  for (i in 1:length(n)){
    if (i == 1){
      ORdf <- oddsRatio(Mod, ModSum, n[i], colName)
    }else{
      Tempdf <- oddsRatio(Mod, ModSum, n[i], colName)
      ORdf <- rbind(ORdf, Tempdf)
    }
  }
  return(ORdf)
}

### Sig Demographicc + Hep C and PO inj ###
n <- c('HepCGrp', 'latinoGrp', 'homelessGrp', 'age', 'incomeGrp2', 'incomeGrp3')

modGeeExchPOH <- gee(poInj~HepCGrp+latinoGrp+homelessGrp+age+incomeGrp, data = df, id = seed,
                     family = 'gaussian', corstr = "exchangeable")
sumGEPOH <- summary(modGeeExchPOH)

GEtablePOH <- ORTable(n, modGeeExchPOH, sumGEPOH, 'Robust S.E.')
write.csv(GEtablePOH, '/Users/jrteubl/Desktop/heroin_data/PO/AdjORhcv.csv', quote = FALSE)

### Sig Demographics + OD and PO inj ###
n <- c('overdoseGrp', 'race', 'latinoGrp', 'homelessGrp', 'incomeGrp2', 'incomeGrp3')
modGeeExchPOOD <- gee(poInj~overdoseGrp+race+latinoGrp+homelessGrp+incomeGrp, data = df, id = seed,
                      family = 'gaussian', corstr = "exchangeable")
sumGEPOOD <- summary(modGeeExchPOOD)

GEtablePOOD <- ORTable(n, modGeeExchPOOD, sumGEPOOD, 'Robust S.E.')
write.csv(GEtablePOOD, '/Users/jrteubl/Desktop/heroin_data/PO/AdjORod.csv', quote = FALSE)

###Sig Demographics + OD + HCV and PO inj ###
n <- c('HepCGrp','overdoseGrp', 'race', 'homelessGrp', 'incomeGrp2', 'incomeGrp3', 'age')
modGeeExch <- gee(poInj~HepCGrp+overdoseGrp+race+homelessGrp+incomeGrp+age, data = df, id = seed,
                      family = 'gaussian', corstr = "exchangeable")
sumGE <- summary(modGeeExch)

GEtablePO <- ORTable(n, modGeeExch, sumGE, 'Robust S.E.')
write.csv(GEtablePO, '/Users/jrteubl/Desktop/heroin_data/PO/AdjOR.csv', quote = FALSE)