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
poInjEverGrp <- rep(0, nrow(dfAll))
poInjEverGrp[poInjEver == 1]<- 1

poRegInj <- dfAll$LifetimeSU_Note1.LifetimeSU_4
# For how many months have you regualrly injected POs
poRegInjGrp <- rep(0, nrow(dfAll))
poRegInjGrp[poRegInj > 0] <- 1

poInj <- rep(NA, nrow(dfAll))
poInj[poInjEverGrp == 0] <- 1
poInj[poInjEverGrp == 1 & poRegInjGrp == 0] <- 2
poInj[poRegInjGrp == 1] <- 3
# 1: never injected POs
# 2: injected POs but not regularly
# 3: regularly injected POs

poInjYN <- rep(NA, nrow(dfAll))
poInjYN[poInj == 1] <- 0
poInjYN[poInj == 2 | poInj == 3] <- 1

poInjRN <- rep(NA, nrow(dfAll))
poInjRN[poInj == 1] <- 0
poInjRN[poInj == 3] <- 1

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
df <- data.frame(id, injGrp, poInj, poInjRN, poInjYN, genderGrp, latinoGrp, race, homelessGrp, incomeGrp, HepCGrp,
                 overdoseGrp, age)
dfSeed <- read.csv("/Users/jrteubl/Desktop/heroin_data/539_seeds.csv", header=T, sep=",")
df <- merge(df, dfSeed, by="id")
df <- df[df$injGrp == 1, ]
df['c1'] <- NULL
df['c2'] <- NULL
df['c3'] <- NULL
df['X'] <- NULL

df$incomeGrp <- as.factor(df$incomeGrp)

df <- df[order(df$seed), ]
### GEE models ###

pvalOR <- function(OR, lo, hi, alpha = .05){
  logOR <- log(OR)
  loglo <- log(lo)
  loghi <- log(hi)
  SE <- (loghi - loglo)/(2*1.96)
  z <- abs(logOR/SE)
  p <- exp(-.717*z-.416*z**2)
  df <- data.frame(LowerCI = lo, OR = OR, UpperCI = hi, pval = p)
  df
}
oddsRatio <- function(Mod, ModSum, varName, colName){
  se <- coef(ModSum)[varName, colName]
  OR <- exp(coef(Mod)[varName])
  OR95 <- exp(coef(Mod)[varName] + c(-1,1) * se * qnorm(.0975))
  df <- pvalOR(OR, OR95[1], OR95[2])
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

### Demographics ###
n <- c('genderGrp', 'latinoGrp', 'race', 'homelessGrp', 'incomeGrp2', 'incomeGrp3', 'age')
modGeeIndDem <- gee(poInj~genderGrp+latinoGrp+race+homelessGrp+incomeGrp+age, data = df, id = seed,
           family = 'gaussian', corstr = "independence")
sumGIDem <- summary(modGeeIndDem)
GItableDem <- ORTable(n, modGeeIndDem, sumGIDem, 'Robust S.E.')

modGeeExchDem <- gee(poInj~genderGrp+latinoGrp+race+homelessGrp+incomeGrp+age, data = df, id = seed,
              family = 'gaussian', corstr = "exchangeable")
sumGEDem <- summary(modGeeExchDem)
GEtableDem <- ORTable(n, modGeeExchDem, sumGEDem, 'Robust S.E.')
write.csv(GEtableDem, '/Users/jrteubl/Desktop/heroin_data/PO/gee_Demographics.csv', quote = FALSE)

### use binary variable ###
modGEEbi1 <- gee(poInjYN~genderGrp+latinoGrp+race+homelessGrp+incomeGrp+age, data = df, id = seed,
                 family = 'gaussian', corstr = "exchangeable")
sumGEDemBi1 <- summary(modGEEbi1)
GEtableDemBi1 <- ORTable(n, modGEEbi1, sumGEDemBi1, 'Robust S.E.')
write.csv(GEtableDemBi1, '/Users/jrteubl/Desktop/heroin_data/PO/gee_Demographics_YesNo.csv', quote = FALSE)


modGEEbi2 <- gee(poInjRN~genderGrp+latinoGrp+race+homelessGrp+incomeGrp+age, data = df, id = seed,
                 family = 'gaussian', corstr = "exchangeable")
sumGEDemBi2 <- summary(modGEEbi2)
GEtableDemBi2 <- ORTable(n, modGEEbi2, sumGEDemBi2, 'Robust S.E.')
write.csv(GEtableDemBi2, '/Users/jrteubl/Desktop/heroin_data/PO/gee_Demographics_RegNev.csv', quote = FALSE)




### Sig Demographics and Hep C ###

n <- c( 'genderGrp', 'latinoGrp', 'race', 'homelessGrp', 'incomeGrp2', 'incomeGrp3', 'age')
modGeeIndH <- gee(HepCGrp~genderGrp+latinoGrp+race+homelessGrp+incomeGrp+age, data = df, id = seed,
                 family = 'gaussian', corstr = "independence")
sumGIH <- summary(modGeeIndH)

GItable <- ORTable(n, modGeeIndH, sumGIH, 'Robust S.E.')

modGeeExchH <- gee(HepCGrp~genderGrp+latinoGrp+race+homelessGrp+incomeGrp+age, data = df, id = seed,
                  family = 'gaussian', corstr = "exchangeable")
sumGEH <- summary(modGeeExchH)

GEtableH <- ORTable(n, modGeeExchH, sumGEH, 'Robust S.E.')
write.csv(GEtableH, '/Users/jrteubl/Desktop/heroin_data/PO/gee_HepC.csv', quote = FALSE)

### Sig Demoigraphics and OD ###

n <- c( 'genderGrp', 'latinoGrp', 'race', 'homelessGrp', 'incomeGrp2', 'incomeGrp3', 'age')
modGeeIndOD <- gee(overdoseGrp~genderGrp+latinoGrp+race+homelessGrp+incomeGrp+age, data = df, id = seed,
                  family = 'gaussian', corstr = "independence")
sumGIOD <- summary(modGeeIndOD)

GItableOD <- ORTable(n, modGeeIndOD, sumGIOD, 'Robust S.E.')

modGeeExchOD <- gee(overdoseGrp~genderGrp+latinoGrp+race+homelessGrp+incomeGrp+age, data = df, id = seed,
                   family = 'gaussian', corstr = "exchangeable")
sumGEOD <- summary(modGeeExchOD)

GEtableOD <- ORTable(n, modGeeExchOD, sumGEOD, 'Robust S.E.')
write.csv(GEtableOD, '/Users/jrteubl/Desktop/heroin_data/PO/gee_OD.csv', quote = FALSE)

### Sig Demographicc + Hep C and PO inj ###
n <- c('HepCGrp', 'latinoGrp', 'homelessGrp', 'age', 'incomeGrp2', 'incomeGrp3')
modGeeIndPOH <- gee(poInj~HepCGrp+latinoGrp+homelessGrp+age+incomeGrp, data = df, id = seed,
                   family = 'gaussian', corstr = "independence")
sumGIPOH <- summary(modGeeIndPOH)

GItablePOH <- ORTable(n, modGeeIndPOH, sumGIPOH, 'Robust S.E.')

modGeeExchPOH <- gee(poInj~HepCGrp+latinoGrp+homelessGrp+age+incomeGrp, data = df, id = seed,
                    family = 'gaussian', corstr = "exchangeable")
sumGEPOH <- summary(modGeeExchPOH)

GEtablePOH <- ORTable(n, modGeeExchPOH, sumGEPOH, 'Robust S.E.')
write.csv(GEtablePOH, '/Users/jrteubl/Desktop/heroin_data/PO/gee_poInj_w_HepC.csv', quote = FALSE)


### Binary variable ###
modGeeExchPOH_bi1 <- gee(poInjYN~HepCGrp+latinoGrp+homelessGrp+age+incomeGrp, data = df, id = seed,
                     family = 'gaussian', corstr = "exchangeable")
sumGEPOH_bi1 <- summary(modGeeExchPOH_bi1)
GEtablePOH_bi1 <- ORTable(n, modGeeExchPOH_bi1, sumGEPOH_bi1, 'Robust S.E.')
write.csv(GEtablePOH_bi1, '/Users/jrteubl/Desktop/heroin_data/PO/gee_poInj_w_HepC_YesNo.csv', quote = FALSE)

modGeeExchPOH_bi2 <- gee(poInjRN~HepCGrp+latinoGrp+homelessGrp+age+incomeGrp, data = df, id = seed,
                         family = 'gaussian', corstr = "exchangeable")
sumGEPOH_bi2 <- summary(modGeeExchPOH_bi2)
GEtablePOH_bi2 <- ORTable(n, modGeeExchPOH_bi2, sumGEPOH_bi2, 'Robust S.E.')
write.csv(GEtablePOH_bi2, '/Users/jrteubl/Desktop/heroin_data/PO/gee_poInj_w_HepC_RegNev.csv', quote = FALSE)



### Sig Demographics + OD and PO inj ###
n <- c('overdoseGrp', 'race', 'latinoGrp', 'homelessGrp', 'incomeGrp2', 'incomeGrp3')
modGeeIndPOOD <- gee(poInj~overdoseGrp+race+latinoGrp+homelessGrp+incomeGrp, data = df, id = seed,
                    family = 'gaussian', corstr = "independence")
sumGIPOOD <- summary(modGeeIndPOOD)

GItablePOOD <- ORTable(n, modGeeIndPOOD, sumGIPOOD, 'Robust S.E.')

modGeeExchPOOD <- gee(poInj~overdoseGrp+race+latinoGrp+homelessGrp+incomeGrp, data = df, id = seed,
                     family = 'gaussian', corstr = "exchangeable")
sumGEPOOD <- summary(modGeeExchPOOD)

GEtablePOOD <- ORTable(n, modGeeExchPOOD, sumGEPOOD, 'Robust S.E.')
write.csv(GEtablePOOD, '/Users/jrteubl/Desktop/heroin_data/PO/gee_poInj_w_OD.csv', quote = FALSE)

#### Binary Variable ###

modGeeExchPOOD_bi1 <- gee(poInjYN~overdoseGrp+race+latinoGrp+homelessGrp+incomeGrp, data = df, id = seed,
                      family = 'gaussian', corstr = "exchangeable")
sumGEPOOD_bi1 <- summary(modGeeExchPOOD_bi1)

GEtablePOOD_bi1 <- ORTable(n, modGeeExchPOOD_bi1, sumGEPOOD_bi1, 'Robust S.E.')
write.csv(GEtablePOOD_bi1, '/Users/jrteubl/Desktop/heroin_data/PO/gee_poInj_w_OD_YesNo.csv', quote = FALSE)

modGeeExchPOOD_bi2 <- gee(poInjRN~overdoseGrp+race+latinoGrp+homelessGrp+incomeGrp, data = df, id = seed,
                          family = 'gaussian', corstr = "exchangeable")
sumGEPOOD_bi2 <- summary(modGeeExchPOOD_bi2)

GEtablePOOD_bi2 <- ORTable(n, modGeeExchPOOD_bi2, sumGEPOOD_bi2, 'Robust S.E.')
write.csv(GEtablePOOD_bi2, '/Users/jrteubl/Desktop/heroin_data/PO/gee_poInj_w_OD_RegNev.csv', quote = FALSE)







### REMOVE HOMELESSNESS AS DEMOGRAPHIC ###
### Demographics ###
n <- c('genderGrp', 'latinoGrp', 'race', 'incomeGrp2', 'incomeGrp3', 'age')
modGeeIndDem <- gee(poInj~genderGrp+latinoGrp+race+incomeGrp+age, data = df, id = seed,
                    family = 'gaussian', corstr = "independence")
sumGIDem <- summary(modGeeIndDem)
GItableDem <- ORTable(n, modGeeIndDem, sumGIDem, 'Robust S.E.')

modGeeExchDem <- gee(poInj~genderGrp+latinoGrp+race+incomeGrp+age, data = df, id = seed,
                     family = 'gaussian', corstr = "exchangeable")
sumGEDem <- summary(modGeeExchDem)
GEtableDem <- ORTable(n, modGeeExchDem, sumGEDem, 'Robust S.E.')
write.csv(GEtableDem, '/Users/jrteubl/Desktop/heroin_data/PO/gee_Demographics_NH.csv', quote = FALSE)

### use binary variable ###
modGEEbi1 <- gee(poInjYN~genderGrp+latinoGrp+race+incomeGrp+age, data = df, id = seed,
                 family = 'gaussian', corstr = "exchangeable")
sumGEDemBi1 <- summary(modGEEbi1)
GEtableDemBi1 <- ORTable(n, modGEEbi1, sumGEDemBi1, 'Robust S.E.')
write.csv(GEtableDemBi1, '/Users/jrteubl/Desktop/heroin_data/PO/gee_Demographics_YesNo_NH.csv', quote = FALSE)


modGEEbi2 <- gee(poInjRN~genderGrp+latinoGrp+race+incomeGrp+age, data = df, id = seed,
                 family = 'gaussian', corstr = "exchangeable")
sumGEDemBi2 <- summary(modGEEbi2)
GEtableDemBi2 <- ORTable(n, modGEEbi2, sumGEDemBi2, 'Robust S.E.')
write.csv(GEtableDemBi2, '/Users/jrteubl/Desktop/heroin_data/PO/gee_Demographics_RegNev_NH.csv', quote = FALSE)




### Sig Demographics and Hep C ###

n <- c('latinoGrp', 'race', 'incomeGrp2', 'incomeGrp3', 'age')
modGeeIndH <- gee(HepCGrp~latinoGrp+race+incomeGrp+age, data = df, id = seed,
                  family = 'gaussian', corstr = "independence")
sumGIH <- summary(modGeeIndH)

GItable <- ORTable(n, modGeeIndH, sumGIH, 'Robust S.E.')

modGeeExchH <- gee(HepCGrp~latinoGrp+race+incomeGrp+age, data = df, id = seed,
                   family = 'gaussian', corstr = "exchangeable")
sumGEH <- summary(modGeeExchH)

GEtableH <- ORTable(n, modGeeExchH, sumGEH, 'Robust S.E.')
write.csv(GEtableH, '/Users/jrteubl/Desktop/heroin_data/PO/gee_HepC_NH.csv', quote = FALSE)

### Sig Demoigraphics and OD ###

n <- c('latinoGrp', 'race', 'incomeGrp2', 'incomeGrp3', 'age')
modGeeIndOD <- gee(overdoseGrp~latinoGrp+race+incomeGrp+age, data = df, id = seed,
                   family = 'gaussian', corstr = "independence")
sumGIOD <- summary(modGeeIndOD)

GItableOD <- ORTable(n, modGeeIndOD, sumGIOD, 'Robust S.E.')

modGeeExchOD <- gee(overdoseGrp~latinoGrp+race+incomeGrp+age, data = df, id = seed,
                    family = 'gaussian', corstr = "exchangeable")
sumGEOD <- summary(modGeeExchOD)

GEtableOD <- ORTable(n, modGeeExchOD, sumGEOD, 'Robust S.E.')
write.csv(GEtableOD, '/Users/jrteubl/Desktop/heroin_data/PO/gee_OD_NH.csv', quote = FALSE)

### Sig Demographicc + Hep C and PO inj ###
n <- c('HepCGrp', 'latinoGrp', 'age', 'incomeGrp2', 'incomeGrp3')
modGeeIndPOH <- gee(poInj~HepCGrp+latinoGrp+age+incomeGrp, data = df, id = seed,
                    family = 'gaussian', corstr = "independence")
sumGIPOH <- summary(modGeeIndPOH)

GItablePOH <- ORTable(n, modGeeIndPOH, sumGIPOH, 'Robust S.E.')

modGeeExchPOH <- gee(poInj~HepCGrp+latinoGrp+age+incomeGrp, data = df, id = seed,
                     family = 'gaussian', corstr = "exchangeable")
sumGEPOH <- summary(modGeeExchPOH)

GEtablePOH <- ORTable(n, modGeeExchPOH, sumGEPOH, 'Robust S.E.')
write.csv(GEtablePOH, '/Users/jrteubl/Desktop/heroin_data/PO/gee_poInj_w_HepC_NH.csv', quote = FALSE)


### Binary variable ###
modGeeExchPOH_bi1 <- gee(poInjYN~HepCGrp+latinoGrp+age+incomeGrp, data = df, id = seed,
                         family = 'gaussian', corstr = "exchangeable")
sumGEPOH_bi1 <- summary(modGeeExchPOH_bi1)
GEtablePOH_bi1 <- ORTable(n, modGeeExchPOH_bi1, sumGEPOH_bi1, 'Robust S.E.')
write.csv(GEtablePOH_bi1, '/Users/jrteubl/Desktop/heroin_data/PO/gee_poInj_w_HepC_YesNo_NH.csv', quote = FALSE)

modGeeExchPOH_bi2 <- gee(poInjRN~HepCGrp+latinoGrp+age+incomeGrp, data = df, id = seed,
                         family = 'gaussian', corstr = "exchangeable")
sumGEPOH_bi2 <- summary(modGeeExchPOH_bi2)
GEtablePOH_bi2 <- ORTable(n, modGeeExchPOH_bi2, sumGEPOH_bi2, 'Robust S.E.')
write.csv(GEtablePOH_bi2, '/Users/jrteubl/Desktop/heroin_data/PO/gee_poInj_w_HepC_RegNev_NH.csv', quote = FALSE)



### Sig Demographics + OD and PO inj ###
n <- c('overdoseGrp', 'race', 'latinoGrp', 'incomeGrp2', 'incomeGrp3')
modGeeIndPOOD <- gee(poInj~overdoseGrp+race+latinoGrp+incomeGrp, data = df, id = seed,
                     family = 'gaussian', corstr = "independence")
sumGIPOOD <- summary(modGeeIndPOOD)

GItablePOOD <- ORTable(n, modGeeIndPOOD, sumGIPOOD, 'Robust S.E.')

modGeeExchPOOD <- gee(poInj~overdoseGrp+race+latinoGrp+incomeGrp, data = df, id = seed,
                      family = 'gaussian', corstr = "exchangeable")
sumGEPOOD <- summary(modGeeExchPOOD)

GEtablePOOD <- ORTable(n, modGeeExchPOOD, sumGEPOOD, 'Robust S.E.')
write.csv(GEtablePOOD, '/Users/jrteubl/Desktop/heroin_data/PO/gee_poInj_w_OD_NH.csv', quote = FALSE)

#### Binary Variable ###

modGeeExchPOOD_bi1 <- gee(poInjYN~overdoseGrp+race+latinoGrp+incomeGrp, data = df, id = seed,
                          family = 'gaussian', corstr = "exchangeable")
sumGEPOOD_bi1 <- summary(modGeeExchPOOD_bi1)

GEtablePOOD_bi1 <- ORTable(n, modGeeExchPOOD_bi1, sumGEPOOD_bi1, 'Robust S.E.')
write.csv(GEtablePOOD_bi1, '/Users/jrteubl/Desktop/heroin_data/PO/gee_poInj_w_OD_YesNo_NH.csv', quote = FALSE)

modGeeExchPOOD_bi2 <- gee(poInjRN~overdoseGrp+race+latinoGrp+incomeGrp, data = df, id = seed,
                          family = 'gaussian', corstr = "exchangeable")
sumGEPOOD_bi2 <- summary(modGeeExchPOOD_bi2)

GEtablePOOD_bi2 <- ORTable(n, modGeeExchPOOD_bi2, sumGEPOOD_bi2, 'Robust S.E.')
write.csv(GEtablePOOD_bi2, '/Users/jrteubl/Desktop/heroin_data/PO/gee_poInj_w_OD_RegNev_NH.csv', quote = FALSE)

