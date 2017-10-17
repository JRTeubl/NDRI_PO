library(RDS)

dfAll <- read.csv("/Users/jrteubl/Desktop/heroin_data/2016-06-29-database-merge-umass-N539.csv", header=T, sep=",")

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
homeless <- dfAll$Soc_Note1.Soc_11 #0 = no, 1 = yes

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

### Percentages of demographics ###
meanAge <- mean(age, na.rm = TRUE)
stdAge <- sd(age, na.rm = TRUE)

ageNP <- paste(round(meanAge, digits = 1), ' (+/-', round(stdAge, digits = 1), ')', sep = '')

gen <- genderGrp[!is.na(genderGrp)]
genderFnum <- length(gen[gen == 1])
genderFper <- 100*(length(gen[gen == 1])/length(gen))
genderFnp <- paste(genderFnum, ' (', round(genderFper, digits = 2), ')', sep = '')
genderMnum <- length(gen[gen == 0])
genderMPer <- 100*(length(gen[gen == 0])/length(gen))
genderMnp <- paste(genderMnum, ' (', round(genderMPer, digits = 2), ')', sep = '')

homelessNum <- length(homelessGrp[homelessGrp == 1])
homelessPer <- 100*(length(homelessGrp[homelessGrp == 1])/length(homelessGrp))
homelessNP <- paste(homelessNum, ' (', round(homelessPer, digits = 2), ')', sep = '')

lat <- latinoGrp[!is.na(latinoGrp)]
latinoNum <- length(lat[lat == 1])
latinoPer <- 100*(length(lat[lat == 1])/length(lat))
latinoNP <- paste(latinoNum, ' (', round(latinoPer, digits = 2), ')', sep = '')

raceNum <- length(race[race == 1])
racePer <- 100*(length(race[race == 1])/length(race))
raceNP <- paste(raceNum, ' (', round(racePer, digits = 2), ')', sep = '')

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

Hep <- HepCGrp[!is.na(HepCGrp)]
hepCnum <- length(Hep[Hep == 1])
hepcPer <- 100*(length(Hep[HepCGrp == 1])/length(Hep))
hepCNP <- paste(hepCnum, ' (', round(hepcPer, digits = 2), ')', sep = '')

od <- overdoseGrp[!is.na(overdoseGrp)]
odNum <- length(od[od == 1])
odPer <- 100*(length(od[od == 1])/length(od))
odNP <- paste(odNum, ' (', round(odPer, digits = 2), ')', sep = '')


nps <- rbind(ageNP, genderMnp, genderFnp, homelessNP, latinoNP, raceNP, income1NP, income2NP, income3NP, hepCNP, odNP)
colnames(nps) <- 'Sample n(%)'
#### Population Estimates ####

mySS <- function(data1, data2, data3, data4, data5, var){
  SS1 <- RDS.SS.estimates(data1, outcome.variable=var, N = 15000,
                          number.ss.samples.per.iteration = 500,
                          number.ss.iterations = 5, hajek = TRUE,
                          empir.lik = TRUE)$interval
  
  SS2 <- RDS.SS.estimates(data2, outcome.variable=var, N = 15000,
                          number.ss.samples.per.iteration = 500,
                          number.ss.iterations = 5, hajek = TRUE,
                          empir.lik = TRUE)$interval
  
  SS3 <- RDS.SS.estimates(data3, outcome.variable=var, N = 15000,
                          number.ss.samples.per.iteration = 500,
                          number.ss.iterations = 5, hajek = TRUE,
                          empir.lik = TRUE)$interval
  
  SS4 <- RDS.SS.estimates(data4, outcome.variable=var, N = 15000,
                          number.ss.samples.per.iteration = 500,
                          number.ss.iterations = 5, hajek = TRUE,
                          empir.lik = TRUE)$interval
  
  SS5 <- RDS.SS.estimates(data5, outcome.variable=var, N = 15000,
                          number.ss.samples.per.iteration = 500,
                          number.ss.iterations = 5, hajek = TRUE,
                          empir.lik = TRUE)$interval
  var <- rbind(SS1,SS2,SS3,SS4,SS5)
  return(var)
}

MI.point.var <- function(point, var, n){
  point.MI <- mean(point)
  W <- mean(var^2)
  B <- (1/(n-1))*sum((point-point.MI)^2)
  var.MI <- W+(1+1/n)*B
  sd.MI <- sqrt(var.MI)
  return(list(point.MI=point.MI, var.MI= var.MI, sd.MI=sd.MI))
}

dfImp <- read.csv("/Users/jrteubl/Desktop/heroin_data/2016-06-29-database-merge-umass-N539-imputation.csv", header=T, sep=",")
dfRec <- read.csv("/Users/jrteubl/Desktop/heroin_data/recIds_539.csv", header = T, sep=",")


ID <- as.character(dfRec$ID)
rec.id <- as.character(dfRec$rec.id)
data.imp1.Lastquestion <- dfImp$data.imp1
data.imp2.Lastquestion <- dfImp$data.imp2
data.imp3.Lastquestion <- dfImp$data.imp3
data.imp4.Lastquestion <- dfImp$data.imp4
data.imp5.Lastquestion <- dfImp$data.imp5

df <- as.data.frame(cbind(ID, rec.id, homelessGrp, genderGrp, latinoGrp, race, incomeGrp, HepCGrp, overdoseGrp, 
                 data.imp1.Lastquestion,data.imp2.Lastquestion, data.imp3.Lastquestion, data.imp4.Lastquestion,
                 data.imp5.Lastquestion))

rds.df1 <-as.rds.data.frame(df=df,id="ID",
                            recruiter.id="rec.id",
                            network.size="data.imp1.Lastquestion",
                            population.size=15000)
rds.df2 <-as.rds.data.frame(df=df,id="ID",
                            recruiter.id="rec.id",
                            network.size="data.imp2.Lastquestion",
                            population.size=15000)
rds.df3 <-as.rds.data.frame(df=df,id="ID",
                            recruiter.id="rec.id",
                            network.size="data.imp3.Lastquestion",
                            population.size=15000)
rds.df4 <-as.rds.data.frame(df=df,id="ID",
                            recruiter.id="rec.id",
                            network.size="data.imp4.Lastquestion",
                            population.size=15000)
rds.df5 <-as.rds.data.frame(df=df,id="ID",
                            recruiter.id="rec.id",
                            network.size="data.imp5.Lastquestion",
                            population.size=15000)


popEstHL <- mySS(rds.df1, rds.df2, rds.df3, rds.df4, rds.df5,"homelessGrp")
popEstGen <- mySS(rds.df1, rds.df2, rds.df3, rds.df4, rds.df5,"genderGrp")
popEstLat <- mySS(rds.df1, rds.df2, rds.df3, rds.df4, rds.df5,"latinoGrp")
popEstRace <- mySS(rds.df1, rds.df2, rds.df3, rds.df4, rds.df5,"race")
popEstInc <- mySS(rds.df1, rds.df2, rds.df3, rds.df4, rds.df5,"incomeGrp")
popEstHep <- mySS(rds.df1, rds.df2, rds.df3, rds.df4, rds.df5,"HepCGrp")
popEstOD <- mySS(rds.df1, rds.df2, rds.df3, rds.df4, rds.df5,"overdoseGrp")

PEhl <-MI.point.var(popEstHL[names(popEstHL[,1])=="1",1], popEstHL[names(popEstHL[,5])=="1",5],5)
PEhl <- paste(round(PEhl$point.MI, digits = 3)*100, ' (+/-',  round(PEhl$var.MI, digits = 3)*100, ')', sep = '')

PEgenM <-MI.point.var(popEstGen[names(popEstGen[,1])=="0",1], popEstGen[names(popEstGen[,5])=="0",5],5)
PEgenM <- paste(round(PEgenM$point.MI, digits = 3)*100, ' (+/-', round(PEgenM$var.MI, digits = 3)*100, ')', sep = '')
PEgenF <-MI.point.var(popEstGen[names(popEstGen[,1])=="1",1], popEstGen[names(popEstGen[,5])=="1",5],5)
PEgenF <- paste(round(PEgenF$point.MI, digits = 3)*100, ' (+/-', round(PEgenF$var.MI, digits = 3)*100, ')', sep = '')

PElat <-MI.point.var(popEstLat[names(popEstLat[,1])=="1",1], popEstLat[names(popEstLat[,5])=="1",5],5)
PElat <- paste(round(PElat$point.MI, digits = 3)*100, ' (+/-', round(PElat$var.MI, digits = 3)*100, ')', sep = '')

PErace <-MI.point.var(popEstRace[names(popEstRace[,1])=="1",1], popEstRace[names(popEstRace[,5])=="1",5],5)
PErace <- paste(round(PErace$point.MI, digits = 3)*100, ' (+/-', round(PErace$var.MI, digits = 3)*100, ')', sep = '')

PEinc1 <-MI.point.var(popEstInc[names(popEstInc[,1])=="1",1], popEstInc[names(popEstInc[,5])=="1",5],5)
PEinc1 <- paste(round(PEinc1$point.MI, digits = 3)*100, ' (+/-', round(PEinc1$var.MI, digits = 3)*100, ')', sep = '')

PEinc2 <-MI.point.var(popEstInc[names(popEstInc[,1])=="2",1], popEstInc[names(popEstInc[,5])=="2",5],5)
PEinc2 <- paste(round(PEinc2$point.MI, digits = 3)*100, ' (+/-', round(PEinc2$var.MI, digits = 3)*100, ')', sep = '')

PEinc3 <-MI.point.var(popEstInc[names(popEstInc[,1])=="3",1], popEstInc[names(popEstInc[,5])=="3",5],5)
PEinc3 <- paste(round(PEinc3$point.MI, digits = 3)*100, ' (+/-', round(PEinc3$var.MI, digits = 3)*100, ')', sep = '')

PEhep <-MI.point.var(popEstHep[names(popEstHep[,1])=="1",1], popEstHep[names(popEstHep[,5])=="1",5],5)
PEhep <- paste(round(PEhep$point.MI, digits = 3)*100, ' (+/-', round(PEhep$var.MI, digits = 3)*100, ')', sep = '')

PEod <-MI.point.var(popEstOD[names(popEstOD[,1])=="1",1], popEstOD[names(popEstOD[,5])=="1",5],5)
PEod <- paste(round(PEod$point.MI, digits = 3)*100, ' (+/-', round(PEod$var.MI, digits = 3)*100, ')', sep = '')


PEs <- rbind('-', PEgenM, PEgenF, PEhl, PElat, PErace, PEinc1, PEinc2, PEinc3, PEhep, PEod)
colnames(PEs) <- 'Pop Estimates mean(std)'
NPEtable <- cbind(nps, PEs)
rownames(NPEtable) <- c('Age', 'Male', 'Female', 'Homeless', 'Latino', 'White', '$0 - $50.000', '$51.000 - $100.000', '$101.000 +',
                   'Hep C +', 'Overdose')

write.csv(NPEtable, "/Users/jrteubl/Desktop/heroin_data/PO/populationEstimates.csv", quote = FALSE, na = "0")


####### Number and Percentages ##########
df <- data.frame(poInj, age, homelessGrp, genderGrp, latinoGrp, race, incomeGrp, HepCGrp, overdoseGrp)
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
write.csv(npTable, "/Users/jrteubl/Desktop/heroin_data/PO/numPercent.csv", quote = FALSE, na = "0")

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
write.csv(csqTable, "/Users/jrteubl/Desktop/heroin_data/PO/chisq.csv", quote = FALSE, na = "0")

##### PO injectors ONLY ########

poInjEverNum <- length(poInjEverGrp[poInjEverGrp == 1])
poInjEverPer <- 100*(length(poInjEverGrp[poInjEverGrp == 1])/length(poInjEverGrp))

poInjRegMo <- dfAll$LifetimeSU_Note1.LifetimeSU_4
poInjLast30 <- dfAll$DaysSU_Note1.DaysSU_46
poInjLast30[poInjLast30 == 77] <- NaN
poUseFirstAge <- dfAll$RxInitiation_Note1.RxInitiation_1
poInjFirstAge <- dfAll$Benchmark_Note1.Benchmark_20
heroinInjFirstAge <- dfAll$Benchmark_Note1.Benchmark_24


dfPOinj <- data.frame(poInjEverGrp, poInjRegMo, poInjLast30, poUseFirstAge, poInjFirstAge, heroinInjFirstAge)
dfPOinj <- dfPOinj[dfPOinj$poInjEverGrp == 1, ]


firstPOuseMean <- round(mean(dfPOinj$poUseFirstAge, na.rm = TRUE), digits = 2)
firstPOuseSD <- round(sd(dfPOinj$poUseFirstAge, na.rm = TRUE), digits = 2)
firstPOUseMSD <- paste(firstPOuseMean, ' (+/-', firstPOuseSD, ')', sep = '')

firstPOinjMean <- round(mean(dfPOinj$poInjFirstAge, na.rm = TRUE), digits = 2)
firstPOinjSD <- round(sd(dfPOinj$poInjFirstAge, na.rm = TRUE), digits = 2)
firstPOInjMSD <- paste(firstPOinjMean, ' (+/-', firstPOinjSD, ')', sep = '')

firstHeroinInjMean <- round(mean(dfPOinj$heroinInjFirstAge, na.rm = TRUE), digits = 2)
firstHeroinInjSD <- round(sd(dfPOinj$heroinInjFirstAge, na.rm = TRUE), digits = 2)
firstHeroinInjMSD <- paste(firstHeroinInjMean, ' (+/-', firstHeroinInjSD, ')', sep = '')

l30POInjMean <- round(mean(dfPOinj$poInjLast30, na.rm = TRUE), digits = 2)
l30POInjSD <- round(sd(dfPOinj$poInjLast30, na.rm = TRUE), digits = 2)
l30POInjMSD <- paste(l30POInjMean, ' (+/-', l30POInjSD, ')', sep = '')

regPOInjMean <- round(mean(dfPOinj$poInjRegMo, na.rm = TRUE), digits = 2)
regPOInjSD <- round(sd(dfPOinj$poInjRegMo, na.rm = TRUE), digits = 2)
regPOInjMSD <- paste(regPOInjMean, ' (+/-', regPOInjSD, ')', sep = '')

## First PO use
# <13, 13-15, 16-18, >18
firstPOUse <- dfPOinj$poUseFirstAge
firstPOUse <- na.omit(firstPOUse)
total <- length(firstPOUse)
firstPOUseNum1 <- length(firstPOUse[firstPOUse < 13])
firstPOUsePer1 <- 100*(firstPOUseNum1 /total)
poUse1np <- paste(firstPOUseNum1, ' (', round(firstPOUsePer1, digits = 2), ')', sep = '')

firstPOUseNum2 <- length(firstPOUse[firstPOUse >= 13 & firstPOUse <= 15])
firstPOUsePer2 <- 100*(firstPOUseNum2/total)
poUse2np <- paste(firstPOUseNum2, ' (', round(firstPOUsePer2, digits = 2), ')', sep = '')

firstPOUseNum3 <- length(firstPOUse[firstPOUse >15 & firstPOUse <= 18])
firstPOUsePer3 <- 100*(firstPOUseNum3/total)
poUse3np <- paste(firstPOUseNum3, ' (', round(firstPOUsePer3, digits = 2), ')', sep = '')

firstPOUseNum4 <- length(firstPOUse[firstPOUse > 18])
firstPOUsePer4 <- 100*(firstPOUseNum4/total)
poUse4np <- paste(firstPOUseNum4, ' (', round(firstPOUsePer4, digits = 2), ')', sep = '')

firstPOUseTable <- rbind(total, firstPOUseMSD, poUse1np, poUse2np, poUse3np, poUse4np)
colnames(firstPOUseTable) <- 'n (%) years'
rownames(firstPOUseTable) <- c('tota', 'mean(std)', '<13', '13-15', '16-18', '>18')

write.csv(firstPOUseTable, "/Users/jrteubl/Desktop/heroin_data/PO/firstPOUse.csv", quote = FALSE, na = "0")

## First PO inj
# <18, 18-20, 21-23, >23
firstPOInj <- dfPOinj$poInjFirstAge
firstPOInj <- na.omit(firstPOInj)
total = length(firstPOInj)

firstPOInjNum1 <- length(firstPOInj[firstPOInj < 18])
firstPOInjPer1 <- (firstPOInjNum1/total)*100
poInj1np <- paste(firstPOInjNum1, ' (', round(firstPOInjPer1, digits = 2), ')', sep = '')

firstPOInjNum2 <- length(firstPOInj[firstPOInj >=18 & firstPOInj <=20])
firstPOInjPer2 <- (firstPOInjNum2/total)*100
poInj2np <- paste(firstPOInjNum2, ' (', round(firstPOInjPer2, digits = 2), ')', sep = '')

firstPOInjNum3 <- length(firstPOInj[firstPOInj >20 &firstPOInj <=23])
firstPOInjPer3 <- (firstPOInjNum3/total)*100
poInj3np <- paste(firstPOInjNum3, ' (', round(firstPOInjPer3, digits = 2), ')', sep = '')

firstPOInjNum4 <- length(firstPOInj[firstPOInj > 23])
firstPOInjPer4 <- (firstPOInjNum4/total)*100
poInj4np <- paste(firstPOInjNum4, ' (', round(firstPOInjPer4, digits = 2), ')', sep = '')

firstPOInjTable <- rbind(total, firstPOInjMSD, poInj1np, poInj2np, poInj3np, poInj4np)
colnames(firstPOInjTable) <- 'n (%) years'
rownames(firstPOInjTable) <- c('total', 'mean(std)','<18', '18-20', '21-23', '>23')

write.csv(firstPOInjTable, "/Users/jrteubl/Desktop/heroin_data/PO/firstPOInj.csv", quote = FALSE, na = "0")

## First heroin inj
# <18, 18-20, 21-23, >23
firstHeroinInj <- dfPOinj$heroinInjFirstAge
firstHeroinInj <- na.omit(firstHeroinInj)
total <- length(firstHeroinInj)

firstHerInjNum1 <- length(firstHeroinInj[firstHeroinInj < 18])
firstHerInjPer1 <- 100*(firstHerInjNum1/total)
herInj1np <- paste(firstHerInjNum1, ' (', round(firstHerInjPer1, digits = 2), ')', sep = '')

firstHerInjNum2 <- length(firstHeroinInj[firstHeroinInj >=18 & firstHeroinInj <=20])
firstHerInjPer2 <- 100*(firstHerInjNum2/total)
herInj2np <- paste(firstHerInjNum2, ' (', round(firstHerInjPer2, digits = 2), ')', sep = '')

firstHerInjNum3 <- length(firstHeroinInj[firstHeroinInj >20 & firstHeroinInj <=23])
firstHerInjPer3 <- 100*(firstHerInjNum3/total)
herInj3np <- paste(firstHerInjNum3, ' (', round(firstHerInjPer3, digits = 2), ')', sep = '')

firstHerInjNum4 <- length(firstHeroinInj[firstHeroinInj > 23])
firstHerInjPer4 <- 100*(firstHerInjNum4/total)
herInj4np <- paste(firstHerInjNum4, ' (', round(firstHerInjPer4, digits = 2), ')', sep = '')

firstHeroinInjTable <- rbind(total, firstHeroinInjMSD, herInj1np, herInj2np, herInj3np, herInj4np)
colnames(firstHeroinInjTable) <- 'n (%) years'
rownames(firstHeroinInjTable) <- c('total','mean(std)', '<18', '18-20', '21-23', '>23')

write.csv(firstHeroinInjTable, "/Users/jrteubl/Desktop/heroin_data/PO/firstHeroinInj.csv", quote = FALSE, na = "0")

### compare heroin and po injection ages ###

dfInjAges <- dfPOinj[c('poInjFirstAge', 'heroinInjFirstAge')]
dfInjAges <- na.omit(dfInjAges)
n <- nrow(dfInjAges)

heroinFirst <- dfInjAges[dfInjAges$heroinInjFirstAge < (dfInjAges$poInjFirstAge - 1.0), ]
hfNum <- nrow(heroinFirst)
hfPer <- 100*(hfNum/n)
hf <- paste(hfNum, ' (', round(hfPer, digits = 2), ')')
poFirst <- dfInjAges[dfInjAges$poInjFirstAge < (dfInjAges$heroinInjFirstAge - 1.0), ]
pfNum <- nrow(poFirst)
pfPer <- 100*(pfNum/n)
pf <- paste(pfNum, ' (', round(pfPer, digits = 2), ')')
withinOneYear <- dfInjAges[abs(dfInjAges$poInjFirstAge - dfInjAges$heroinInjFirstAge) < 1.0, ]
woNum <- nrow(withinOneYear)
woPer <- 100*(woNum/n)
wof <- paste(woNum, ' (', round(woPer, digits = 2), ')')

ageComp <- rbind(firstHeroinInjMSD, firstPOInjMSD, hf, pf, wof)
colnames(ageComp) <- 'n (%)'
rownames(ageComp) <- c('Age first injected heroin;mean(std)', 'Age first injected PO;mean(std)',
                       'Injected heroin before POs', 'Injected POs before heroin', 
                       'Injected heroin and POs within a year of each other')

write.csv(ageComp, "/Users/jrteubl/Desktop/heroin_data/PO/firstInjAgeCompare.csv", quote = FALSE, na = "0")

## Regular po injection in months
# 0, 1-12, 13-36, >36

regPoInj <- dfPOinj$poInjRegMo
regPoInj <- na.omit(regPoInj)
total <- length(regPoInj)

regPOInjNum1 <- length(regPoInj[regPoInj == 0])
regPOInjPer1 <- 100*(regPOInjNum1/total)
regPOInj1np <- paste(regPOInjNum1, ' (', round(regPOInjPer1, digits = 2), ')', sep = '')

regPOInjNum2 <- length(regPoInj[regPoInj >0 & regPoInj <=12])
regPOInjPer2 <- 100*(regPOInjNum2/total)
regPOInj2np <- paste(regPOInjNum2, ' (', round(regPOInjPer2, digits = 2), ')', sep = '')

regPOInjNum3 <- length(regPoInj[regPoInj >12 & regPoInj <=36])
regPOInjPer3 <- 100*(regPOInjNum3/total)
regPOInj3np <- paste(regPOInjNum3, ' (', round(regPOInjPer3, digits = 2), ')', sep = '')

regPOInjNum4 <- length(regPoInj[regPoInj >36])
regPOInjPer4 <- 100*(regPOInjNum4/total)
regPOInj4np <- paste(regPOInjNum4, ' (', round(regPOInjPer4, digits = 2), ')', sep = '')

regPOInjTable <- rbind(total, regPOInjMSD, regPOInj1np, regPOInj2np, regPOInj3np, regPOInj4np)
colnames(regPOInjTable) <- 'n (%) months'
rownames(regPOInjTable) <- c('total', 'mean(std)', '0', '1-12', '13-36', '>36')

write.csv(regPOInjTable, "/Users/jrteubl/Desktop/heroin_data/PO/regPOInj.csv", quote = FALSE, na = "0")

##po injection last 30 days
# 0, 1-7, 8-15, >15
l30POInj <- dfPOinj$poInjLast30
l30POInj <- na.omit(l30POInj)
total <- length(l30POInj)

l30POInjNum1 <- length(l30POInj[l30POInj == 0])
l30POInjPer1 <- 100*(l30POInjNum1/total)
l30POInj1np <- paste(l30POInjNum1, ' (', round(l30POInjPer1, digits = 2), ')', sep = '')

l30POInjNum2 <- length(l30POInj[l30POInj >0 & l30POInj <= 7])
l30POInjPer2 <- 100*(l30POInjNum2/total)
l30POInj2np <- paste(l30POInjNum2, ' (', round(l30POInjPer2, digits = 2), ')', sep = '')

l30POInjNum3 <- length(l30POInj[l30POInj >7 & l30POInj <= 15])
l30POInjPer3 <- 100*(l30POInjNum3/total)
l30POInj3np <- paste(l30POInjNum3, ' (', round(l30POInjPer3, digits = 2), ')', sep = '')

l30POInjNum4 <- length(l30POInj[l30POInj >15])
l30POInjPer4 <- 100*(l30POInjNum4/total)
l30POInj4np <- paste(l30POInjNum4, ' (', round(l30POInjPer4, digits = 2), ')', sep = '')

l30POInjTable <- rbind(total, l30POInjMSD,l30POInj1np, l30POInj2np, l30POInj3np, l30POInj4np)
colnames(l30POInjTable) <- 'n (%) days'
rownames(l30POInjTable) <- c('total', 'mean(std)', '0', '1-7', '8-15', '>15')

write.csv(l30POInjTable, "/Users/jrteubl/Desktop/heroin_data/PO/Last30POInj.csv", quote = FALSE, na = "0")

#### Regression ####

dfRegOD <- data.frame(poInjRegMo, overdoseGrp)
dfRegOD <- na.omit(dfRegOD)
mean <- mean(dfRegOD$poInjRegMo)
std <- sd(dfRegOD$poInjRegMo)
model <- glm(overdoseGrp~poInjRegMo, data = dfRegOD, family = binomial)
summary(model)
testNums = rnorm(10000, mean, std)
testData = data.frame(poInjRegMo = testNums)
probOD <- predict(model, testData, type="response")
testProbs <- data.frame(testNums, probOD)
sortTestProbs <- testProbs[order(testNums), ]
plot(sortTestProbs$testNums, sortTestProbs$probOD, ylab = 'probability', xlab = 'number of months',
     main = 'Probability of Overdose for Months of PO Injection', xlim = c(0, 100), pch = 20,
     col = 'blue')

dfRegHep <- data.frame(poInjRegMo, HepCGrp)
dfRegHep <- na.omit(dfRegHep)
model <- glm(HepCGrp~poInjRegMo, data = dfRegHep, family = binomial)
summary(model)
mean <- mean(dfRegHep$poInjRegMo)
std <- sd(dfRegHep$poInjRegMo)
testNums = rnorm(1000, mean, std)
testData = data.frame(poInjRegMo = testNums)
probOD <- predict(model, testData, type="response")
testProbs <- data.frame(testNums, probOD)
sortTestProbs <- testProbs[order(testNums), ]
plot(sortTestProbs$testNums, sortTestProbs$probOD, ylab = 'probability', xlab = 'number of months',
     main = 'Probability of HCV+ for Months of PO Injection', xlim = c(0, 100), pch = 20,
     col = 'blue')

