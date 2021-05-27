require(readxl)
require(writexl)
require(tidyverse)

#Creating transfusions DataFrame
data2011 = read_excel("E:/May/Combined data.xlsx", sheet = "2011")
data2012 = read_excel("E:/May/Combined data.xlsx", sheet = "2012")
data2013 = read_excel("E:/May/Combined data.xlsx", sheet = "2013")
data2014 = read_excel("E:/May/Combined data.xlsx", sheet = "2014")
data2015 = read_excel("E:/May/Combined data.xlsx", sheet = "2015")
data2016 = read_excel("E:/May/Combined data.xlsx", sheet = "2016")
data2017 = read_excel("E:/May/Combined data.xlsx", sheet = "2017")
data2018 = read_excel("E:/May/Combined data.xlsx", sheet = "2018")
data2019 = read_excel("E:/May/Combined data.xlsx", sheet = "2019")

transfusions = bind_rows(data2011, data2012, data2013, data2014, data2015, data2016, data2017, data2018, data2019)
transfusions$PtID = as.numeric(transfusions$PtID)
transfusions = subset(transfusions, !is.na(PtID))

#Creating PatientList DataFrame
PatientList = read_excel("C:/Users/mikel/Desktop/BloodBank/MAY/Boa5_ML.xlsx")
names(PatientList) = make.names(str_trim(names(PatientList)), unique = TRUE)
PatientList = subset(PatientList,Blood.this.admit.y.n %in% c("yes", "Yes", "YES") )
PatientList$MRN = as.numeric(PatientList$MRN)

#Searchs through Transfusions to fill Total Units for 1,4,and 24 hours
findTransfusions = function(pt_MRN, pt_AdmitTime)
{
  pts = subset(transfusions, PtID == pt_MRN)
  return(list(pts$Issue_Time - as.numeric(pt_AdmitTime)))
}

class(PatientList$Transfuses) = "list"
for(i in 1:nrow(PatientList))
{
  PatientList[i,]$Transfuses = findTransfusions(PatientList[i,]$MRN,PatientList[i,]$HospTD) 
}

for(i in 1:nrow(PatientList))
{
  uhr1 = sum(PatientList[i,]$Transfuses[[1]] > 0 & PatientList[i,]$Transfuses[[1]] < 3600)
  PatientList[i,]$total.u.hr.1 = uhr1
  uhr4 = sum(PatientList[i,]$Transfuses[[1]] > 0 & PatientList[i,]$Transfuses[[1]] < 3600 * 4)
  PatientList[i,]$total.u.1st4h = uhr4
  uhr24 = sum(PatientList[i,]$Transfuses[[1]] > 0 & PatientList[i,]$Transfuses[[1]] < 3600 * 24)
  PatientList[i,]$total.u.1st24h = uhr24
}

#Calculates Yes/No for 3units1hr, 5unit4hr, 10/20 units 24hr
PatientList$X..3u.h1.y.no = PatientList$total.u.hr.1 >= 3
PatientList$X..5u.1st4h.y.n = PatientList$total.u.1st4h >= 5
PatientList$X..10u.1st24h.y.n = PatientList$total.u.1st24h >= 10
PatientList$X..20u.1st24h.y.no = PatientList$total.u.1st24h >= 20

#Calculates Platelets
findPlatletTransfusions = function(pt_MRN, pt_AdmitTime)
{
  pts = subset(transfusions, PtID == pt_MRN)
  pts = subset(pts, Product == "PLG") 
  return(list(pts$Issue_Time - as.numeric(pt_AdmitTime)))
}

PatientList$PltTransfuses = ""
class(PatientList$Transfuses) = "list"
for(i in 1:nrow(PatientList))
{
  PatientList[i,]$PltTransfuses = findPlatletTransfusions(PatientList[i,]$MRN,PatientList[i,]$HospTD) 
}

for(i in 1:nrow(PatientList))
{
  Pltuhr1 = sum(PatientList[i,]$PltTransfuses[[1]] > 0 & PatientList[i,]$PltTransfuses[[1]] < 3600)
  PatientList[i,]$total.plts.h1 = Pltuhr1
  Pltuhr4 = sum(PatientList[i,]$PltTransfuses[[1]] > 0 & PatientList[i,]$PltTransfuses[[1]] < 3600 * 4)
  PatientList[i,]$total.plts.1st4h = Pltuhr4
  Pltuhr24 = sum(PatientList[i,]$PltTransfuses[[1]] > 0 & PatientList[i,]$PltTransfuses[[1]] < 3600 * 24)
  PatientList[i,]$total.plts.1st24h = Pltuhr24
}

#Calculates Cryo
findCryoTransfusions = function(pt_MRN, pt_AdmitTime)
{
  pts = subset(transfusions, PtID == pt_MRN)
  pts = subset(pts, Product == "CRYG") 
  return(list(pts$Issue_Time - as.numeric(pt_AdmitTime)))
}

PatientList$CryoTransfuses = ""
class(PatientList$Transfuses) = "list"
for(i in 1:nrow(PatientList))
{
  PatientList[i,]$CryoTransfuses = findCryoTransfusions(PatientList[i,]$MRN,PatientList[i,]$HospTD) 
}

for(i in 1:nrow(PatientList))
{
  Cryouhr1 = sum(PatientList[i,]$CryoTransfuses[[1]] > 0 & PatientList[i,]$CryoTransfuses[[1]] < 3600)
  PatientList[i,]$total.cryo.h1 = Cryouhr1
  Cryouhr4 = sum(PatientList[i,]$CryoTransfuses[[1]] > 0 & PatientList[i,]$CryoTransfuses[[1]] < 3600 * 4)
  PatientList[i,]$total.cryo.1st4h = Cryouhr4
  Cryouhr24 = sum(PatientList[i,]$CryoTransfuses[[1]] > 0 & PatientList[i,]$CryoTransfuses[[1]] < 3600 * 24)
  PatientList[i,]$total.cryo.1st24h = Cryouhr24
}


#Calculates RBC
findRBCTransfusions = function(pt_MRN, pt_AdmitTime)
{
  pts = subset(transfusions, PtID == pt_MRN)
  pts = subset(pts, Product == "RBCG") 
  return(list(pts$Issue_Time - as.numeric(pt_AdmitTime)))
}

PatientList$RBCTransfuses = ""
class(PatientList$Transfuses) = "list"
for(i in 1:nrow(PatientList))
{
  PatientList[i,]$RBCTransfuses = findRBCTransfusions(PatientList[i,]$MRN,PatientList[i,]$HospTD) 
}

for(i in 1:nrow(PatientList))
{
  RBCuhr1 = sum(PatientList[i,]$RBCTransfuses[[1]] > 0 & PatientList[i,]$RBCTransfuses[[1]] < 3600)
  PatientList[i,]$total.RBC.h.1 = RBCuhr1
  RBCuhr4 = sum(PatientList[i,]$RBCTransfuses[[1]] > 0 & PatientList[i,]$RBCTransfuses[[1]] < 3600 * 4)
  PatientList[i,]$total.RBC.1st4h = RBCuhr4
  RBCuhr24 = sum(PatientList[i,]$RBCTransfuses[[1]] > 0 & PatientList[i,]$RBCTransfuses[[1]] < 3600 * 24)
  PatientList[i,]$total.RBC.1st24h = RBCuhr24
}

#Calculates Plasma
findPlasmaTransfusions = function(pt_MRN, pt_AdmitTime)
{
  pts = subset(transfusions, PtID == pt_MRN)
  pts = subset(pts, Product == "PLSG") 
  return(list(pts$Issue_Time - as.numeric(pt_AdmitTime)))
}

PatientList$PlsmTransfuses = ""
class(PatientList$Transfuses) = "list"
for(i in 1:nrow(PatientList))
{
  PatientList[i,]$PlsmTransfuses = findPlasmaTransfusions(PatientList[i,]$MRN,PatientList[i,]$HospTD) 
}



write_xlsx(PatientList, "C:/Users/mikel/desktop/boa5.xlsx")