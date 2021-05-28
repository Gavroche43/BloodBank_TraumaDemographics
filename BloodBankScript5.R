install.packages("sjmisc")

require(readxl)
require(tidyverse)
require(dplyr)
require(sjmisc)
require(writexl)

#*********Creating Boa6***********
#*Boa6 is a dataset from the original Boa6 files
#*Removed 2nd admission of 4694962
#*Merged 2020 patients to 
#*Merged admit time and date columns to Admit_TD

Boa6 = read_excel("F:/May/2011Apr6-2020Dec31.xlsx")

#Remove 4696962 second admission
Boa6 = Boa6[Boa6$MRN != 4696962 | Boa6$`HMC Arrival Mode` != "POV",]


#Merge Dataset with update that Lynn sent through email
update_2020 = read_excel("F:/May/Copy of end 2020 questions for SC-TR.xlsx")
update_2020 = subset(update_2020, !is.na(codeID))
update_2020$MRN = 10000000 - update_2020$codeID
for(pt in update_2020$MRN){
  if(pt %in% Boa6$MRN)
  {
    Boa6[Boa6$MRN == pt,]$Outcome = update_2020[update_2020$MRN == pt,]$Outcome
    Boa6[Boa6$MRN == pt,]$ttdh = update_2020[update_2020$MRN == pt,]$ttdh
  }
}

#Merging Admit time/date
Boa6$admit_TD = as.numeric(as.POSIXct(Boa6$`Hosp Arr Time`,tz= "GMT") - as.POSIXct("1899-12-31 00:00:00",tz= "GMT"))
Boa6$admit_TD = Boa6$admit_TD + Boa6$`Hosp Arr Date`

#Binning Transfer status
Boa6$Transfer[Boa6$Transfer %in% c("F","R","UC","Y")] = "Y"

#Binning Mechanism bin
Boa6$`Mechanism bin`[Boa6$`Mechanism bin` %in% c("Fall","falls")] = "FALL"
Boa6$`Mechanism bin`[Boa6$`Mechanism bin` %in% c("GSW","guns")] = "GSW"
Boa6$`Mechanism bin`[Boa6$`Mechanism bin` %in% c("motors","MV","MVA")] = "MVA"
Boa6$`Mechanism bin`[Boa6$`Mechanism bin` %in% c(" o/u","o/u - chemical","o/u - hanging",
                                                 "o/u - inhalation","o/u - medical","o/u - strain/sprain",
                                                 "o/u - strangulation","other/unk")] = "o/u"
#Binning Trauma Type
Boa6$`Trauma Type`[Boa6$`Trauma Type` %in% c("OTHER","SKIN")] = "OTHER"

#Dropping people w/o an ISS
Boa6$ISS = as.numeric(Boa6$ISS)
Boa6 = subset(Boa6, ISS %in% 1:75)
Boa6 = subset(Boa6,!is.na(ISS))

#Fixing Cod bin
Boa6$`COD bin`[Boa6$MRN == 4596023] = "CNS/highspine"
Boa6$`COD bin`[Boa6$MRN == 4660989] = "CNS/highspine"
Boa6$`COD bin`[Boa6$MRN == 4635339] = "o/u"

Boa6$`COD bin`[Boa6$`COD bin` %in% c("b","bleeding", "Bleeding")] = "Bleeding"
Boa6$`COD bin`[Boa6$`COD bin` %in% c("CNS-high spine","CNS/high-spine", " CNS/highspine")] = "CNS/highspine"
Boa6$`COD bin`[Boa6$`COD bin` %in% c("MOF","OF")] = "MOF"
Boa6$`COD bin`[Boa6$`COD bin` %in% c("o/u","o/u - medical","o/u - suicide","o/u burn","other/unk")] = "o/u"
Boa6$`COD bin`[Boa6$`COD bin` %in% c("poly/catastrophic","poly/catastrophic trauma")] = "poly/catastrophic trauma"

#Fixing `blood this hosp y/n` bins
Boa6$`blood this hosp y/n`[Boa6$`blood this hosp y/n` %in% c("Yes","yes")] = "Yes"
Boa6$`blood this hosp y/n`[Boa6$`blood this hosp y/n` %in% c("No","no")] = "No"



#*********Creating Transusions***********
#*Dataset of all transfusions from orginial 2011-2020 files
#*Cleaning them up
#*Merging Issue Date and Issue Time into Issue TD

Blood_2011 = read_excel("F:/May/Bloodbank_Data_RAW/2011.xlsx")
Blood_2012 = read_excel("F:/May/Bloodbank_Data_RAW/2012.xlsx")
Blood_2013 = read_excel("F:/May/Bloodbank_Data_RAW/2013.xlsx")
Blood_2014 = read_excel("F:/May/Bloodbank_Data_RAW/2014.xlsx")
Blood_2015 = read_excel("F:/May/Bloodbank_Data_RAW/2015.xlsx")
Blood_2016 = read_excel("F:/May/Bloodbank_Data_RAW/2016.xlsx")
Blood_2017 = read_excel("F:/May/Bloodbank_Data_RAW/2017.xlsx")
Blood_2018 = read_excel("F:/May/Bloodbank_Data_RAW/2018.xlsx")
Blood_2019 = read_excel("F:/May/Bloodbank_Data_RAW/2019.xlsx",sheet = "removed duplicates")
Blood_2020 = read_excel("F:/May/Bloodbank_Data_RAW/2020.xlsx")


#Aligning Columns
Blood_2011 = subset(Blood_2011, select=c("Issue Date","Issue Time","Patient Id","Patient ABO...4","Patient Rh...5",
                                         "Component Type Description","Unit ABO", "Unit Rh"))
names(Blood_2011)[names(Blood_2011) == "Patient ABO...4"] = "Patient ABO"
names(Blood_2011)[names(Blood_2011) == "Patient Rh...5"] = "Patient Rh"
Blood_2012 = subset(Blood_2012, select=c("Issue Date","Issue Time","Patient Id","Patient ABO...4","Patient Rh...5",
                                         "Component Type Description","Unit ABO", "Unit Rh"))
names(Blood_2012)[names(Blood_2012) == "Patient ABO...4"] = "Patient ABO"
names(Blood_2012)[names(Blood_2012) == "Patient Rh...5"] = "Patient Rh"
Blood_2013 = subset(Blood_2013, select=c("Issue Date","Issue Time","Patient Id","Patient ABO","Patient Rh",
                                         "Component Type Description","Unit ABO", "Unit Rh"))
Blood_2014 = subset(Blood_2014, select=c("Issue Date","Issue Time","Patient Id","Patient ABO","Patient Rh",
                                         "Component Type Description","Unit ABO", "Unit Rh"))
Blood_2015 = subset(Blood_2015, select=c("Issue Date","Issue Time","Patient Id","Patient ABO","Patient Rh",
                                         "Component Type Description","Unit ABO", "Unit Rh"))
Blood_2016 = subset(Blood_2016, select=c("Issue Date","Issue Time","Patient Id","Patient ABO","Patient Rh",
                                         "Component Type Description","Unit ABO", "Unit Rh"))
Blood_2017 = subset(Blood_2017, select=c("Issue Date","Issue Time","Patient Id","Patient ABO","Patient Rh",
                                         "Component Type Description","Unit ABO", "Unit Rh"))
Blood_2018 = subset(Blood_2018, select=c("Issue Date","Issue Time","Patient Id","Patient ABO","Patient Rh",
                                         "Component Type Description","Unit ABO", "Unit Rh"))
Blood_2019 = subset(Blood_2019, select=c("Issue Date","Issue Time","Patient Id","Patient ABO","Patient Rh",
                                         "Component Type Description","Unit ABO", "Unit Rh"))
Blood_2020 = subset(Blood_2020, select=c("Issue Date","Issue Time","Patient Id","Patient ABO","Patient Rh",
                                         "Component Type Description","Unit ABO", "Unit Rh"))

transfusions = rbind(Blood_2011,Blood_2012,Blood_2013,Blood_2014,Blood_2015,Blood_2016,Blood_2017,Blood_2018,Blood_2019,Blood_2020)

#Merging Issue Time and Issue Date
transfusions$Issue_TD = as.numeric(as.POSIXct(transfusions$`Issue Time`,tz= "GMT") - as.POSIXct("1899-12-31 00:00:00",tz= "GMT"))
transfusions$Issue_TD = transfusions$Issue_TD + transfusions$`Issue Date`
transfusions = subset(transfusions, select = c("Issue_TD","Patient Id","Patient ABO","Patient Rh",
                                               "Component Type Description","Unit ABO", "Unit Rh"))
#Dropping Medic patients
transfusions = subset(transfusions, !is.na(transfusions$`Patient ABO`))

#Converting MRN format to numeric
transfusions$MRN = as.numeric(substring(transfusions$`Patient Id`,2,10))
transfusions = subset(transfusions, !is.na(MRN))

#Binning unit types
transfusions$`Component Type Description`[transfusions$`Component Type Description` %in% c("CRYO PPL")] = "CRYO"
transfusions$`Component Type Description`[transfusions$`Component Type Description` %in% c("PLASMA","PLASMA POOL","CR PLASMA T")] = "PLASMA"
RBC_names = c("RBC AUTO","RBC DG","RBC DG IL","RBC DG L","RBC DG LR","RBC IL","RBC IRR","RBC IRRLR",
              "RBC L","RBC LR","RBC1","RBC1 L","RBC1 LR","RBC2","RBC2 L","RBC2 LR","RBCPH I L 1","RBCPH I L 2",
              "RBCPH L 1","RBCPH L 2","RBCW")
transfusions$`Component Type Description`[transfusions$`Component Type Description` %in% RBC_names] = "RBC"
transfusions$`Component Type Description`[transfusions$`Component Type Description` %in% c("WH BLD L")] = "WB"
transfusions$`Component Type Description`[!transfusions$`Component Type Description` %in% c("CRYO","RBC","WB","PLASMA")] = "Plt"


#Adding Transfuses
#Transfuses is a list of transfusions, each transfusion is represented by a time in hours
#Since patient admit
recBlood = subset(Boa6, `blood this hosp y/n` == "Yes")
findTransfusions = function(pt_MRN, pt_AdmitTime)
{
  pts = subset(transfusions, MRN == pt_MRN)
  return(list(as.numeric(difftime(as.POSIXct(pts$"Issue_TD", tz = "UTC"),as.POSIXct(pt_AdmitTime, tz = "UTC"), units = "hours"))))
}


recBlood$Transfuses = c("")
class(recBlood$Transfuses) = "list"
for(i in 1:nrow(recBlood))
{
  recBlood[i,]$Transfuses = findTransfusions(recBlood[i,]$MRN ,recBlood[i,]$admit_TD)
}

#Adding Cryo Transfuses
findCryo = function(pt_MRN, pt_AdmitTime)
{
  Cryo = subset(transfusions,`Component Type Description` == "CRYO")
  pts = subset(Cryo, MRN == pt_MRN)
  return(list(as.numeric(difftime(as.POSIXct(pts$"Issue_TD", tz = "UTC"),as.POSIXct(pt_AdmitTime, tz = "UTC"), units = "hours"))))
}

recBlood$Cryos = c("")
class(recBlood$Cryos) = "list"
for(i in 1:nrow(recBlood))
{
  recBlood[i,]$Cryos = findCryo(recBlood[i,]$MRN ,recBlood[i,]$admit_TD)
}

#Total Cryo 
class(recBlood$`total cryo h1`) = "integer"
class(recBlood$`total cryo 1st4h`) = "integer"
class(recBlood$`total cryo 1st24h`) = "integer"
for(i in 1:nrow(recBlood))
{
  Cryo1hr = sum(recBlood[i,]$Cryos[[1]] > 0 & recBlood[i,]$Cryos[[1]] < 1)
  recBlood[i,]$`total cryo h1` = Cryo1hr
  Cryo4hr = sum(recBlood[i,]$Cryos[[1]] > 0 & recBlood[i,]$Cryos[[1]] < 4)
  recBlood[i,]$`total cryo 1st4h` = Cryo4hr
  Cryo24hr = sum(recBlood[i,]$Cryos[[1]] > 0 & recBlood[i,]$Cryos[[1]] <  24)
  recBlood[i,]$`total cryo 1st24h` = Cryo24hr
}

#Adding Plasma Transfuses
findPlasma = function(pt_MRN, pt_AdmitTime)
{
  Plasma = subset(transfusions,`Component Type Description` == "PLASMA")
  pts = subset(Plasma, MRN == pt_MRN)
  return(list(as.numeric(difftime(as.POSIXct(pts$"Issue_TD", tz = "UTC"),as.POSIXct(pt_AdmitTime, tz = "UTC"), units = "hours"))))
}

recBlood$Plasmas = c("")
class(recBlood$Plasmas) = "list"
for(i in 1:nrow(recBlood))
{
  recBlood[i,]$Plasmas = findPlasma(recBlood[i,]$MRN ,recBlood[i,]$admit_TD)
}

#Total Plasma 
class(recBlood$`total plasma h1`) = "integer"
class(recBlood$`total plasma 1st4h`) = "integer"
class(recBlood$`total plasma 1st24h`) = "integer"
for(i in 1:nrow(recBlood))
{
  Plasma1hr = sum(recBlood[i,]$Plasmas[[1]] > 0 & recBlood[i,]$Plasmas[[1]] < 1)
  recBlood[i,]$`total plasma h1` = Plasma1hr
  Plasma4hr = sum(recBlood[i,]$Plasmas[[1]] > 0 & recBlood[i,]$Plasmas[[1]] < 4)
  recBlood[i,]$`total plasma 1st4h` = Plasma4hr
  Plasma24hr = sum(recBlood[i,]$Plasmas[[1]] > 0 & recBlood[i,]$Plasmas[[1]] <  24)
  recBlood[i,]$`total plasma 1st24h` = Plasma24hr
}

#Adding Platlet Transfuses
findPlatlet= function(pt_MRN, pt_AdmitTime)
{
  Platlet = subset(transfusions,`Component Type Description` == "Plt")
  pts = subset(Platlet, MRN == pt_MRN)
  return(list(as.numeric(difftime(as.POSIXct(pts$"Issue_TD", tz = "UTC"),as.POSIXct(pt_AdmitTime, tz = "UTC"), units = "hours"))))
}

recBlood$Platlets = c("")
class(recBlood$Platlets) = "list"
for(i in 1:nrow(recBlood))
{
  recBlood[i,]$Platlets = findPlatlet(recBlood[i,]$MRN ,recBlood[i,]$admit_TD)
}

#Total Platlet 
class(recBlood$`total plts h1`) = "integer"
class(recBlood$`total plts 1st4h`) = "integer"
class(recBlood$`total plts 1st24h`) = "integer"
for(i in 1:nrow(recBlood))
{
  Platlet1hr = sum(recBlood[i,]$Platlets[[1]] > 0 & recBlood[i,]$Platlets[[1]] < 1)
  recBlood[i,]$`total plts h1` = Platlet1hr
  Platlet4hr = sum(recBlood[i,]$Platlets[[1]] > 0 & recBlood[i,]$Platlets[[1]] < 4)
  recBlood[i,]$`total plts 1st4h` = Platlet4hr
  Platlet24hr = sum(recBlood[i,]$Platlets[[1]] > 0 & recBlood[i,]$Platlets[[1]] <  24)
  recBlood[i,]$`total plts 1st24h` = Platlet24hr
}

#Adding RBC Transfuses
findRBCs= function(pt_MRN, pt_AdmitTime)
{
  RBCs = subset(transfusions,`Component Type Description` == "RBC")
  pts = subset(RBCs, MRN == pt_MRN)
  return(list(as.numeric(difftime(as.POSIXct(pts$"Issue_TD", tz = "UTC"),as.POSIXct(pt_AdmitTime, tz = "UTC"), units = "hours"))))
}

recBlood$RBCs = c("")
class(recBlood$RBCs) = "list"
for(i in 1:nrow(recBlood))
{
  recBlood[i,]$RBCs = findRBCs(recBlood[i,]$MRN ,recBlood[i,]$admit_TD)
}

#Total RBC 
class(recBlood$`total RBC h 1`) = "integer"
class(recBlood$`total RBC 1st4h`) = "integer"
class(recBlood$`total RBC 1st24h`) = "integer"
for(i in 1:nrow(recBlood))
{
  RBC1hr = sum(recBlood[i,]$RBCs[[1]] > 0 & recBlood[i,]$RBCs[[1]] < 1)
  recBlood[i,]$`total RBC h 1` = RBC1hr
  RBC4hr = sum(recBlood[i,]$RBCs[[1]] > 0 & recBlood[i,]$RBCs[[1]] < 4)
  recBlood[i,]$`total RBC 1st4h` = RBC4hr
  RBC24hr = sum(recBlood[i,]$RBCs[[1]] > 0 & recBlood[i,]$RBCs[[1]] <  24)
  recBlood[i,]$`total RBC 1st24h` = RBC24hr
}

#Adding WB Transfuses
findWBs= function(pt_MRN, pt_AdmitTime)
{
  WBs = subset(transfusions,`Component Type Description` == "WB")
  pts = subset(WBs, MRN == pt_MRN)
  return(list(as.numeric(difftime(as.POSIXct(pts$"Issue_TD", tz = "UTC"),as.POSIXct(pt_AdmitTime, tz = "UTC"), units = "hours"))))
}

recBlood$WBs = c("")
class(recBlood$WBs) = "list"
for(i in 1:nrow(recBlood))
{
  recBlood[i,]$WBs = findWBs(recBlood[i,]$MRN ,recBlood[i,]$admit_TD)
}

#Total WB 
class(recBlood$`total WB h1`) = "integer"
class(recBlood$`total WB 1st4h`) = "integer"
class(recBlood$`total WB 1st24h`) = "integer"
for(i in 1:nrow(recBlood))
{
  WB1hr = sum(recBlood[i,]$WBs[[1]] > 0 & recBlood[i,]$WBs[[1]] < 1)
  recBlood[i,]$`total WB h1` = WB1hr
  WB4hr = sum(recBlood[i,]$WBs[[1]] > 0 & recBlood[i,]$WBs[[1]] < 4)
  recBlood[i,]$`total WB 1st4h` = WB4hr
  WB24hr = sum(recBlood[i,]$WBs[[1]] > 0 & recBlood[i,]$WBs[[1]] <  24)
  recBlood[i,]$`total WB 1st24h` = WB24hr
}

#Filling in other columns of recBlood
recBlood$`total u hr 1` = recBlood$`total cryo h1` + recBlood$`total plasma h1` + recBlood$`total plts h1` +
  recBlood$`total RBC h 1` + recBlood$`total WB h1`
recBlood$`sum AN:AR` = recBlood$`total u hr 1` - recBlood$`total RBC h 1`

recBlood$`total u 1st4h` = recBlood$`total cryo 1st4h` + recBlood$`total plasma 1st4h` + recBlood$`total plts 1st4h` +
  recBlood$`total RBC 1st4h` + recBlood$`total WB 1st4h`
recBlood$`sum AW:AZ` = recBlood$`total u 1st4h` - recBlood$`total RBC 1st4h`

recBlood$`total u 1st24h` = recBlood$`total cryo 1st24h` + recBlood$`total plasma 1st24h` + recBlood$`total plts 1st24h` +
  recBlood$`total RBC 1st24h` + recBlood$`total WB 1st24h`
recBlood$`sum BE:BH` = recBlood$`total u 1st24h` - recBlood$`total RBC 1st24h`

#Patient Rh/ABO
class(recBlood$`Pt Rh`) = "character"
class(recBlood$`Pt ABO`) = "character"
for(i in 1:length(recBlood))
{
  recBlood[i,]$`Pt Rh` = transfusions$`Patient Rh`[transfusions$MRN == recBlood[i,]$MRN][1]
  recBlood[i,]$`Pt ABO` = transfusions$`Patient ABO`[transfusions$MRN == recBlood[i,]$MRN][1]
}

#Preparing recBlood for merge
recBlood = subset(recBlood, select = `BB #s`:`admit_TD`)
INSIGHT = rbind(subset(Boa6,`blood this hosp y/n` == "No"),recBlood)

keep = names(INSIGHT)
keep = keep[!keep %in% c("BB #s","Hosp Arr Date","Hosp Arr Time","Date of Death","Time of Death",
                         "Hospital DC Date","Hospital DC Time","admit_TD")]
INSIGHT = subset(INSIGHT, select = keep)

#Output file
