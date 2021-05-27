require(readxl)
require(tidyverse)
require(dplyr)

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
Blood_2019 = read_excel("F:/May/Bloodbank_Data_RAW/2019.xlsx")
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

#Binning unit types
transfusions$`Component Type Description`[transfusions$`Component Type Description` %in% c("CRYO PPL")] = "CRYO"
transfusions$`Component Type Description`[transfusions$`Component Type Description` %in% c("PLASMA","PLASMA POOL")] = "PLASMA"

#Adding Transfuses
#Transfuses is a list of transfusions, each transfusion is represented by a time in hours
#Since patient admit
findTransfusions = function(pt_MRN, pt_AdmitTime)
{
  pts = subset(transfusions, MRN == pt_MRN)
  return(list(as.numeric(difftime(as.POSIXct(pts$"Issue_TD", tz = "UTC"),as.POSIXct(pt_AdmitTime, tz = "UTC"), units = "hours"))))
}

recBlood = subset(Boa6, `blood this hosp y/n` %in% c("Yes","yes"))

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

recBlood = subset(Boa6, `blood this hosp y/n` %in% c("Yes","yes"))

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

#Merging the recBlood back with Boa6
#recBlood was the subset of patients that recieved blood
#this merges the two by MRN
temp = select(Boa6,-c(`total cryo h1`))
temp = left_join(temp,recBlood)
