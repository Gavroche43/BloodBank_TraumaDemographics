require(readxl)
require(writexl)
require(tidyverse)

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


#COPY PASTED FROM FIRST BLOODBANKSCRIPT
excelData = read_excel("C:/Users/mikel/Desktop/BloodBank/FEBURARY/datasetFeb12.xlsx") #BOA4
names(excelData) = make.names(str_trim(names(excelData)), unique = TRUE)

dataset = subset(excelData, ISS >= 1)
dataset$COD.bin.general[dataset$COD.bin.general  %in% c("Bleeding","bleeding")] = "bleeding"
dataset$COD.bin.general[dataset$COD.bin.general  %in% c("CNS-high spine","CNS/highspine")] = "CNS/highspine"
dataset$COD.bin.general[dataset$COD.bin.general %in% c("o/u","o/u - medical", "o/u - suicide", "o/u burn","o/u - chemical")] = "o/u"


dataset$Mechanism.bin[dataset$Mechanism.bin %in% c("o/u","o/u - medical", "o/u - suicide", "o/u burn","o/u - chemical")] = "o/u"
dataset$Mechanism.bin[dataset$Mechanism.bin %in% c("MV", "MVa")] = "MVA"

table(dataset$Mechanism.bin, useNA = "ifany")
table(dataset$COD.bin.general, useNA = "ifany")

dataset$ISS = as.numeric(dataset$ISS)
dataset$NISS = as.numeric(dataset$NISS)
dataset$ttdh = as.numeric(dataset$ttdh)
dataset$digage.yrs = as.numeric(dataset$digage.yrs)
dataset$tt.1st.unit = as.numeric(dataset$tt.1st.unit)

dataset$tt.1st.unit[dataset$tt.1st.unit < 0.01] = 0.01
       
dataset$Blood.this.admit.y.n[dataset$Blood.this.admit.y.n %in% c("no", "No")] = "no"
dataset$Blood.this.admit.y.n[dataset$Blood.this.admit.y.n %in% c("yes", "Yes")] = "yes"
table(dataset$Blood.this.admit.y.n, useNA = "ifany")
#**************************


TIME_CONSTANT = 2209046400

dataset$MRN = (10000000-dataset$PtID)
working = subset(transfusions, PtID %in% dataset$MRN)
wtf2 = subset(transfusions,!PtID %in% dataset$MRN)
dataset$Admit_Time = as.numeric(as.POSIXct(dataset$Hosp.Arr.Date)) + as.numeric(as.POSIXct(dataset$Hosp.Arr.Time)) + TIME_CONSTANT

working$time = as.numeric(as.POSIXct(working$Issue_Time))

print(findAdmitTime(570095,as.numeric(as.POSIXct("2011-09-23 21:00:00"))))
working$ttu = -1
for(i in 1:nrow(working))
{
  working[i,]$ttu = findAdmitTime(working[i,]$PtID,working[i,]$time)
}

#Takes in a MRN and a Transfusion Time, searches through dataset and
# returns the time till transfuse calculated from the most recent admission
findAdmitTime = function(pt_MRN,time)
{
    admits = subset(dataset, MRN == pt_MRN)
    times = c()
    for(admit in admits$Admit_Time)
    {
      cat(time, " : ", admit, "\n")
        if(time - admit > 0)
        {
          times = append(times,time - admit)
        }
    }
    return(min(times) /3600)
}

working$o3unit1hr = FALSE
for(i in 1:nrow(working))
{
  print(i)
  ID = working[i,]$PtID
  patients = subset(working, PtID == ID)
  working[i,]$o3unit1hr = nrow(subset(patients, ttu < 1)) > 3
}
