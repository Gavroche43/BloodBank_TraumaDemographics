require(readxl)
require(ggplot2)
require(survival)
require(survminer)
require(lubridate)
require(ggfortify)
require(tidyverse)
require(writexl)

#Creating dataset
#Removes all patients withh ISS < 1
#Sets minimum tt.1st.unit as 0.01hr
#Compiles all Mechanism.bin
excelData = read_excel("C:/Users/mikel/Desktop/BloodBank/datasetFeb12.xlsx")
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

table(dataset$Outcome,useNA = "ifany")
summary(dataset$ISS)
summary(dataset$NISS)
summary(dataset$ttdh)
summary(dataset$tt.1st.unit)

table(dataset$Trauma.Type, useNA = "ifany")
table(dataset$Transf.bin, useNA = "ifany")
table(dataset$Mechanism.bin, useNA = "ifany")

#Creates subsets for analysis
falls = subset(dataset, Mechanism.bin == "FALL")
motor = subset(dataset, Mechanism.bin == "MVA")
burns = subset(dataset, Mechanism.bin == "BURN")
blunt = subset(dataset, Mechanism.bin == "blunt/crush")
sharp = subset(dataset, Mechanism.bin == "cut/stab/tear")
guns = subset(dataset, Mechanism.bin == "GSW")
other = subset(dataset, Mechanism.bin == "o/u")

output_table1 = data.frame(matrix(ncol = 8, nrow = 20))
colnames(output_table1) = c("All","Falls","Motors","Burns","Blunt/Crush", "Sharps","Guns","Other")


round_ = function(num)
{
    if(num == "")    {
        return(num)
    }
    else if(num < .000001)
    {
        return("<.000001")
    }
    else {
        return(round(num,digits = 6))
    }
}

#Fills out a column of data using the given dataset
create_column_table1 = function(data)
{
    not_data = subset(dataset, !dataset$Mechanism.bin %in% data$Mechanism.bin)
    temp = dataset
    temp$Mech.bin = ifelse(dataset$Mechanism.bin == data$Mechanism.bin[1], data$Mechanism.bin[1], "Other")
    isdata = nrow(not_data ) == 0
    Total = paste(nrow(data), 
                  ifelse(nrow(data) == nrow(dataset)," ",
                         paste(" (",  as.character(round(100 * nrow(data) / nrow(dataset), digits = 1)),")",sep = "")))
    Age = paste(round(mean(data$digage.yrs),digits = 1),
                "±", round(sd(data$digage.yrs), digits = 1), sep = "")
    if (!isdata)    {
        P1 = t.test(data$digage.yrs,not_data$digage.yrs)$p.value
    }
    else{
        P1 = ""
    }
    Male_sex = paste(sum(data$Sex == 'M'), " (",
                     round(sum(data$Sex == 'M')/nrow(data) * 100, digits = 1), ")", sep = "")
    if (!isdata)    {
        P2 = chisq.test(temp$Mech.bin, temp$Sex)$p.value
    }
    else{
        P2 = binom.test(sum(data$Sex == 'M'),nrow(data),p = 0.5, alternative = "two.sided")$p.value
    }
    Admitted_From_scene = paste(sum(data$Transf.bin == "N")," (",
                                round(sum(data$Transf.bin == "N")/nrow(data) * 100, digits = 1), ")", sep = "")
    Transferred = paste(sum(data$Transf.bin == "Y")," (",
                          round(sum(data$Transf.bin == "Y")/nrow(data) * 100, digits = 1), ")", sep = "")
    if (!isdata)    {
        P3 = chisq.test(temp$Mech.bin, temp$Transf.bin)$p.value
    }
    else{
        P3 = binom.test(sum(data$Transf.bin == 'Y'),nrow(data),p = 0.5, alternative = "two.sided")$p.value
    }
    Blunt_injury = paste(sum(data$Trauma.Type == "B"), " (",
                         round(sum(data$Trauma.Type == "B") / nrow(data) * 100, digits = 1),")", sep ="")
    Penetrating = paste(sum(data$Trauma.Type == "P"), " (",
       round(sum(data$Trauma.Type == "P") / nrow(data) * 100, digits = 1),")", sep ="")
    if (!isdata)    {
        P4 = chisq.test(temp$Mech.bin, temp$Trauma.Type)$p.value
    }
    else{
        P4 = ""
    }
    ISS = paste(round(mean(data$ISS), digits = 1), "±",
                round(sd(data$ISS), digits = 1), sep = "")
    if (!isdata)    {
        P5 = t.test(data$ISS,not_data$ISS)$p.value
    }
    else{
        P5 = ""
    }
    d = subset(data,Outcome == "D")
    Deaths = paste(sum(data$Outcome == "D"), " (",
                   round(sum(data$Outcome == "D")/nrow(data) * 100, digits = 1), ")", sep = "")
    if (!isdata)    {
        P6 = chisq.test(temp$Mech.bin, temp$Outcome)$p.value
    }
    else{
        P6 = ""
    }
    Age_at_death = paste(round(mean(d$digage.yrs), digits = 1), "±",
                        round(sd(d$digage.yrs), digits = 1), sep = "")
    if (!isdata)    {
        P7 = t.test(data$digage.yrs,not_data$digage.yrs)$p.value
    }
    else{
        P7 = ""
    }
    Male_deaths = paste(sum(d$Sex == "M"), " (",
                        round(sum(d$Sex == "M")/nrow(d) * 100, digits = 1), ")", sep = "")
    data$out = ifelse(data$Outcome == "D", TRUE, FALSE)
    P8 = round(summary(glm(out ~ digage.yrs, data = data, family = "binomial" ))$coefficients[2,4],digits = 6)
    P8 = ifelse(P8 < .000001,"<.000001",P8)
    Sex_death_P = round(chisq.test(data$Sex,data$Outcome)$p.value,digits = 6)
    return(c(Total, Age,round_(P1),Male_sex,round_(P2),Admitted_From_scene,
             Transferred,round_(P3),Blunt_injury,Penetrating,round_(P4),ISS,round_(P5),Deaths,round_(P6),
             Age_at_death,round_(P7), Male_sex,round_(P8),Sex_death_P))
}

output_table1$All = create_column_table1(dataset)
output_table1$Falls = create_column_table1(falls)
output_table1$Motors = create_column_table1(motor)
output_table1$Burns = create_column_table1(burns)
output_table1$`Blunt/Crush`= create_column_table1(blunt)
output_table1$Sharps = create_column_table1(sharp)
output_table1$Guns = create_column_table1(guns)
output_table1$Other = create_column_table1(other)


write_xlsx(output_table1,"C:/Users/mikel/Desktop/BloodBank/table1.xlsx")

output_table1a = data.frame(matrix(nrow = 12, ncol = 8))
output_table1b = data.frame(matrix(nrow = 12, ncol = 8))

colnames(output_table1a) = c("All","Falls","Motors","Burns","Blunt/Crush", "Sharps","Guns","Other")
colnames(output_table1b) = c("All","Falls","Motors","Burns","Blunt/Crush", "Sharps","Guns","Other")

create_column_table1sup = function(data)
{
    not_data = subset(subset(dataset, !dataset$Mechanism.bin %in% data$Mechanism.bin),Transf.bin == data$Transf.bin[1])
    temp = subset(dataset, Transf.bin == data$Transf.bin[1])
    temp$Mech.bin = ifelse(temp$Mechanism.bin == data$Mechanism.bin[1], data$Mechanism.bin[1], "Other")
    isdata = nrow(not_data ) == 0
    Total = paste(nrow(data), 
                  ifelse(nrow(data) == nrow(temp)," ",
                         paste(" (",  as.character(round(100 * nrow(data) / nrow(temp), digits = 1)),")",sep = "")))
    Age = paste(round(mean(data$digage.yrs),digits = 1),
                "±", round(sd(data$digage.yrs), digits = 1), sep = "")
    if (!isdata)    {
        P1 = t.test(data$digage.yrs,not_data$digage.yrs)$p.value
    }
    else{
        P1 = ""
    }
    Blunt_injury = paste(sum(data$Trauma.Type == "B"), " (",
                         round(sum(data$Trauma.Type == "B") / nrow(data) * 100, digits = 1),")", sep ="")
    Penetrating = paste(sum(data$Trauma.Type == "P"), " (",
                        round(sum(data$Trauma.Type == "P") / nrow(data) * 100, digits = 1),")", sep ="")
    if (!isdata)    {
        P2 = chisq.test(temp$Mech.bin, temp$Trauma.Type)$p.value
    }
    else{
        P2 = ""
    }
    ISS = paste(round(mean(data$ISS), digits = 1), "±",
                round(sd(data$ISS), digits = 1), sep = "")
    if (!isdata)    {
        P3 = t.test(data$ISS,not_data$ISS)$p.value
    }
    else{
        P3 = ""
    }
    d = subset(data,Outcome == "D")
    Deaths = paste(sum(data$Outcome == "D"), " (",
                   round(sum(data$Outcome == "D")/nrow(data) * 100, digits = 1), ")", sep = "")
    if (!isdata)    {
        P4 = chisq.test(temp$Mech.bin, temp$Outcome)$p.value
    }
    else{
        P4 = ""
    }
    Age_at_death = paste(round(mean(d$digage.yrs), digits = 1), "±",
                         round(sd(d$digage.yrs), digits = 1), sep = "")
    if (!isdata)    {
        P5 = t.test(data$digage.yrs,not_data$digage.yrs)$p.value
    }
    else{
        P5 = ""
    }
    return(c(Total, Age,round_(P1),Blunt_injury,Penetrating,
             round_(P2),ISS, round_(P3), Deaths, round_(P4), Age_at_death,round_(P5)))
}

output_table1a$All = create_column_table1sup(subset(dataset,Transf.bin == "N"))
output_table1a$Falls = create_column_table1sup(subset(falls,Transf.bin == "N"))
output_table1a$Motors = create_column_table1sup(subset(motor,Transf.bin == "N"))
output_table1a$Burns = create_column_table1sup(subset(burns,Transf.bin == "N"))
output_table1a$`Blunt/Crush`= create_column_table1sup(subset(blunt,Transf.bin == "N"))
output_table1a$Sharps = create_column_table1sup(subset(sharp,Transf.bin == "N"))
output_table1a$Guns = create_column_table1sup(subset(guns,Transf.bin == "N"))
output_table1a$Other = create_column_table1sup(subset(other,Transf.bin == "N"))

output_table1b$All = create_column_table1sup(subset(dataset,Transf.bin == "Y"))
output_table1b$Falls = create_column_table1sup(subset(falls,Transf.bin == "Y"))
output_table1b$Motors = create_column_table1sup(subset(motor,Transf.bin == "Y"))
output_table1b$Burns = create_column_table1sup(subset(burns,Transf.bin == "Y"))
output_table1b$`Blunt/Crush`= create_column_table1sup(subset(blunt,Transf.bin == "Y"))
output_table1b$Sharps = create_column_table1sup(subset(sharp,Transf.bin == "Y"))
output_table1b$Guns = create_column_table1sup(subset(guns,Transf.bin == "Y"))
output_table1b$Other = create_column_table1sup(subset(other,Transf.bin == "Y"))

write_xlsx(output_table1a,"C:/Users/mikel/Desktop/BloodBank/table1a.xlsx")
write_xlsx(output_table1b,"C:/Users/mikel/Desktop/BloodBank/table1b.xlsx")

