 #   ***   Filling out Table 2. and Table 2.a/b   ***
#Creates subsets for analysis

burnless = subset(dataset, Mechanism.bin != "BURN")
burnless$total.units.this.admit = as.numeric(burnless$total.units.this.admit)

falls_B = subset(burnless, Mechanism.bin == "FALL")
motor_B = subset(burnless, Mechanism.bin == "MVA")
blunt_B = subset(burnless, Mechanism.bin == "blunt/crush")
sharp_B = subset(burnless, Mechanism.bin == "cut/stab/tear")
guns_B = subset(burnless, Mechanism.bin == "GSW")
other_B = subset(burnless, Mechanism.bin == "o/u")


#Creates the tables to be outputted as data frames
output_table2 = data.frame(matrix(ncol = 7, nrow = 23))
colnames(output_table2) = c("All","Falls","Motors","Blunt/Crush", "Sharps","Guns","Other")

output_table2a = data.frame(matrix(ncol = 7, nrow = 23))
colnames(output_table2a) = c("All","Falls","Motors","Blunt/Crush", "Sharps","Guns","Other")

output_table2b = data.frame(matrix(ncol = 7, nrow = 23))
colnames(output_table2b) = c("All","Falls","Motors","Blunt/Crush", "Sharps","Guns","Other")


#Accepts a subset for analysis. Returns a populated column for table 2
create_column_table2 = function(data){
  
  not_data = subset(burnless, !burnless$Mechanism.bin %in% data$Mechanism.bin)
  temp = burnless
  temp$Mech.bin = ifelse(burnless$Mechanism.bin == data$Mechanism.bin[1], data$Mechanism.bin[1], "Other")
  
  data$blood_1hr = ifelse(data$tt.1st.unit <= 1 , TRUE, FALSE)
  data$blood_1hr = ifelse(is.na(data$blood_1hr), FALSE, data$blood_1hr)
  temp$blood_1hr = ifelse(temp$tt.1st.unit <= 1 , TRUE, FALSE)
  temp$blood_1hr = ifelse(is.na(temp$blood_1hr), FALSE, temp$blood_1hr)
  
  data$blood_4hr = ifelse(data$tt.1st.unit <= 4 , TRUE, FALSE)
  data$blood_4hr = ifelse(is.na(data$blood_4hr), FALSE, data$blood_4hr)
  temp$blood_4hr = ifelse(temp$tt.1st.unit <= 4 , TRUE, FALSE)
  temp$blood_4hr = ifelse(is.na(temp$blood_4hr), FALSE, temp$blood_4hr)
  
  data$RBC_WB = data$RBC + data$WB
  not_data$RBC_WB = not_data$RBC + not_data$WB
  
  isdata = nrow(not_data ) == 0
  

  Used_any_blood = paste(sum(data$Blood.this.admit.y.n == "yes")," (",
                         round(sum(data$Blood.this.admit.y.n == "yes") * 100 / nrow(data), digits = 1),")", sep = "")
  if (!isdata)    {
    P1 = chisq.test(temp$Mech.bin, temp$Blood.this.admit.y.n)$p.value
  }
  else{
    P1 = binom.test(sum(data$Blood.this.admit.y.n == 'yes'),nrow(data),p = 0.5, alternative = "two.sided")$p.value
  }
  
  
  blood_1hr = paste(sum(data$blood_1hr)," (",
                    round(sum(data$blood_1hr) * 100 / nrow(data), digits = 1),")", sep = "")
  
  if (!isdata)    {
    P2 = chisq.test(temp$Mech.bin, temp$blood_1hr)$p.value
  }
  else{
    P2 = binom.test(sum(data$blood_1hr == 'yes'),nrow(data),p = 0.5, alternative = "two.sided")$p.value
  }
  
  blood_4hr = paste(sum(data$blood_4hr)," (",
                    round(sum(data$blood_4hr) * 100 / nrow(data), digits = 1),")", sep = "")
  
  if (!isdata)    {
    P3 = chisq.test(temp$Mech.bin, temp$blood_4hr)$p.value
  }
  else{
    P3 = binom.test(sum(data$blood_4hr == 'yes'),nrow(data),p = 0.5, alternative = "two.sided")$p.value
  }
  
  Total_units = paste(round(mean(data$total.units.this.admit, na.rm = TRUE),digits = 1), " (", 
                      round(sd(data$total.units.this.admit, na.rm = TRUE), digits = 1) , ")", sep = "")
  
  if (!isdata)    {
    P5 = t.test(data$total.units.this.admit,not_data$total.units.this.admit)$p.value
  }
  else{
    P5 = ""
  }
  
  RBC_WB =  paste(round(mean(data$RBC_WB, na.rm = TRUE),digits = 1), " (", 
                  round(sd(data$RBC_WB, na.rm = TRUE), digits = 1) , ")", sep = "")
  if (!isdata)    {
    P6 = t.test(data$RBC_WB,not_data$RBC_WB)$p.value
  }
  else{
    P6 = ""
  }
  
  DEAD = subset(data,Outcome == "D")
  DEATHS = paste(nrow(DEAD), " (", round(nrow(DEAD) *100 / nrow(data), digits = 1), ") (",
                 round(nrow(DEAD) * 100 / nrow(DEAD),digits = 1),")", sep = "")
  Hrs_2_ded = paste(round(quantile(DEAD$ttdh)["50%"],digits = 1), " (", round(quantile(DEAD$ttdh)["25%"],digits = 1),
                    "-",round(quantile(DEAD$ttdh)["75%"],digits = 1),")",sep = "")
  
  ded_poly = subset(DEAD, COD.bin.general == "poly/catastrophic trauma")
  DEATHS_poly = paste(nrow(ded_poly), " (", round(nrow(ded_poly) *100 / nrow(data), digits = 1), ") (",
                      round(nrow(ded_poly) * 100 / nrow(DEAD),digits = 1),")", sep = "")
  Hrs_2_ded_poly = paste(round(quantile(ded_poly$ttdh)["50%"],digits = 1), " (", round(quantile(ded_poly$ttdh)["25%"],digits = 1),
                    "-",round(quantile(ded_poly$ttdh)["75%"],digits = 1),")",sep = "")
  
  ded_bleed = subset(DEAD, COD.bin.general == "bleeding")
  DEATHS_bleed = paste(nrow(ded_bleed), " (", round(nrow(ded_bleed) *100 / nrow(data), digits = 1), ") (",
                      round(nrow(ded_bleed) * 100 / nrow(DEAD),digits = 1),")", sep = "")
  Hrs_2_ded_bleed = paste(round(quantile(ded_bleed$ttdh)["50%"],digits = 1), " (", round(quantile(ded_bleed$ttdh)["25%"],digits = 1),
                         "-",round(quantile(ded_bleed$ttdh)["75%"],digits = 1),")",sep = "")
  
  ded_cns = subset(DEAD, COD.bin.general == "CNS/highspine")
  DEATHS_cns = paste(nrow(ded_cns), " (", round(nrow(ded_cns) *100 / nrow(data), digits = 1), ") (",
                       round(nrow(ded_cns) * 100 / nrow(DEAD),digits = 1),")", sep = "")
  Hrs_2_ded_cns = paste(round(quantile(ded_cns$ttdh)["50%"],digits = 1), " (", round(quantile(ded_cns$ttdh)["25%"],digits = 1),
                          "-",round(quantile(ded_cns$ttdh)["75%"],digits = 1),")",sep = "")
  
  ded_mof = subset(DEAD, COD.bin.general == "MOF")
  DEATHS_mof = paste(nrow(ded_mof), " (", round(nrow(ded_mof) *100 / nrow(data), digits = 1), ") (",
                     round(nrow(ded_mof) * 100 / nrow(DEAD),digits = 1),")", sep = "")
  Hrs_2_ded_mof = paste(round(quantile(ded_mof$ttdh)["50%"],digits = 1), " (", round(quantile(ded_mof$ttdh)["25%"],digits = 1),
                        "-",round(quantile(ded_mof$ttdh)["75%"],digits = 1),")",sep = "")
  
  ded_other = subset(DEAD, COD.bin.general == "o/u")
  DEATHS_other = paste(nrow(ded_other), " (", round(nrow(ded_other) *100 / nrow(data), digits = 1), ") (",
                     round(nrow(ded_other) * 100 / nrow(DEAD),digits = 1),")", sep = "")
  Hrs_2_ded_other = paste(round(quantile(ded_other$ttdh)["50%"],digits = 1), " (", round(quantile(ded_other$ttdh)["25%"],digits = 1),
                        "-",round(quantile(ded_other$ttdh)["75%"],digits = 1),")",sep = "")
  
  return(c(Used_any_blood, round_(P1), blood_1hr,round_(P2),blood_4hr,round_(P3),Total_units,round_(P5),RBC_WB,round_(P6),"No. (%)1a (%)1b",
          DEATHS,Hrs_2_ded,DEATHS_poly, Hrs_2_ded_poly,DEATHS_bleed,Hrs_2_ded_bleed,
          DEATHS_cns,Hrs_2_ded_cns,DEATHS_mof,Hrs_2_ded_mof,DEATHS_other,Hrs_2_ded_other))
}

output_table2$All = create_column_table2(burnless)
output_table2$Falls = create_column_table2(subset(burnless, Mechanism.bin == "FALL"))
output_table2$Motors = create_column_table2(subset(burnless, Mechanism.bin == "MVA"))
output_table2$`Blunt/Crush` = create_column_table2(subset(burnless, Mechanism.bin == "blunt/crush"))
output_table2$Sharps = create_column_table2(subset(burnless, Mechanism.bin == "cut/stab/tear"))
output_table2$Guns = create_column_table2(subset(burnless, Mechanism.bin == "GSW"))
output_table2$Other = create_column_table2(subset(burnless, Mechanism.bin == "o/u"))

write_xlsx(output_table2,"C:/Users/mikel/Desktop/BloodBank/table2.xlsx")

output_table2a$All = create_column_table2(subset(burnless,Transf.bin == "N"))
output_table2a$Falls = create_column_table2(subset(subset(burnless, Mechanism.bin == "FALL"),Transf.bin == "N"))
output_table2a$Motors = create_column_table2(subset(subset(burnless, Mechanism.bin == "MVA"),Transf.bin == "N"))
output_table2a$`Blunt/Crush` = create_column_table2(subset(subset(burnless, Mechanism.bin == "blunt/crush"),Transf.bin == "N"))
output_table2a$Sharps = create_column_table2(subset(subset(burnless, Mechanism.bin == "cut/stab/tear"),Transf.bin == "N"))
output_table2a$Guns = create_column_table2(subset(subset(burnless, Mechanism.bin == "GSW"),Transf.bin == "N"))
output_table2a$Other = create_column_table2(subset(subset(burnless, Mechanism.bin == "o/u"),Transf.bin == "N"))

write_xlsx(output_table2a,"C:/Users/mikel/Desktop/BloodBank/table2a.xlsx")

output_table2b$All = create_column_table2(subset(burnless,Transf.bin == "Y"))
output_table2b$Falls = create_column_table2(subset(subset(burnless, Mechanism.bin == "FALL"),Transf.bin == "Y"))
output_table2b$Motors = create_column_table2(subset(subset(burnless, Mechanism.bin == "MVA"),Transf.bin == "Y"))
output_table2b$`Blunt/Crush` = create_column_table2(subset(subset(burnless, Mechanism.bin == "blunt/crush"),Transf.bin == "Y"))
output_table2b$Sharps = create_column_table2(subset(subset(burnless, Mechanism.bin == "cut/stab/tear"),Transf.bin == "Y"))
output_table2b$Guns = create_column_table2(subset(subset(burnless, Mechanism.bin == "GSW"),Transf.bin == "Y"))
output_table2b$Other = create_column_table2(subset(subset(burnless, Mechanism.bin == "o/u"),Transf.bin == "Y"))

write_xlsx(output_table2b,"C:/Users/mikel/Desktop/BloodBank/table2b.xlsx")

=======
#   ***   Filling out Table 2. and Table 2.a/b   ***
#Creates subsets for analysis

burnless = subset(dataset, Mechanism.bin != "BURN")
burnless$total.units.this.admit = as.numeric(burnless$total.units.this.admit)

falls_B = subset(burnless, Mechanism.bin == "FALL")
motor_B = subset(burnless, Mechanism.bin == "MVA")
blunt_B = subset(burnless, Mechanism.bin == "blunt/crush")
sharp_B = subset(burnless, Mechanism.bin == "cut/stab/tear")
guns_B = subset(burnless, Mechanism.bin == "GSW")
other_B = subset(burnless, Mechanism.bin == "o/u")


#Creates the tables to be outputted as data frames
output_table2 = data.frame(matrix(ncol = 7, nrow = 23))
colnames(output_table2) = c("All","Falls","Motors","Blunt/Crush", "Sharps","Guns","Other")

output_table2a = data.frame(matrix(ncol = 7, nrow = 23))
colnames(output_table2a) = c("All","Falls","Motors","Blunt/Crush", "Sharps","Guns","Other")

output_table2b = data.frame(matrix(ncol = 7, nrow = 23))
colnames(output_table2b) = c("All","Falls","Motors","Blunt/Crush", "Sharps","Guns","Other")


#Accepts a subset for analysis. Returns a populated column for table 2
create_column_table2 = function(data){
  
  not_data = subset(burnless, !burnless$Mechanism.bin %in% data$Mechanism.bin)
  temp = burnless
  temp$Mech.bin = ifelse(burnless$Mechanism.bin == data$Mechanism.bin[1], data$Mechanism.bin[1], "Other")
  
  data$blood_1hr = ifelse(data$tt.1st.unit <= 1 , TRUE, FALSE)
  data$blood_1hr = ifelse(is.na(data$blood_1hr), FALSE, data$blood_1hr)
  temp$blood_1hr = ifelse(temp$tt.1st.unit <= 1 , TRUE, FALSE)
  temp$blood_1hr = ifelse(is.na(temp$blood_1hr), FALSE, temp$blood_1hr)
  
  data$blood_4hr = ifelse(data$tt.1st.unit <= 4 , TRUE, FALSE)
  data$blood_4hr = ifelse(is.na(data$blood_4hr), FALSE, data$blood_4hr)
  temp$blood_4hr = ifelse(temp$tt.1st.unit <= 4 , TRUE, FALSE)
  temp$blood_4hr = ifelse(is.na(temp$blood_4hr), FALSE, temp$blood_4hr)
  
  data$RBC_WB = data$RBC + data$WB
  not_data$RBC_WB = not_data$RBC + not_data$WB
  
  isdata = nrow(not_data ) == 0
  

  Used_any_blood = paste(sum(data$Blood.this.admit.y.n == "yes")," (",
                         round(sum(data$Blood.this.admit.y.n == "yes") * 100 / nrow(data), digits = 1),")", sep = "")
  if (!isdata)    {
    P1 = chisq.test(temp$Mech.bin, temp$Blood.this.admit.y.n)$p.value
  }
  else{
    P1 = binom.test(sum(data$Blood.this.admit.y.n == 'yes'),nrow(data),p = 0.5, alternative = "two.sided")$p.value
  }
  
  
  blood_1hr = paste(sum(data$blood_1hr)," (",
                    round(sum(data$blood_1hr) * 100 / nrow(data), digits = 1),")", sep = "")
  
  if (!isdata)    {
    P2 = chisq.test(temp$Mech.bin, temp$blood_1hr)$p.value
  }
  else{
    P2 = binom.test(sum(data$blood_1hr == 'yes'),nrow(data),p = 0.5, alternative = "two.sided")$p.value
  }
  
  blood_4hr = paste(sum(data$blood_4hr)," (",
                    round(sum(data$blood_4hr) * 100 / nrow(data), digits = 1),")", sep = "")
  
  if (!isdata)    {
    P3 = chisq.test(temp$Mech.bin, temp$blood_4hr)$p.value
  }
  else{
    P3 = binom.test(sum(data$blood_4hr == 'yes'),nrow(data),p = 0.5, alternative = "two.sided")$p.value
  }
  
  Total_units = paste(round(mean(data$total.units.this.admit, na.rm = TRUE),digits = 1), " (", 
                      round(sd(data$total.units.this.admit, na.rm = TRUE), digits = 1) , ")", sep = "")
  
  if (!isdata)    {
    P5 = t.test(data$total.units.this.admit,not_data$total.units.this.admit)$p.value
  }
  else{
    P5 = ""
  }
  
  RBC_WB =  paste(round(mean(data$RBC_WB, na.rm = TRUE),digits = 1), " (", 
                  round(sd(data$RBC_WB, na.rm = TRUE), digits = 1) , ")", sep = "")
  if (!isdata)    {
    P6 = t.test(data$RBC_WB,not_data$RBC_WB)$p.value
  }
  else{
    P6 = ""
  }
  
  DEAD = subset(data,Outcome == "D")
  DEATHS = paste(nrow(DEAD), " (", round(nrow(DEAD) *100 / nrow(data), digits = 1), ") (",
                 round(nrow(DEAD) * 100 / nrow(DEAD),digits = 1),")", sep = "")
  Hrs_2_ded = paste(round(quantile(DEAD$ttdh)["50%"],digits = 1), " (", round(quantile(DEAD$ttdh)["25%"],digits = 1),
                    "-",round(quantile(DEAD$ttdh)["75%"],digits = 1),")",sep = "")
  
  ded_poly = subset(DEAD, COD.bin.general == "poly/catastrophic trauma")
  DEATHS_poly = paste(nrow(ded_poly), " (", round(nrow(ded_poly) *100 / nrow(data), digits = 1), ") (",
                      round(nrow(ded_poly) * 100 / nrow(DEAD),digits = 1),")", sep = "")
  Hrs_2_ded_poly = paste(round(quantile(ded_poly$ttdh)["50%"],digits = 1), " (", round(quantile(ded_poly$ttdh)["25%"],digits = 1),
                    "-",round(quantile(ded_poly$ttdh)["75%"],digits = 1),")",sep = "")
  
  ded_bleed = subset(DEAD, COD.bin.general == "bleeding")
  DEATHS_bleed = paste(nrow(ded_bleed), " (", round(nrow(ded_bleed) *100 / nrow(data), digits = 1), ") (",
                      round(nrow(ded_bleed) * 100 / nrow(DEAD),digits = 1),")", sep = "")
  Hrs_2_ded_bleed = paste(round(quantile(ded_bleed$ttdh)["50%"],digits = 1), " (", round(quantile(ded_bleed$ttdh)["25%"],digits = 1),
                         "-",round(quantile(ded_bleed$ttdh)["75%"],digits = 1),")",sep = "")
  
  ded_cns = subset(DEAD, COD.bin.general == "CNS/highspine")
  DEATHS_cns = paste(nrow(ded_cns), " (", round(nrow(ded_cns) *100 / nrow(data), digits = 1), ") (",
                       round(nrow(ded_cns) * 100 / nrow(DEAD),digits = 1),")", sep = "")
  Hrs_2_ded_cns = paste(round(quantile(ded_cns$ttdh)["50%"],digits = 1), " (", round(quantile(ded_cns$ttdh)["25%"],digits = 1),
                          "-",round(quantile(ded_cns$ttdh)["75%"],digits = 1),")",sep = "")
  
  ded_mof = subset(DEAD, COD.bin.general == "MOF")
  DEATHS_mof = paste(nrow(ded_mof), " (", round(nrow(ded_mof) *100 / nrow(data), digits = 1), ") (",
                     round(nrow(ded_mof) * 100 / nrow(DEAD),digits = 1),")", sep = "")
  Hrs_2_ded_mof = paste(round(quantile(ded_mof$ttdh)["50%"],digits = 1), " (", round(quantile(ded_mof$ttdh)["25%"],digits = 1),
                        "-",round(quantile(ded_mof$ttdh)["75%"],digits = 1),")",sep = "")
  
  ded_other = subset(DEAD, COD.bin.general == "o/u")
  DEATHS_other = paste(nrow(ded_other), " (", round(nrow(ded_other) *100 / nrow(data), digits = 1), ") (",
                     round(nrow(ded_other) * 100 / nrow(DEAD),digits = 1),")", sep = "")
  Hrs_2_ded_other = paste(round(quantile(ded_other$ttdh)["50%"],digits = 1), " (", round(quantile(ded_other$ttdh)["25%"],digits = 1),
                        "-",round(quantile(ded_other$ttdh)["75%"],digits = 1),")",sep = "")
  
  return(c(Used_any_blood, round_(P1), blood_1hr,round_(P2),blood_4hr,round_(P3),Total_units,round_(P5),RBC_WB,round_(P6),"No. (%)1a (%)1b",
          DEATHS,Hrs_2_ded,DEATHS_poly, Hrs_2_ded_poly,DEATHS_bleed,Hrs_2_ded_bleed,
          DEATHS_cns,Hrs_2_ded_cns,DEATHS_mof,Hrs_2_ded_mof,DEATHS_other,Hrs_2_ded_other))
}

output_table2$All = create_column_table2(burnless)
output_table2$Falls = create_column_table2(subset(burnless, Mechanism.bin == "FALL"))
output_table2$Motors = create_column_table2(subset(burnless, Mechanism.bin == "MVA"))
output_table2$`Blunt/Crush` = create_column_table2(subset(burnless, Mechanism.bin == "blunt/crush"))
output_table2$Sharps = create_column_table2(subset(burnless, Mechanism.bin == "cut/stab/tear"))
output_table2$Guns = create_column_table2(subset(burnless, Mechanism.bin == "GSW"))
output_table2$Other = create_column_table2(subset(burnless, Mechanism.bin == "o/u"))

write_xlsx(output_table2,"C:/Users/mikel/Desktop/BloodBank/table2.xlsx")

output_table2a$All = create_column_table2(subset(burnless,Transf.bin == "N"))
output_table2a$Falls = create_column_table2(subset(subset(burnless, Mechanism.bin == "FALL"),Transf.bin == "N"))
output_table2a$Motors = create_column_table2(subset(subset(burnless, Mechanism.bin == "MVA"),Transf.bin == "N"))
output_table2a$`Blunt/Crush` = create_column_table2(subset(subset(burnless, Mechanism.bin == "blunt/crush"),Transf.bin == "N"))
output_table2a$Sharps = create_column_table2(subset(subset(burnless, Mechanism.bin == "cut/stab/tear"),Transf.bin == "N"))
output_table2a$Guns = create_column_table2(subset(subset(burnless, Mechanism.bin == "GSW"),Transf.bin == "N"))
output_table2a$Other = create_column_table2(subset(subset(burnless, Mechanism.bin == "o/u"),Transf.bin == "N"))

write_xlsx(output_table2a,"C:/Users/mikel/Desktop/BloodBank/table2a.xlsx")

output_table2b$All = create_column_table2(subset(burnless,Transf.bin == "Y"))
output_table2b$Falls = create_column_table2(subset(subset(burnless, Mechanism.bin == "FALL"),Transf.bin == "Y"))
output_table2b$Motors = create_column_table2(subset(subset(burnless, Mechanism.bin == "MVA"),Transf.bin == "Y"))
output_table2b$`Blunt/Crush` = create_column_table2(subset(subset(burnless, Mechanism.bin == "blunt/crush"),Transf.bin == "Y"))
output_table2b$Sharps = create_column_table2(subset(subset(burnless, Mechanism.bin == "cut/stab/tear"),Transf.bin == "Y"))
output_table2b$Guns = create_column_table2(subset(subset(burnless, Mechanism.bin == "GSW"),Transf.bin == "Y"))
output_table2b$Other = create_column_table2(subset(subset(burnless, Mechanism.bin == "o/u"),Transf.bin == "Y"))

write_xlsx(output_table2b,"C:/Users/mikel/Desktop/BloodBank/table2b.xlsx")

>>>>>>> f35905635fdebdacb182f86d5f6032a1830cc633
