################################################################################
# R Script Header
# 
# Created by: Felipe Sanchez
# Contact: anfesanz@gmail.com
# Date: July 2023
#
# Description:
# Clean the data by performing the following steps:
# 1. Import the registers of non-fetal (nf) deaths in 2008 in SPSS format to R.
# 2. Clean the data by reclassifying the simple ages into 5 age groups and reclassifying the education levels.
# Calculate ax (life tables) by level of education using vital statistics (deaths) data from Colombia.
#
# Input: Defun_2018nf.sav (Registers of non-fetal (nf) deaths in 2008 in SPSS format)
#
# Outputs:
# ax2018.rds
#
################################################################################

# Clear the workspace
rm(list = ls(all = TRUE))  

# Set the working directory
getwd()
setwd("/Users/felipesanchez/Documents/GitHub/PopEdu Reconstruction/")


#install.packages("haven")
library(haven)
library(tidyverse)

#Dic Variables
# "GRU_ED1"
# 00 = Menor de una hora 
# 01 = Menor de un día
# 02 = De 1 a 6 días 
# 03 = De 7 a 27 días 
# 04 = De 28 a 29 días 
# 05 = De 1 a 5 meses 
# 06 = De 6 a 11 meses 
# 07 = De 1 año 
# 08 = De 2 a 4 años 
# 09 = De 5 a 9 años
# 10 = De 10 a 14 años 
# 11 = De 15 a 19 años 
# 12 = De 20 a 24 años 
# 13 = De 25 a 29 años 
# 14 = De 30 a 34 años 
# 15 = De 35 a 39 años 
# 16 = De 40 a 44 años 
# 17 = De 45 a 49 años
# 18 = De 50 a 54 años
# 19 = De 55 a 59 años
# 20 = De 60 a 64 años
# 21 = De 65 a 69 años 
# 22 = De 70 a 74 años 
# 23 = De 75 a 79 años 
# 24 = De 80 a 84 años 
# 25 = De 85 a 89 años 
# 26 = De 90 a 94 años 
# 27 = De 95 a 99 años 
# 28 = De 100 años y más 
# 29 = Edad desconocida


# "NIVEL_EDU"
#   1 = Preescolar 
# 2 = Básica primaria 
# 3 = Básica secundaria 
# 4 = Media académica o clásica 
# 5 = Media técnica 
# 6 = Normalista 
# 7 = Técnica profesional 
# 8 = Tecnológica 
# 9 = Profesional 
# 10 = Especialización 
# 11 = Maestría 
# 12 = Doctorado 
# 13 = Ninguno 
# 99 = Sin información
#Read data andData clean
Defun2018 <- as.tibble(read_sav("Data Original/Defun_2018nf.sav")) %>%
  select("ANO","MES","SEXO","GRU_ED1","NIVEL_EDU")  %>% #Keep relevant variables
  rename(year="ANO", month="MES", sex="SEXO", age1="GRU_ED1", edu="NIVEL_EDU") %>%
  mutate(month = as.numeric(as.character(month)), edu = as.numeric(as.character(edu))) %>% #As numeric haven_labelled, which is a class used by the haven package
  mutate(
    age_group = case_when(
      age1 %in% c("00", "01", "02", "03", "04", "05", "06") ~ "0",
      age1 %in% c("07", "08") ~ "1",
      age1 %in% c("09") ~ "5",
      age1 %in% c("10") ~ "10",
      age1 %in% c("11") ~ "15",
      age1 %in% c("12") ~ "20",
      age1 %in% c("13") ~ "25",
      age1 %in% c("14") ~ "30",
      age1 %in% c("15") ~ "35",
      age1 %in% c("16") ~ "40",
      age1 %in% c("17") ~ "45",
      age1 %in% c("18") ~ "50",
      age1 %in% c("19") ~ "55",
      age1 %in% c("20") ~ "60",
      age1 %in% c("21") ~ "65",
      age1 %in% c("22") ~ "70",
      age1 %in% c("23") ~ "75",
      age1 %in% c("24") ~ "80",
      age1 %in% c("25", "26", "27", "28") ~ "85+",
      age1 %in% c("29") ~ "NA") #Codification b age groups 0, 1, 5, 10 , ...
  ) %>%
  mutate(age_group = factor(age_group, levels = c("0", "1", "5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55", "60", "65", "70", "75", "80", "85+"), ordered = TRUE))%>%
  mutate(
    edu_level = case_when(
      edu %in% c(1, 13) ~ "1",
      edu %in% c(2, 3) ~ "2",
      edu %in% c(4, 5, 6, 7) ~ "3",
      edu %in% c(8, 9, 10, 11, 12) ~ "4",
      edu %in% c(99) ~ "NA")) %>%
  select(sex, age_group, edu_level, age1, month)


ax2018 <- Defun2018 %>% #Create edu level into four groups #collapse average of a_x by edu_level, sex, age_group
  mutate(
    a_x = case_when(
      age1 %in% c("00", "01", "02", "03", "04") ~ 0.0825,
      age1 %in% c("05") ~ 0.2475,
      age1 %in% c("06") ~ 0.7425,
      age1 %in% c("07") ~ 0.0825 * month,
      age1 %in% c("08") ~ 0.2491667 * month,
      age1 %in% c("09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24") ~ 0.4158333 * as.numeric(month),
      age_group %in% c("85+") ~ 0.8325 * month)) %>%  # 10 years 9.99/12, 5 years 4.99/12
  group_by(sex, age_group, edu_level) %>%
  summarise(a_x = mean(a_x, na.rm = TRUE)) %>%
  filter(sex!=3&edu_level!="NA") %>%
  pivot_wider(names_from = edu_level, values_from = a_x, names_prefix = "a_x_") %>%
  filter(age_group >= "15")


#Save information
saveRDS(df, "Data Created/ax2018.rds")


# Load the contents of the RDS file
#ax2018 <- readRDS("Data Created/ax2018.rds")


  

  
  