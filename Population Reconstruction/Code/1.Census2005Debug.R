################################################################################
# R Script Header
# 
# Created by: Felipe Sánchez
# Contact: anfesanz@gmail.com
# Date: Feb, May, Jul 2023
#
# Description:
# This script imports the Census 2005 information for Colombia by education from http://redatam.org/.
# It cleans and recodes the imported information.
#
# Input:
# - reporte2005 (Census 2005 raw data)
#
# Output:
# - Census2005_d (Cleaned and debugged Census 2005 data)
#
################################################################################


rm(list = ls(all = TRUE))  # Clear the workspace

# Working directory
getwd()
#setwd("/Users/felipesanchez/Library/CloudStorage/Dropbox-Personal/UoM/UoM from MA/Mortality by Education/")
setwd("/Users/felipesanchez/Documents/GitHub/PopEdu Reconstruction")

#Call packages
library(tidyverse)
library(readxl)
library(writexl)

#Census 2005
#Import tabulate information
census2005 <- read_excel("Data Original/reporte2005.xlsx", col_types = c("text", "text", "text", "text", "text"), skip = 12)
#Excel depuration of merge cells
census2005[1,1]<-"Preescolar"

#Rename Columns to Male, Female and Age, fill merged cells and debug data
census2005  <- census2005 %>%
  rename(Education=Preescolar, Age=...2, Male=Hombre, Female=Mujer) %>%
  fill(Education) %>% #fill merged cells
  filter(Male!="Hombre"  & Age!="Total")  %>% #drop rows with names
  filter(Education!="Total") %>% #drop information with total sums
  mutate(Age  = parse_number(Age)) %>% #As Age containts characters eg 3 anos, here we get only the numbers
  mutate(Age=as.numeric(Age), Male=as.numeric(Male),Female=as.numeric(Female), Total=as.numeric(Total)) %>% #Characters to numeric
  replace(is.na(.), 0)  #NA to zeros 

#Levels of Education definition.
census2005  <- census2005 %>%
  mutate(eduFactor = factor(Education) %>%
         fct_recode("1" = "Preescolar",
                    "2" = "Básica primaria",
                    "3" = "Básica secundaria",
                    "4" = "Media académica o clásica",
                    "5" = "Media técnica",
                    "6" = "Normalista",
                    "7" = "Superior y postgrado",
                    "8" = "Ninguno",
                    "9" = "No informa")) %>%
  mutate(eduLevel = factor(Education) %>%
           fct_recode("1" = "Preescolar",
                      "2" = "Básica primaria",
                      "2" = "Básica secundaria",
                      "3" = "Media académica o clásica",
                      "3" = "Media técnica",
                      "3" = "Normalista",
                      "4" = "Superior y postgrado",
                      "1" = "Ninguno",
                      "9" = "No informa"))

#Just testing that the Total is equal to the sum
#census2005  <- census2005 %>%
#mutate(Total2=Male+Female, val=Total==Total2)

#Only variables that I need
census2005  <- census2005 %>%
  select(-Total, -eduFactor, -Education)


#Groups of Ages definition 0-4, 5-9, ... , 85+
census2005  <- census2005 %>%
  mutate(
    # Create categories
    age_group = dplyr::case_when(
      Age < 5            ~ "0-4",
      Age >= 5 & Age < 10 ~ "5-9",
      Age >= 10 & Age < 15 ~ "10-14",
      Age >= 15 & Age < 20 ~ "15-19",
      Age >= 20 & Age < 25 ~ "20-24",
      Age >= 25 & Age < 30 ~ "25-29",
      Age >= 30 & Age < 35 ~ "30-34",
      Age >= 35 & Age < 40 ~ "35-39",
      Age >= 40 & Age < 45 ~ "40-44",
      Age >= 45 & Age < 50 ~ "45-49",
      Age >= 50 & Age < 55 ~ "50-54",
      Age >= 55 & Age < 60 ~ "55-59",
      Age >= 60 & Age < 65 ~ "60-64",
      Age >= 65 & Age < 70 ~ "65-69",
      Age >= 70 & Age < 75 ~ "70-74",
      Age >= 75 & Age < 80 ~ "75-79",
      Age >= 80 & Age < 85 ~ "80-84",
      Age >= 85  ~ "85+"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59","60-64","65-69","70-74","75-79","80-84","85+")
    )) %>%
  select(-Age) %>% #Drop no informa
  group_by(age_group,eduLevel)%>%
  summarise_all(funs(sum))


census2005  <- census2005 %>%
  pivot_longer(!c(age_group, eduLevel), names_to = "Sex", values_to = "Count") %>%
  pivot_wider(names_from = "eduLevel", values_from = "Count") %>%
  rename("edu1"="1", "edu2"="2", "edu3"="3", "edu4"="4", "No Given"="9") %>%
  replace(.,is.na(.),0) %>%
  select(-`No Given`) %>%
  arrange(Sex)

#Save information
saveRDS(census2005, "Data Created/Census2005_d.rds")





