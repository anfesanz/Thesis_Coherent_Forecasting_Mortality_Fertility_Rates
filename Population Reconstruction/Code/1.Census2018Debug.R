

# Clear the workspace
rm(list = ls(all = TRUE))  

# Set the working directory
getwd()
setwd("/Users/felipesanchez/Documents/GitHub/PopEdu Reconstruction/")

# Load libraries
library(tidyverse)
library(readxl)

# Load user-defined functions
source("Code/functions.R")

#This code is processing a original dataset from REDATAM by renaming columns,
#handling missing values, creating new columns, 
#categorizing age into groups, and summarizing the data by sex and age group.

Census2018 <-read_excel("Data Original/reporte2018.xlsx", skip = 12)

#Create categories of education by sex
Census2018_85 <- Census2018 %>%
  select(-`...1`) %>%
  rename(age="...3",sex="Hombre") %>%
  mutate(sex = replace(sex, 1, "Hombre")) %>%
  fill(sex) %>%
  mutate(sex = case_when(
    sex == "Mujer" ~ "Male",
    sex == "Hombre" ~ "Female")) %>%
  filter(!is.na(sex)) %>%
  mutate(across(-c("sex"), as.numeric)) %>%
  filter(!is.na(age)) %>%
  mutate_all(~ifelse(is.na(.), 0, .)) %>%
  mutate(edu1= `Ninguno` +`Preescolar`,
         edu2= `Básica primaria`+`Básica secundaria`,
         edu3= `Media academica o clasica`+`Media tecnica`+`Normalista`, 
         edu4=`Técnica profesional o Tecnológica`+`Universitario`+`Especialización, maestría, doctorado`) %>%
  #mutate(Totaltest=edu1+edu2+edu3+edu4+`No informa`) #To test that the total of each level of education is equal to the total reporteds
  # Create categories
  mutate(age_group = case_when(
    age <= 4 ~ "0-4",
    age >= 5 & age <= 9 ~ "5-9",
    age >= 10 & age <= 14 ~ "10-14",
    age >= 15 & age <= 19 ~ "15-19",
    age >= 20 & age <= 24 ~ "20-24",
    age >= 25 & age <= 29 ~ "25-29",
    age >= 30 & age <= 34 ~ "30-34",
    age >= 35 & age <= 39 ~ "35-39",
    age >= 40 & age <= 44 ~ "40-44",
    age >= 45 & age <= 49 ~ "45-49",
    age >= 50 & age <= 54 ~ "50-54",
    age >= 55 & age <= 59 ~ "55-59",
    age >= 60 & age <= 64 ~ "60-64",
    age >= 65 & age <= 69 ~ "65-69",
    age >= 70 & age <= 74 ~ "70-74",
    age >= 75 & age <= 79 ~ "75-79",
    age >= 80 & age <= 84 ~ "80-84",
    age >= 85 ~ "85+"),
    # Convert to factor
    age_group = factor(age_group,level = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                           "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                           "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
    )
  ) %>%
  select(sex, age_group, edu1, edu2, edu3, edu4) %>%
  group_by(sex, age_group) %>%
  summarize(across(where(is.numeric), sum))


Census2018_100 <- Census2018 %>%
  select(-`...1`) %>%
  rename(age="...3",sex="Hombre") %>%
  mutate(sex = replace(sex, 1, "Hombre")) %>%
  fill(sex) %>%
  mutate(sex = case_when(
    sex == "Mujer" ~ "Male",
    sex == "Hombre" ~ "Female")) %>%
  filter(!is.na(sex)) %>%
  mutate(across(-c("sex"), as.numeric)) %>%
  filter(!is.na(age)) %>%
  mutate_all(~ifelse(is.na(.), 0, .)) %>%
  mutate(edu1= `Ninguno` +`Preescolar`,
         edu2= `Básica primaria`+`Básica secundaria`,
         edu3= `Media academica o clasica`+`Media tecnica`+`Normalista`, 
         edu4=`Técnica profesional o Tecnológica`+`Universitario`+`Especialización, maestría, doctorado`) %>%
  #mutate(Totaltest=edu1+edu2+edu3+edu4+`No informa`) #To test that the total of each level of education is equal to the total reporteds
  # Create categories
  mutate(age_group = case_when(
    age <= 4 ~ "0-4",
    age >= 5 & age <= 9 ~ "5-9",
    age >= 10 & age <= 14 ~ "10-14",
    age >= 15 & age <= 19 ~ "15-19",
    age >= 20 & age <= 24 ~ "20-24",
    age >= 25 & age <= 29 ~ "25-29",
    age >= 30 & age <= 34 ~ "30-34",
    age >= 35 & age <= 39 ~ "35-39",
    age >= 40 & age <= 44 ~ "40-44",
    age >= 45 & age <= 49 ~ "45-49",
    age >= 50 & age <= 54 ~ "50-54",
    age >= 55 & age <= 59 ~ "55-59",
    age >= 60 & age <= 64 ~ "60-64",
    age >= 65 & age <= 69 ~ "65-69",
    age >= 70 & age <= 74 ~ "70-74",
    age >= 75 & age <= 79 ~ "75-79",
    age >= 80 & age <= 84 ~ "80-84",
    age >= 85 & age <= 89 ~ "85-89",
    age >= 90 & age <= 94 ~ "90-94",
    age >= 95 & age <= 99 ~ "95-99",
    age >= 100 ~ "100+"),
    # Convert to factor
    age_group = factor(age_group,level = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                           "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                           "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99","100+")
    )
  ) %>%
  select(sex, age_group, edu1, edu2, edu3, edu4) %>%
  group_by(sex, age_group) %>%
  summarize(across(where(is.numeric), sum))


#Save information
saveRDS(Census2018_85, "Data Created/Census2018_85.rds")
saveRDS(Census2018_100, "Data Created/Census2018_100.rds")

