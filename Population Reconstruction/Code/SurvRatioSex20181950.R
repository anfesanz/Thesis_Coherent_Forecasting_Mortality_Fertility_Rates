################################################################################
# R Script Header
# 
# Created by: Felipe Sánchez
# Contact: anfesanz@gmail.com
# Date: Feb, May, Jul 2023
#
# Description:
# Survivorship is calculated by sex and applied to all levels of education.
# The survival ratio between two ages is calculated by dividing the population at the later age by the population at the earlier age.
# This approach follows Speringer (2019) on page 22. For example, between 2013 and 2018, the interval for the age group "100+" is provided by DANE.
# The formula used is: years = n_{t-5,x-5} / n_{t,x}, where n_{t-5,x-5} represents the population 5 years earlier and n_{t,x} represents the population in the present year and age group.
# This calculation assumes constant migration and accounts for mortality in the cohort.
#
# Input:
# Database from the Colombian Statistical Office in Colombia (DANE) with information on the population reconstruction by year and simple age.
# Years to perform the reconstruction.
#
# Output:
# Object "PopAndSurvival"
# With "Population20181950_100" With population by sex and age group (max each group 100+) between 1950 and 2018 of each year
# With "Population20181950_85" With population by sex and age group (max each group 85+) between 1950 and 2018 of each year
# With "SurvivalRSex20181993" "sr_df" with debugged information of the population of each 5 years of the reconstruction by sex and age group and the Survival Ratio calculated by cohort.
#
################################################################################

rm(list = ls(all = TRUE))  # Clear the workspace

# Working directory
getwd()

#setwd("/Users/felipesanchez/Library/CloudStorage/Dropbox-Personal/UoM/UoM from MA/Mortality by Education/")
setwd("/Users/felipesanchez/Documents/GitHub/PopEdu Reconstruction")

#Call packages
library(readxl)
library(httr)
library(tidyverse)

# URL of the Excel file
url <- "https://www.dane.gov.co/files/censo2018/proyecciones-de-poblacion/Nacional/DCD-area-sexo-edad-proypoblacion-Nac-1950-2019.xlsx"

# Temporary file to store the downloaded Excel file
temp_file <- tempfile(fileext = ".xlsx")
# Download the file
download.file(url, temp_file, mode="wb")
# Read the Excel file into R (specify the sheet if there are multiple)
data <- read_excel(temp_file, skip = 11, sheet = 1)
# View the data
head(data)

#Years to do the reconstruction
rec_years <- c(2018, 2013, 2008, 2003, 1998)


df <- data %>%
  rename(year=`AÑO`)  %>%
  filter(`ÁREA GEOGRÁFICA` == "Total") %>%
  select(-c(`DP`, `DPNOM`, `ÁREA GEOGRÁFICA`, cols = starts_with("Total"))) %>%
  pivot_longer(cols = starts_with("Hombres_") | starts_with("Mujeres_"),
               names_to = "age_gender",
               values_to = "pop") %>%
  mutate(age = as.numeric(gsub("Hombres_|Mujeres_", "", age_gender)),
         sex = ifelse(str_detect(age_gender, "^Hombres_"), "Male", "Female")) %>%
  select(year, age, sex, pop) %>%
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
                                           "60-64", "65-69", "70-74", "75-79", "80-84", "85-89",
                                           "90-94", "95-99", "100+" ))) %>%
  group_by(year, sex, age_group) %>%
  summarize(across(where(is.numeric), sum))%>%
  select(-age)


#
df2 <- data %>%
rename(year=`AÑO`)  %>%
  filter(`ÁREA GEOGRÁFICA` == "Total") %>%
  select(-c(`DP`, `DPNOM`, `ÁREA GEOGRÁFICA`, cols = starts_with("Total"))) %>%
  pivot_longer(cols = starts_with("Hombres_") | starts_with("Mujeres_"),
               names_to = "age_gender",
               values_to = "pop") %>%
  mutate(age = as.numeric(gsub("Hombres_|Mujeres_", "", age_gender)),
         sex = ifelse(str_detect(age_gender, "^Hombres_"), "Male", "Female")) %>%
  select(year, age, sex, pop) %>%
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
    age >= 85  ~ "85+"),
    # Convert to factor
    age_group = factor(age_group,level = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                           "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                           "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))) %>%
  group_by(year, sex, age_group) %>%
  summarize(across(where(is.numeric), sum))%>%
  select(-age)


sr_df <- df  %>%
  filter(year %in% rec_years)%>%
  arrange(sex, year, age_group) %>%
  group_by(sex) %>%
  mutate(Survival_Ratio = pop / lag(pop,n = 22)) %>%
  mutate(Survival_Ratio = ifelse(age_group == "0-4", NA, Survival_Ratio)) #First group to missing (replace by "15-19")


Population20181950_85 <- df2
Population20181950_100 <- df
SurvivalRSex20181993 <- sr_df


PopAndSurvival <- tibble(
  id = c("Population20181950_100","Population20181950_85", "SurvivalRSex20181993"),
  data = list(Population20181950_100, Population20181950_85, SurvivalRSex20181993)
)


#Save information
saveRDS(PopAndSurvival, "Data Created/PopAndSurvival20181993.rds")
saveRDS(Population20181950_100, "Data Created/Population20181950_100.rds")
saveRDS(Population20181950_85, "Data Created/Population20181950_85.rds")
