################################################################################
# R Script Header
# 
# Created by: Felipe Sanchez
# Contact: anfesanz@gmail.com
# Date: July 2023
#
# Description: 
# The purpose of this script is to calculate the survival ratios by level of education and sex, which will be used in the reconstruction process.
#
# Input:
# - Surveys cleaned by education and sex: Survey1998_d, Survey2003_d, Survey2008_d, Survey2013_d, and Census2018
#
# Output:
# - Data Created/sredu.rds (Survival ratios by education and sex)
#
################################################################################

# Clear the workspace
rm(list = ls(all = TRUE))  

# Set the working directory
getwd()
setwd("/Users/felipesanchez/Documents/GitHub/PopEdu Reconstruction/")


# Load the required library
library(tidyverse)

#Load user-defined functions
source("Code/functions.R")

#Logistic extrapolation of open intervals in surveys and information of census until 100+
source("Code/joinpopedusex2.R")
#Saving the information
saveRDS(popsex, "Data Created/popsex.rds")
saveRDS(popedu,  "Data Created/popedu.rds")


#Logistic extrapolation of the age of 15 to calculate the bTR
#output popedu_15
source("Code/extrapolate15.R")
#Save information
saveRDS(popedu_15,  "Data Created/popedu_15.rds")

#Logistic extrapolation of the age of 15 to calculate the bTR
#output popedu_15
source("Code/extrapolate100.R")
#Save information
saveRDS(popedu_15_100,  "Data Created/popedu_15_100.rds")




#Survival ratios
sredu <- popedu_15_100 %>%
  arrange(sex, year, age_group) %>%
  group_by(sex) %>%
  mutate(sr1 = edu1 / lag(edu1,n = 18), sr2 = edu2 / lag(edu2,n = 18), sr3 = edu3 / lag(edu3,n = 18), sr4 = edu4 / lag(edu4,n = 18)) %>%
  mutate(across(everything(), ~replace(., is.infinite(.), 0))) %>% 
  # Replace NA values for age_group "15-19" with 0
  mutate(sr1 = ifelse(age_group == "15-19" & year != 1998, 0, sr1),
         sr2 = ifelse(age_group == "15-19" & year != 1998, 0, sr2),
         sr3 = ifelse(age_group == "15-19" & year != 1998, 0, sr3),
         sr4 = ifelse(age_group == "15-19" & year != 1998, 0, sr4)) %>%
  select(year, sex, age_group, starts_with("sr"))





#Save information
saveRDS(sredu, "Data Created/sredu.rds")



