################################################################################
# R Script Header
# 
# Created by: Felipe Sanchez
# Contact: anfesanz@gmail.com
# Last Update: July 2023
#
# Description: 
# Main file to accomplish the paper (population reconstruction by level of education and sex).
# Comparing the methodologies of Lutz and Speringer and our proposal using the Holt Filter.
#
# Input:
# - Original data from DANE, Surveys of 1998, 2003, 2008, 2013 and Census of 2018
# - Original mortality counts from DANE
#
# Output:
# - popedu_rec: Population Reconstruction using Lutz and Speringer methodology
# - popedu_rec_hf: Population Reconstruction under our methodology
################################################################################


# Clear the workspace
rm(list = ls(all = TRUE))  

# Set the working directory
getwd()
setwd("/Users/felipesanchez/Documents/UoM/PhD/Phd Second Year/PopEdu Reconstruction/")


# Load the required library
library(tidyverse)
library(haven)

# Load author-defined functions
source("Code/PopRecHF/functions.R")

#Surveys by education
#Load original data and clean the survey recoding by education
source("Code/PopRecHF/A.HHSurveysbyEdu.R")
# Inside are saved the surveys debbuged
# saveRDS(Survey1998, "Data Created/Survey1998_d.rds")
# saveRDS(Survey2003, "Data Created/Survey2003_d.rds")
# saveRDS(Survey2008, "Data Created/Survey2008_d.rds")
# saveRDS(Survey2013, "Data Created/Survey2013_d.rds")

# Logit extrapolation of open intervals in surveys and information of census until 100+
source("Code/PopRecHF/B.joindata.R")

#Saving the information
saveRDS(popsex, "Data Created/popsex.rds")
saveRDS(popedu,  "Data Created/popedu.rds")

# Graph Pop by edu

# Clean workspace and read data
rm(list = ls(all = TRUE)) 
#Read Data and put it in the same format
popsex <- readRDS("Data Created/popsex.rds")
popedu <- readRDS("Data Created/popedu.rds")

# Logistical extrapolation of the age of 15 (to calculate the bTR)and open intervals are converted into close intervals. 
# Output popedu_15
source("Code/PopRecHF/C.logisticextrapolation.R")

# OR

# Hodrick Filter
# Output popedu_15
source("Code/PopRecHF/D.HoltFilter.R")

# Save information
saveRDS(popedu_15_100,  "Data Created/popedu_15_100.rds")
saveRDS(popedu_15_100hf,  "Data Created/popedu_15_100hf.rds")

# Read Information
popedu_15_100 <- readRDS("Data Created/popedu_15_100.rds")
popedu_15_100hf <- readRDS("Data Created/popedu_15_100hf.rds")

# Compute survival ratios
source("Code/PopRecHF/E.Survival Ratios.R")

#Save information
saveRDS(sredu, "Data Created/sredu.rds")
saveRDS(sredu_hf, "Data Created/sredu_hf.rds")

# Backward Reconstruction (Lutz(2007) and Speringer(2019))
source("Code/PopRecHF/F.Backward Reconstruction.R")


# Backward Reconstruction Holt Filter
source("Code/PopRecHF/G.Backward Reconstruction HF.R")


# Clean workspace and load data for graphs
rm(list = ls(all = TRUE)) 
rec <- readRDS("Data Created/rec.rds")
rec_hf <- readRDS("Data Created/rec_hf.rds")
popedu <- readRDS("Data Created/popedu.rds")

# Prepare data for graphs
source("Code/PopRecHF/H.PrepareDataForGraphs.R")
# Save information
# Lutz reconstruction
saveRDS(popedu_rec, "Data Created/popedu_rec.rds")
# This paper proposal
saveRDS(popedu_rec_hf, "Data Created/popedu_rec_hf.rds")

# Graphs
########
#Population by sex (DANE Censuses 2018 and 2005)
source("Code/PopRecHF/I.GraphCensuses.R")

#Population by sex (DANE Reconstruction)
source("Code/PopRecHF/J.GraphDaneRecSex19502019.R")

#Spatial Violence and Education
source("Code/PopRecHF/K.GraphViolenceEdu.R")

# Reconstruction graphs
source("Code/PopRecHF/L.GraphsPopRecEdu.R")

# Mortality Rates Graphs

#Calculating the mortality rates using the Population Reconstruction
source("Code/PopRecHF/M.MortalityRates.R")

#HeatMap for mortality rates by sex and education
source("Code/PopRecHF/N.GraphsMortalityRates.R")




