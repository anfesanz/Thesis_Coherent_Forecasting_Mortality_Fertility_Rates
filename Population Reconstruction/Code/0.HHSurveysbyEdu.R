################################################################################
# R Script Header
# 
# Created by: Felipe Sanchez
# Contact: anfesanz@gmail.com
# Date: July 2023
#
# Description: This script processes from different surveys available from years 1998, 2003, 2008 and 2013. Data cleaning and recodification of education and age
#The code for 1998, 2003, 2008 and 2013 reads and cleans data from the "col98.sav" file. It selects specific columns, renames them, and creates new variables. It categorizes age into groups and recodes the education variable. The data is filtered to remove missing values, grouped, and summarized. The final summarized data is saved as "Survey1998_d.rds".
#
# Input: Household/col98.sav, Household/col03_ecv.dta, Household/col08_ecv.dta. For 2013 Caracter°sticas y composici¢n del hogar.sav and Educaci¢n.sav
#
# Outputs:
# Cleaned data by level of education for 1998 in Data Created/Survey1998_d.rds
# Cleaned data by level of education for 2003 in Data Created/Survey2003_d.rds
# Cleaned data by level of education for 2008 in Data Created/Survey2008_d.rds
# Cleaned data by level of education for 2013 in Data Created/Survey2013_d.rds
################################################################################

# Clear the workspace
rm(list = ls(all = TRUE))  

# Set the working directory
getwd()
setwd("/Users/felipesanchez/Documents/GitHub/PopEdu Reconstruction/")


#install.packages("haven")
library(haven)
library(tidyverse)


################################################################################
# 1998
################################################################################

#Read data
col98 <- read_sav("Data Original/Household/col98.sav")

col98 <- as_tibble(col98)

#Data Clean 
#Sexo
#####
#1: Hombre
#2: Mujer
# NIVEDUC	NIVEL EDUCATIVO (Pregunta 12: ¿Cuál es el nivel educativo más alto alcanzado y el último año aprobado en ese nivel?)
# ####################################
# 0: No aplicable (menores de 5 años)
# 100: Ninguno
# 200: Preescolar
# 201: Preescolar
# 300: Primaria (asistiendo)
# 301: Primaria 1 año
# 302: Primaria 2 años
# 303: Primaria 3 años
# 304: Primaria 4 años
# 305: Primaria 5 años
# 400: Secundaria 0 año (asistiendo)
# 406: Secundaria 1 año
# 407: Secundaria 2 años
# 408: Secundaria 3 años
# 409: Secundaria 4 años
# 410: Secundaria 5 años
# 411: Secundaria 6 años
# 500: Superior o universitaria 0 año (asistiendo)
# 501: Superior o universitaria 1 año
# 502: Superior o universitaria 2 años
# 503: Superior o universitaria 3 años
# 504: Superior o universitaria 4 años
# 505: Superior o universitaria 5 años
# 506: Superior o universitaria 6 años
# 507: Superior o universitaria 7 años
# 508: Superior o universitaria 8 años
# 509: Superior o universitaria 9 años
# 999: No informa

Survey1998 <- col98 %>%
  select(DEPTO, MUNICIPI, SEXO, EDAD, ESTCIVI, NIVEDUC,FACTOREX) %>%
  rename(sex=SEXO, age=EDAD, edu=NIVEDUC) %>%
  mutate(
    # Create categories
    age_group = dplyr::case_when(
      age < 15             ~ "0-14",
      age >= 15 & age < 20 ~ "15-19",
      age >= 20 & age < 25 ~ "20-24",
      age >= 25 & age < 30 ~ "25-29",
      age >= 30 & age < 35 ~ "30-34",
      age >= 35 & age < 40 ~ "35-39",
      age >= 40 & age < 45 ~ "40-44",
      age >= 45 & age < 50 ~ "45-49",
      age >= 50 & age < 55 ~ "50-54",
      age >= 55 & age < 60 ~ "55-59",
      age >= 60 & age < 65 ~ "60-64",
      age >= 65 & age < 70 ~ "65-69",
      age >= 70 & age < 75 ~ "70-74",
      age >= 75 & age < 80 ~ "75-79",
      age >= 80 & age < 85 ~ "80-84",
      age >= 85  ~ "85+"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("0-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59","60-64","65-69","70-74","75-79","80-84","85+")
    )) %>%
  select(-age) %>%
  mutate(edu = recode(edu,
                      `0` = 99,
                      `100` = 1,
                      `200` = 1,
                      `201` = 1,
                      `300` = 1,
                      `301` = 1,
                      `302` = 1,
                      `303` = 1,
                      `304` = 1,
                      `305` = 2,
                      `400` = 2,
                      `406` = 2,
                      `407` = 2,
                      `408` = 2,
                      `409` = 2,
                      `410` = 2,
                      `411` = 3,
                      `500` = 3,
                      `501` = 3,
                      `502` = 3,
                      `503` = 3,
                      `504` = 4,
                      `505` = 4,
                      `506` = 4,
                      `507` = 4,
                      `508` = 4,
                      `509` = 4,
                      `999` = 99)) %>%
  mutate(edu = ifelse(edu == 99, NA_integer_, edu)) %>%
  filter(!is.na(edu)) %>%
  group_by(sex, age_group, edu) %>%
  summarize(pop = sum(FACTOREX, na.rm = TRUE))

#Save information
 saveRDS(Survey1998, "Data Created/Survey1998_d.rds")


################################################################################
# 2003
################################################################################
# Clear the workspace
rm(list = ls(all = TRUE))

# Import the Stata data file
col03 <- read_dta("Data Original/Household/col03_ecv.dta")

col03 <- as_tibble(col03)

#Data Clean 
Survey2003 <- col03 %>%
  select(i0401, i0402, fex, region, e02, e03) %>%
  rename(sex=e03, age=e02, edu=i0401, educ=i0402) %>%
  mutate(age=as.numeric(age)) %>%
  mutate(
    # Create categories
    age_group = dplyr::case_when(
      age < 15             ~ "0-14",
      age >= 15 & age < 20 ~ "15-19",
      age >= 20 & age < 25 ~ "20-24",
      age >= 25 & age < 30 ~ "25-29",
      age >= 30 & age < 35 ~ "30-34",
      age >= 35 & age < 40 ~ "35-39",
      age >= 40 & age < 45 ~ "40-44",
      age >= 45 & age < 50 ~ "45-49",
      age >= 50 & age < 55 ~ "50-54",
      age >= 55 & age < 60 ~ "55-59",
      age >= 60 & age < 65 ~ "60-64",
      age >= 65 & age < 70 ~ "65-69",
      age >= 70 & age < 75 ~ "70-74",
      age >= 75 & age < 80 ~ "75-79",
      age >= 80 & age < 85 ~ "80-84",
      age >= 85  ~ "85+"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("0-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59","60-64","65-69","70-74","75-79","80-84","85+")
    )) %>%
  select(-age) %>%
  mutate(edulevel = recode(edu,
                           `1` = 1,
                           `2` = 1,
                           `3` = 2,
                           `4` = 3,
                           `5` = 3,
                           `6` = 3,
                           `7` = 4,
                           `8` = 4,
                           `9` = 4)) %>%
  mutate(edulevel = case_when(
    edu == 3 & educ == "05" ~ 2,
    edu == 4 & educ == "13" ~ 3,
    TRUE ~ edulevel
  )) %>%
  select(sex, age_group, edulevel, fex) %>%
  rename(edu=edulevel) %>%
  filter(!is.na(edu)) %>%
  group_by(sex, age_group, edu) %>%
  summarize(pop = sum(fex, na.rm = TRUE))

#Save information
saveRDS(Survey2003, "Data Created/Survey2003_d.rds")


################################################################################
# 2008
################################################################################
# Clear the workspace
rm(list = ls(all = TRUE))

# Import the Stata data file
col08 <- read_dta("Data Original/Household/col08_ecv.dta")

col08 <- as_tibble(col08)


#Data Clean 
#PERSONAS P6020 NUMÉRICO 10 ¿Es hombre o mujer?
#PERSONAS P6040 NUMÉRICO 3 ¿Cuántos años cumplidos tiene...? (Si es menor de 1 año, escriba
# PERSONAS P6219 NUMÉRICO 10 ¿Cuál es el nivel educativo más alto alcanzado por ... y el último año
# PERSONAS P6219S1 NUMÉRICO 2 ¿Cuál es el nivel educativo más alto alcanzado por ... y el último año

Survey2008 <- col08 %>%
  select(factor_expansion, p6020, p6040, p6219) %>%
  rename(sex=p6020, age=p6040, edu=p6219)  %>%
  mutate(age=as.numeric(age)) %>%
  mutate(
    # Create categories
    age_group = dplyr::case_when(
      age < 15             ~ "0-14",
      age >= 15 & age < 20 ~ "15-19",
      age >= 20 & age < 25 ~ "20-24",
      age >= 25 & age < 30 ~ "25-29",
      age >= 30 & age < 35 ~ "30-34",
      age >= 35 & age < 40 ~ "35-39",
      age >= 40 & age < 45 ~ "40-44",
      age >= 45 & age < 50 ~ "45-49",
      age >= 50 & age < 55 ~ "50-54",
      age >= 55 & age < 60 ~ "55-59",
      age >= 60 & age < 65 ~ "60-64",
      age >= 65 & age < 70 ~ "65-69",
      age >= 70 & age < 75 ~ "70-74",
      age >= 75 & age < 80 ~ "75-79",
      age >= 80 & age < 85 ~ "80-84",
      age >= 85  ~ "85+"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("0-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59","60-64","65-69","70-74","75-79","80-84","85+")
    )) %>%
  select(-age) %>%
  mutate(edulevel = recode(edu,
                           "1" = 1,
                           "2" = 1,
                           "3" = 2,
                           "4" = 3,
                           "5" = 3,
                           "6" = 3,
                           "7" = 3,
                           "8" = 4,
                           "9" = 4,
                           "10" = 4))%>%
  select(sex, age_group, edulevel, factor_expansion) %>%
  rename(edu=edulevel) %>%
  filter(!is.na(edu)) %>%
  group_by(sex, age_group, edu) %>%
  summarize(pop = sum(factor_expansion, na.rm = TRUE))

#Save information
saveRDS(Survey2008, "Data Created/Survey2008_d.rds")


################################################################################
# 2013
################################################################################
# Clear the workspace
rm(list = ls(all = TRUE))

#Vivienda
#col13viv <- read_sav("/Users/felipesanchez/Downloads/ECV2013/1.Datos de la vivienda/Datos de la vivienda.sav") #20878
#Household
#col13hog_s <- read_sav("/Users/felipesanchez/Downloads/ECV2013/2.Servicios del hogar/Servicios del Hogar.sav") #21565
#col13hog_t <- read_sav("/Users/felipesanchez/Downloads/ECV2013/3.Tenencia y financiacion de la vivienda que ocupa el hogar/Tenencia y financiaci¢n de la vivienda que ocupa el hogar.sav") #21565
#col13hog_c <- read_sav("/Users/felipesanchez/Downloads/ECV2013/4.Condiciones de vida del hogar y tendencia de bienes/Condiciones de vida del hogar y tenencia de bienes.sav") #21565
#Individuals
col13ind_c <- read_sav("/Users/felipesanchez/Documents/GitHub/PopEdu Reconstruction/Data Original/Household/ECV2013/5.Caracteristicas y composicion del hogar/Caracter°sticas y composici¢n del hogar.sav") #73155
#col13ind_s <- read_sav("/Users/felipesanchez/Downloads/ECV2013/6.Salud/Salud.sav") #73155
#col13ind_a <- read_sav("/Users/felipesanchez/Downloads/ECV2013/7.Atencion integral de los ninos y ninas menores de 5 anos/Atenci¢n integral de los ni§os y ni§as menores de 5 a§os.sav") #5736
col13ind_e <- read_sav("/Users/felipesanchez/Documents/GitHub/PopEdu Reconstruction/Data Original/Household/ECV2013/8.Educacion/Educaci¢n.sav") #67419

#Exp Factors
col13ind_fe <- read_sav("/Users/felipesanchez/Documents/GitHub/PopEdu Reconstruction/Data Original/Household/ECV2013/Factores de expansion 2013 basados en el CNPV 2018/FACTORES_ECV_2013_CNPV2018.sav")#20787

#Key variables 
# Directorio
# Nro_encuesta
# Secuencia_encuesta
# Secuencia_p
# Orden

#P5857s1, P6211 or P1088 P1088s1 P6216
#P8587 Nivel mas alto (G.4 in the question form)#¿Cuál es el nivel educativo más alto alcanzado por… y el último año o grado aprobado en este nivel?
#Si es menor de 18 años pase a pregunta 18 (P783), de lo contrario termine capítulo
# Ninguno 1
# Preescolar 2
# Básica primaria (1.°-5.°) 3
# Básica secundaria (6.° a 9.°) 4
# Media (10.°-13.°) 5
# Técnico sin título 6
# Técnico con título 7
# Tecnológico sin título 8
# Tecnológico con título 9
# Universitario sin título 10
# Universitario con título 11
# Posgrado sin título 12
# Postgrado con título 13
#P783 ¿Cuál es el nivel educativo de esta persona? P781s1, P782

Survey2013  <- col13ind_c %>%
  left_join(col13ind_e, by = c("Directorio", "Nro_encuesta", "Secuencia_encuesta", "Secuencia_p", "Orden"))  %>%#73155
  select(P6020, P6040, P8587, P783, Fex_c.x) %>%
  rename(sex=P6020, age=P6040, edu_a=P8587, edu_b=P783)  %>%
  mutate(age=as.numeric(age)) %>%
  mutate(
    # Create categories
    age_group = dplyr::case_when(
      age < 15             ~ "0-14",
      age >= 15 & age < 20 ~ "15-19",
      age >= 20 & age < 25 ~ "20-24",
      age >= 25 & age < 30 ~ "25-29",
      age >= 30 & age < 35 ~ "30-34",
      age >= 35 & age < 40 ~ "35-39",
      age >= 40 & age < 45 ~ "40-44",
      age >= 45 & age < 50 ~ "45-49",
      age >= 50 & age < 55 ~ "50-54",
      age >= 55 & age < 60 ~ "55-59",
      age >= 60 & age < 65 ~ "60-64",
      age >= 65 & age < 70 ~ "65-69",
      age >= 70 & age < 75 ~ "70-74",
      age >= 75 & age < 80 ~ "75-79",
      age >= 80 & age < 85 ~ "80-84",
      age >= 85  ~ "85+"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("0-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59","60-64","65-69","70-74","75-79","80-84","85+")
    )) %>%
  select(-age) %>%
  mutate(edu_a=as.numeric(edu_a), edu_b=as.numeric(edu_b)) %>%
  mutate(edu = recode(edu_a, `1` = 1, `2` = 1, `3` = 2, `4` = 2, `5` = 3, `6` = 3, `7` = 3, 
                      `8` = 3, `9` = 3, `10` = 3, `11` = 4, `12` = 4, `13` = 4)) %>%
  mutate(edu = case_when(
    is.na(edu) & edu_b == 1 ~ 1,
    is.na(edu) & edu_b == 2 ~ 2,
    is.na(edu) & edu_b == 3 ~ 2,
    is.na(edu) & edu_b == 4 ~ 3,
    is.na(edu) & edu_b == 5 ~ 3,
    is.na(edu) & edu_b == 6 ~ 3,
    is.na(edu) & edu_b == 7 ~ 3,
    is.na(edu) & edu_b == 8 ~ 4,
    is.na(edu) & edu_b == 9 ~ 1,
    TRUE ~ edu
  )) %>%
  select(sex, age_group, edu, Fex_c.x)  %>%
  filter(!is.na(edu)) %>%
  group_by(sex, age_group, edu) %>%
  summarize(pop = sum(Fex_c.x, na.rm = TRUE))

#Save information
 saveRDS(Survey2013, "Data Created/Survey2013_d.rds")
