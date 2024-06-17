#Extrapolate backward to include the backward Transition Ratio (bTR) in postsecondary education at age 15. Speringer(2019). Page 33
#popedu_15 is the logistic extrapolation at the age of 15 to estimate the bTR
################################################################################
# # Clear the workspace
# rm(list = ls(all = TRUE))  
# # Set the working directory
# setwd("/Users/felipesanchez/Documents/GitHub/PopEdu Reconstruction/")
# # Load user-defined functions
# source("Code/functions.R")
# # Load the required library
# library(tidyverse)

#Read Data and put it in the same format
popedu <- readRDS("Data Created/popedu.rds") 

#Calculate weights
popedu_w <- popedu %>%
  filter(str_detect(age_group, "^(1[5-9]|[2-9][0-9])")) %>% # Excluding ages bellow 15
  rowwise() %>%
  mutate(across(starts_with("edu"), ~ ./sum(c_across(starts_with("edu"))))) %>%# Calculate weights by row, ensuring the sum of each row is 1
  mutate(
    pi1 = edu2 + edu3 + edu4,
    pi2 = edu3 + edu4,
    pi3 = edu4) %>%
  mutate(
    lpi1 = logit(pi1),
    lpi2 = logit(pi2),
    lpi3 = logit(pi3)
  ) %>%
  select(year, sex, age_group, lpi1, lpi2, lpi3) %>%
  pivot_wider(names_from = sex, values_from = c(lpi1, lpi2, lpi3), names_sep = "_") %>%
  mutate(across(everything(), ~replace(., is.infinite(.), NA)))

#Performing the regression to estimate NAs (edu4 at age 15)
popedu_wr <- popedu_w %>%
  group_by(year)  %>% #reg_aux help me to do regression in educational ages (under 40 years) and off of educational ages (above 40 years)
  mutate(reg_aux = case_when(age_group %in% c('15-19', '20-24', '25-29', '30-34', '35-39') ~ 1,
    TRUE ~ 2
  ))  %>%
  group_by(year, reg_aux) %>% # Create index to perfom the regression before 40s and after 40s
  mutate(age_index = row_number()) %>%
  pivot_longer(-c(year, age_group, age_index, reg_aux), names_to = "variable", values_to = "value") %>%#data to long format
  group_by(year, reg_aux, variable) %>%
  do({
    model = lm(value ~ age_index, data = ., na.action = na.exclude)
    value = ifelse(is.na(.$value), predict(model, newdata = .), .$value)
    data.frame(., value)
  }) %>%
  ungroup()


#Population weithed with the estimation
popedu_we <- popedu_wr %>%
  pivot_wider(-c(value, reg_aux, age_index), names_from = "variable", values_from = "value.1") %>%
  pivot_longer(-c(year, age_group), names_to = "variable", values_to = "value") %>%
  mutate(value=logistic(value)) %>% #Inverse transformation
  mutate(variable = str_sub(variable, 2)) %>%#Drop "l"
  separate(variable, into=c("var","sex"), sep="_", remove = TRUE) %>%
  pivot_wider(names_from = var, values_from = value) %>%
  mutate(
    edu4 = pi3,
    edu3 = pi2-edu4,
    edu2 = pi1-edu3-edu4,
    edu1 = 1-pi1
  )  %>% #pi to edu
  mutate(sex=as.numeric(sex), year=as.numeric(year)) %>%
  select(-c(pi1, pi2, pi3))


#Convert weigths into population
popedu_15 <- popedu %>%
  select(-starts_with("edu")) %>%
  left_join(popedu_we) %>%
  mutate(edu1=as.integer(edu1*pop), edu2=as.integer(edu2*pop), edu3=as.integer(edu3*pop), edu4=as.integer(edu4*pop))





