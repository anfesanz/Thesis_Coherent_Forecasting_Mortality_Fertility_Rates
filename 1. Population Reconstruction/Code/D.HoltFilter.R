#Extrapolate backward to include the backward Transition Ratio (bTR) in postsecondary education at age 15. Speringer(2019). Page 33
#popedu_15 is the logistic extrapolation at the age of 15 to estimate the bTR
################################################################################
# # Clear the workspace
 rm(list = ls(all = TRUE))
# # Set the working directory
 setwd("/Users/felipesanchez/Documents/UoM/PhD/Phd Second Year/PopEdu Reconstruction/")
# # Load user-defined functions
 source("Code/PopRecHF/functions.R")
# # Load the required library
 library(tidyverse)

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

#Save Population weighted 
saveRDS(popedu_w, "Data Created/popedu_w.rds")

# And now... My methodology!!
#Holt extrapolation. Weighted Reconstruction (wr)
popedu_wr <- readRDS("Data Created/popedu_w.rds") %>%
  filter(age_group %in% c('40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85-89', '90-94', '95-99'), year != 2018) %>%
  pivot_wider(names_from = year, values_from = matches("^lp")) %>%
  mutate(age_index = row_number()) %>% 
  mutate(across(starts_with("lpi"), ~ ifelse(age_index %in% c(10, 11, 12), forecast::forecast(forecast::holt(.), h = 3)$mean, .))) %>%
  select(-starts_with("ts"), -age_index) %>%
  pivot_longer(cols = -age_group, 
               names_to = c(".value", "year"), 
               names_pattern = "(.*)_(\\d{4})$") %>%
  mutate(year=as.numeric(year)) 
# Come back to the previous format
popedu_wr_aux <- readRDS("Data Created/popedu_wr_100.rds") %>%
  filter(!age_group %in% c('40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85-89', '90-94', '95-99') & year != 2018) %>%
  select(-value, -reg_aux, -age_index) %>%
  pivot_wider(names_from = variable, values_from = value.1)

popedu_wr_aux2 <- readRDS("Data Created/popedu_wr_100.rds") %>%
  filter(year == 2018) %>%
  select(-value, -reg_aux, -age_index) %>%
  pivot_wider(names_from = variable, values_from = value.1)

popedu_wr_hf <- bind_rows(popedu_wr, popedu_wr_aux)
popedu_wr_hf <- bind_rows(popedu_wr, popedu_wr_aux2)

rm(popedu_wr_aux, popedu_wr_aux2)

#Population weithed with the estimation
popedu_we_hf <- popedu_wr_hf %>%
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
popedu_15_100hf <- popedu %>%
  select(-starts_with("edu")) %>%
  left_join(popedu_we_hf) %>%
  mutate(edu1=as.integer(edu1*pop), edu2=as.integer(edu2*pop), edu3=as.integer(edu3*pop), edu4=as.integer(edu4*pop))





