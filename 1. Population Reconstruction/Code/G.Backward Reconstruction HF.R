#Backward Reconstruction
########################
rm(list = ls(all = TRUE)) 
#Load user-defined functions
source("Code/PopRecHF/functions.R")
sredu <- readRDS("Data Created/sredu.rds")
sredu_hf <- readRDS("Data Created/sredu_hf.rds")
Census2018_100 <- readRDS("Data Created/Census2018_100.rds")


#Going Back

#Preparing the information from the census
reconstruction_ext <- Census2018_100  %>%  
  mutate(sex = recode(sex, 'Female' = 2, 'Male' = 1), year=2018) %>%
  filter(!(age_group %in% c("5-9", "10-14", "100+"))) %>%
  rowwise() %>% # Calculate weights by row, ensuring the sum of each row is 1
  mutate(across(starts_with("edu"), ~ ./sum(c_across(starts_with("edu"))))) # The backward reconstruction is over the percentages (edu weights)
#Saving the initial data
rec_hf <- reconstruction_ext

#Defining the variables in the iteration
##########################################
baseline_year = 2018
baseline_year_rec =1998

while (baseline_year > 1998) {
  
  #
  baseline_sur <- reconstruction_ext%>% 
  left_join(sredu_hf %>% filter(year == baseline_year))

  #Reconstruction
  ################
  # Subtract 5 from the baseline year
  baseline_year <- baseline_year - 5
  #
  reconstruction <- baseline_sur %>%
  group_by(sex) %>%
  mutate(age_group = lag(age_group)) %>%
  mutate(edu1=edu1/sr1, edu2=edu2/sr2, edu3=edu3/sr3, edu4=edu4/sr4, year=baseline_year) %>%
  filter(!is.na(age_group)) %>%
  select(-starts_with("sr")) %>%
  bind_rows(data.frame(sex = c(1, 2),
                       age_group = "95-99",
                       edu1 = NA,
                       edu2 = NA,
                       edu3 = NA,
                       edu4 = NA,
                       year = baseline_year))
  #Reconstruction Extrapolation  
  reconstruction_ext <- reconstruction %>%
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
      lpi3 = logit(pi3)) %>%
    select(year, sex, age_group, lpi1, lpi2, lpi3) %>%
    pivot_wider(names_from = sex, values_from = c(lpi1, lpi2, lpi3), names_sep = "_") %>%
    mutate(across(everything(), ~replace(., is.infinite(.), NA))) %>%
    mutate(reg_aux = case_when(age_group %in% c('15-19', '20-24', '25-29', '30-34', '35-39') ~ 1,
                             TRUE ~ 2))  %>%
    group_by(year, reg_aux) %>% # Create index to perfom the regression before 40s and after 40s
    mutate(age_index = row_number()) %>%
    pivot_longer(-c(year, age_group, age_index, reg_aux), names_to = "variable", values_to = "value") %>%#data to long format
    group_by(variable, year,reg_aux) %>%
    do({
    model = lm(value ~ age_index, data = ., na.action = na.exclude)
    value = ifelse(is.na(.$value), predict(model, newdata = .), .$value)
    data.frame(., value)
  }) %>%
  ungroup() %>%
  pivot_wider(-c(value, reg_aux, age_index), names_from = "variable", values_from = "value.1") %>%
  pivot_longer(-c(year, age_group), names_to = "variable", values_to = "value") %>%
  mutate(value=logistic(value)) %>% #Inverse transformation
  mutate(variable = str_sub(variable, 2)) %>%#Drop "l"
  separate(variable, into=c("var","sex"), sep="_", remove = TRUE) %>%
  pivot_wider(names_from = var, values_from = value) %>% # Be sure that pi1>pi2>pi3 after the interpolation.
  #mutate(aja = if_else(pi1 > pi2 & pi2 > pi3, "true_case_value", "false_case_value"))
  mutate(
    pi1a = pmax(pi1, pi2),
    pi2a = pmax(pi2, pi3),
    pi3a = pmin(pi2, pi3)
  ) %>%
  mutate(pi1=pi1a, pi2=pi2a, pi3=pi3a) %>%
  mutate(
    edu1 = 1-pi1,
    edu2 = 1-(edu1 + pi2),
    edu3 = 1-(edu1 + edu2 + pi3),  
    edu4 = pi3 
  )  %>% #pi to edu
  mutate(sex=as.numeric(sex), year=as.numeric(year)) %>%
  select(-c(starts_with("pi")))
  #Saving the reconstruction in year-5
  rec_hf <- rbind(rec_hf, reconstruction_ext)

}

saveRDS(rec_hf, "Data Created/rec_hf.rds")


