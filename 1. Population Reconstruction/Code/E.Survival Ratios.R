#Survival ratios
sredu <- popedu_15_100 %>%
  arrange(sex,year,age_group) %>%
  group_by(sex) %>%
  mutate(sr1 = edu1 / lag(edu1,n = 18), sr2 = edu2 / lag(edu2,n = 18), 
         sr3 = edu3 / lag(edu3,n = 18), sr4 = edu4 / lag(edu4,n = 18)) %>%
  # Replace NA values for age_group "15-19" with 0
  mutate_at(vars(starts_with("sr")), ~ifelse(age_group == "15-19", NA, .)) %>%
  select(year, sex, age_group, starts_with("sr"))


#Survival ratios
sredu_hf <- popedu_15_100hf %>%
  arrange(sex,year,age_group) %>%
  group_by(sex) %>%
  mutate(sr1 = edu1 / lag(edu1,n = 18), sr2 = edu2 / lag(edu2,n = 18), 
         sr3 = edu3 / lag(edu3,n = 18), sr4 = edu4 / lag(edu4,n = 18)) %>%
  # Replace NA values for age_group "15-19" with 0
  mutate_at(vars(starts_with("sr")), ~ifelse(age_group == "15-19", NA, .)) %>%
  select(year, sex, age_group, starts_with("sr"))
