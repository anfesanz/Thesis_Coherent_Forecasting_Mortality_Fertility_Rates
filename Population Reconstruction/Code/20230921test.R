
reconstruction_ext <- Census2018_100 %>%
  mutate(sex = recode(sex, 'Female' = 2, 'Male' = 1), year = 2018) %>%
  mutate(age_group = recode(age_group, "100+" = "100-104")) %>%
  filter(!(age_group %in% c("5-9", "10-14"))) %>%
  rowwise() %>%
  mutate(across(starts_with("edu"), ~ ./sum(c_across(starts_with("edu")))))



rename  age_group "100+" as "100-104"

reconstruction_ext <- Census2018_100  %>%  
  mutate(sex = recode(sex, 'Female' = 2, 'Male' = 1), year=2018) %>%
  filter(!(age_group %in% c("5-9", "10-14"))) %>%
  rowwise() %>% # Calculate weights by row, ensuring the sum of each row is 1
  mutate(across(starts_with("edu"), ~ ./sum(c_across(starts_with("edu"))))) 