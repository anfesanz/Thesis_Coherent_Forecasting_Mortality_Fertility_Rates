



# #Read the information
popedu_rec <- readRDS("Data Created/popedu_rec.rds")
popedu_rec_hf <- readRDS("Data Created/popedu_rec_hf.rds")
load("/Users/felipesanchez/Library/CloudStorage/Dropbox-Personal/UoM/UoM from MA/Mortality by Education/Data Created/MortalityColDebugged.RData")

deathedu <- as.tibble(Defun) %>%
  rename(death=dead) %>%
  mutate(agenew = case_when(
    age == 1 ~ "0-1",
    age == 2 ~ "1-4",
    age == 3 ~ "5-9",
    age == 4 ~ "10-14",
    age == 5 ~ "15-19",
    age == 6 ~ "20-24",
    age == 7 ~ "25-29",
    age == 8 ~ "30-34",
    age == 9 ~ "35-39",
    age == 10 ~ "40-44",
    age == 11 ~ "45-49",
    age == 12 ~ "50-54",
    age == 13 ~ "55-59",
    age == 14 ~ "60-64",
    age == 15 ~ "65-69",
    age == 16 ~ "70-74",
    age == 17 ~ "75+",
    TRUE ~ NA_character_
  )) %>%
  select(-age) %>%
  rename(age_group = agenew) %>%
  filter(year %in% c(1998, 2003, 2008, 2013, 2018)) %>%
  pivot_wider(names_from = edu, values_from = death, names_prefix = "deathedu") %>%
  filter(age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+"))  %>%
  mutate(age_group = as.character(age_group)) %>%
  mutate(age_group = factor(age_group))

#Mortality rates with population reconstruction
mredu <- popedu_rec %>%
  filter(age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79")) %>%
  mutate(age_group = as.character(age_group)) %>%
  mutate(age_group = ifelse(age_group == "75-79", "75+", age_group)) %>%
  mutate(age_group = factor(age_group)) %>%
  left_join(deathedu) %>%
  mutate(m1=deathedu1/edu1, m2=deathedu2/edu2, m3=deathedu3/edu3, m4=deathedu4/edu4) %>%
  select(-starts_with("edu"), -starts_with("death"))

#Mortality rates with population reconstruction Holt Filter
mredu_hf <- popedu_rec_hf %>%
  filter(age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79")) %>%
  mutate(age_group = as.character(age_group)) %>%
  mutate(age_group = ifelse(age_group == "75-79", "75+", age_group)) %>%
  mutate(age_group = factor(age_group)) %>%
  left_join(deathedu) %>%
  mutate(m1=deathedu1/edu1, m2=deathedu2/edu2, m3=deathedu3/edu3, m4=deathedu4/edu4) %>%
  select(-starts_with("edu"), -starts_with("death"))

#Saving mortality rates by level of education and sex
saveRDS(mredu, "Data Created/mredu.rds")
#Saving mortality rates by level of education and sex (Using the our methodology to reconstruct population)
saveRDS(mredu_hf, "Data Created/mredu_hf.rds")
