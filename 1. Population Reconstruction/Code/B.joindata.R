

#Read Data and put it in the same format
popedu1998_d <- readRDS("Data Created/Survey1998_d.rds") %>%
  mutate(sex=as.numeric(sex), edu=as.numeric(edu), pop=as.numeric(pop)) %>% #Selecting ages above 15
  filter(str_detect(age_group, "^(1[5-9]|[2-9][0-9])")) %>%  #See R program to fill this missing values
  ungroup() #%>%
  #add_row(sex = 2, age_group = "80-84", edu = 3, pop = 500.5) %>%
  #add_row(sex = 2, age_group = "85+", edu = 4, pop = 850)
popedu2003_d <- readRDS("Data Created/Survey2003_d.rds")%>%
  mutate(sex=as.numeric(sex), edu=as.numeric(edu), pop=as.numeric(pop))%>%
  filter(str_detect(age_group, "^(1[5-9]|[2-9][0-9])")) #Selecting ages above 15
popedu2008_d <- readRDS("Data Created/Survey2008_d.rds")%>%
  mutate(sex=as.numeric(sex), edu=as.numeric(edu), pop=as.numeric(pop))%>%
  filter(str_detect(age_group, "^(1[5-9]|[2-9][0-9])")) #Selecting ages above 15
popedu2013_d <- readRDS("Data Created/Survey2013_d.rds") %>%
  mutate(sex=as.numeric(sex), edu=as.numeric(edu), pop=as.numeric(pop))%>%
  filter(str_detect(age_group, "^(1[5-9]|[2-9][0-9])")) #Selecting ages above 15
popedu2018_d <- readRDS("Data Created/Census2018_100.rds")  %>%
  pivot_longer(cols = starts_with("edu"), 
               names_to = "edu",
               values_to = "pop") %>%
  mutate(edu = str_remove(edu, "edu")) %>%
  mutate(sex = recode(sex, "Male" = 1, "Female" = 2)) %>%
  mutate(sex=as.numeric(sex), edu=as.numeric(edu), pop=as.numeric(pop)) %>%
  filter(str_detect(age_group, "^(1[5-9]|[2-9][0-9])"))

# join all the data of education 
popedu_w <- popedu2018_d %>%
  rename(pop18=pop) %>%
  left_join(popedu2013_d) %>%
  rename(pop13=pop) %>%
  left_join(popedu2008_d) %>%
  rename(pop08=pop) %>%
  left_join(popedu2003_d) %>%
  rename(pop03=pop) %>%
  left_join(popedu1998_d) %>%
  rename(pop98=pop) %>%
  mutate(across(-age_group, as.numeric)) %>% #replace all pops equal to 0 if age group is 15-19 and edu 4
  mutate(pop18 = if_else(age_group == "15-19" & edu == 4 , 0, pop18),
         pop13 = if_else(age_group == "15-19" & edu == 4 , 0, pop13),
         pop08 = if_else(age_group == "15-19" & edu == 4 , 0, pop08),
         pop03 = if_else(age_group == "15-19" & edu == 4 , 0, pop03),
         pop98 = if_else(age_group == "15-19" & edu == 4 , 0, pop98)) %>%
  mutate_at(vars(-group_cols()), ~replace(., is.na(.), 0)) %>%
  pivot_longer(cols = starts_with("pop"),
               names_to = "year",
               values_to = "pop") %>%
  mutate(year = case_when(
    year == "pop98" ~ "1998",
    year == "pop03" ~ "2003",
    year == "pop08" ~ "2008",
    year == "pop13" ~ "2013",
    year == "pop18" ~ "2018",
    TRUE ~ year)) %>%
  pivot_wider(names_from = edu, values_from = pop, names_prefix = "edu") %>%
  arrange(year, sex, age_group) %>% #Calculing the percentage by ege
  rowwise() %>%
  mutate(across(starts_with("edu"), ~ ./sum(c_across(starts_with("edu"))))) %>%
  mutate(across(-age_group, as.numeric))

#Data with only informatio of sex
popsex <- readRDS("Data Created/Population20181950_100.rds") %>%
  mutate(sex = recode(sex, "Male" = 1, "Female" = 2)) %>%
  filter(str_detect(age_group, "^(1[5-9]|[2-9][0-9])")) %>%#Selecting ages above 15
  filter(year %in% c(1998, 2003, 2008, 2013, 2018)) %>%
  arrange(year, sex, age_group)

#Pop by education and sex adjusted
popedu <- popedu_w %>%
  left_join(popsex) %>%
  mutate(edu1=as.integer(edu1*pop), edu2=as.integer(edu2*pop), edu3=as.integer(edu3*pop), edu4=as.integer(edu4*pop) )

