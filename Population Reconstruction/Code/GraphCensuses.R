rm(list = ls(all = TRUE))  # Clear the workspace
setwd("/Users/felipesanchez/Documents/GitHub/PopEdu Reconstruction")

#Call packages
library(tidyverse)
library(readxl)
library(viridis)
library(cividis)

#Read the information
popedu_census_2005 <- read_excel("Data Original/2005.xlsx") %>%
  rename("No Education"="Uneducated", "Post Secondary"="Tertiary", "sex"="Sex", "age"="age_group") %>%
  pivot_longer(!c(age, sex), names_to = "education", values_to = "pop") %>%
  mutate(education = replace(education, age == "0-4", "Under 15"),
         education = replace(education, age == "5-9", "Under 15"),
         education = replace(education, age == "10-14", "Under 15")) %>% 
  mutate(age = factor(age, levels = unique(age))) %>%
  mutate(education = factor(education, levels = unique(education))) %>%
  mutate(pop_pm = ifelse(test = sex == "Female", yes = -pop, no = pop),
         pop_pm = pop_pm/1e6) %>%
  mutate(pop_max = ifelse(sex == "Female", -max(pop/1e6), max(pop/1e6)), year=2005) %>%
  filter(year == 2005) 

popedu_graph_2005 <-  popedu_census_2005 %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = fct_rev(education))) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "white") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_viridis_d(option = "cividis", direction = 1, end = .9, name = "Education") +  # use the "cividis" palette
  labs(x = "Colombian Population 2005 (millions)", y = "Age") +
  facet_wrap(facets = "sex", scales = "free_x", strip.position = "bottom") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))

popedu_graph_2005

print(popedu_graph_2005)
ggsave("Output/popedu_graph_2005.png", device = "png")

################################################################################
################################################################################
################################################################################
################################################################################

popedu_census_2018 <- read_excel("Data Original/2018.xlsx") %>%
  rename("No Education"="Uneducated", "Post Secondary"="Tertiary", "sex"="Sex", "age"="age_group") %>%
  pivot_longer(!c(age, sex), names_to = "education", values_to = "pop") %>%
  mutate(education = replace(education, age == "0-4", "Under 15"),
         education = replace(education, age == "5-9", "Under 15"),
         education = replace(education, age == "10-14", "Under 15")) %>% 
  mutate(age = factor(age, levels = unique(age))) %>%
  mutate(education = factor(education, levels = unique(education))) %>%
  mutate(pop_pm = ifelse(test = sex == "Female", yes = -pop, no = pop),
         pop_pm = pop_pm/1e6) %>%
  mutate(pop_max = ifelse(sex == "Female", -max(pop/1e6), max(pop/1e6)), year=2018) %>%
  filter(year == 2018)
  
popedu_graph_2018 <-  popedu_census_2018 %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = fct_rev(education))) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "white") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_viridis_d(option = "cividis", direction = 1, end = .9, name = "Education") +  # use the "cividis" palette
  labs(x = "Colombian Population 2018 (millions)", y = "Age") +
  facet_wrap(facets = "sex", scales = "free_x", strip.position = "bottom") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))

popedu_graph_2018


print(popedu_graph_2018)
ggsave("Output/popedu_graph_2018.png", device = "png")
