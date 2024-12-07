################################################################################
# R Script Header
# 
# Created by: Felipe Sánchez
# Contact: anfesanz@gmail.com
# Created: 2022
# Last updated: Feb, May, Jul 2023
#
# Description:
# This script generates pyramid graphs of the population data from reconstructions
# There are two reconstructions presented Lutz et al and Our proposal using the HF
#
# Input:
# Objects for 2005, 2018, and the reconstruction.
#
# Output:
# Graphs are saved in the Output folder with names: p2018.png, p2005.png, and 
# for the reconstruction graph_rec.png and graph_rec_hf.png.
#
################################################################################


# rm(list = ls(all = TRUE))  # Clear the workspace
# # Working directory
# getwd()
# #setwd("/Users/felipesanchez/Library/CloudStorage/Dropbox-Personal/UoM/UoM from MA/Mortality by Education/")
# setwd("/Users/felipesanchez/Documents/GitHub/PopEdu Reconstruction")
# 
# #Call packages
# library(tidyverse)
# library(viridis)
# library(RColorBrewer)
# ############################################################################
# 
# #Read the information
# popedu_rec <- readRDS("Data Created/popedu_rec.rds")
# popedu_rec_hf <- readRDS("Data Created/popedu_rec_hf.rds")
# Reverse the "Blues" palette
reversed_blues <- rev(brewer.pal(4, "Blues"))


####Lutz and Speringer
######################
# Processing and visualizing the population data

# Preparing the data for the graph
popedu_graph_rec <- popedu_rec  %>%
  rename("No Education"="edu1","Primary"="edu2", "Secondary"="edu3", "Post Secondary"="edu4", "age"="age_group") %>%
  mutate(sex = recode(sex, `1` = "Male", `2` = "Female"), sex = as.factor(sex))  %>%
  select(-pop) %>%
  pivot_longer(!c(age, sex, year), names_to = "education", values_to = "pop") %>%
  mutate(education = replace(education, age == "0-4", "Under 15"),
         education = replace(education, age == "5-9", "Under 15"),
         education = replace(education, age == "10-14", "Under 15")) %>% 
  mutate(age = factor(age, levels = unique(age))) %>%
  mutate(education = factor(education, levels = unique(education))) %>%
  mutate(pop_pm = ifelse(test = sex == 2, yes = -pop, no = pop),
         pop_pm = pop_pm/1e6) %>%
  mutate(pop_max = ifelse(sex == 2, -max(pop/1e6), max(pop/1e6)))


popedu_graph_rec <- popedu_graph_rec %>%
  mutate(pop_pm = if_else(sex == "Male", -pop_pm, pop_pm),
         education = factor(education, levels = rev(c("No Education", "Primary", "Secondary", "Post Secondary"))))

# Create and save the plot
popedu_graph_rec$year <- factor(popedu_graph_rec$year, levels = rev(unique(popedu_graph_rec$year)))

graph_rec <- ggplot(popedu_graph_rec, aes(x = pop_pm, y = age, fill = education)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "white") +
  facet_wrap(~ year, ncol = 5) +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_viridis_d(option = "cividis", name = "Education") +  # Here we use the cividis color scale
  labs(x = "Colombian Population (millions)", y = "Age") +
  theme(panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin(b = 0, t = 0)),
        panel.background = element_blank())

print(graph_rec)
ggsave("Output/graph_rec.png", device = "png")


####. HF
######################
#Add pop under 15
################################################################################
#Preparing the information to make the graphs
popedu_graph_rec_hf <- popedu_rec_hf  %>%
  rename("No Education"="edu1","Primary"="edu2", "Secondary"="edu3", "Post Secondary"="edu4", "age"="age_group") %>%
  mutate(sex = recode(sex, `1` = "Male", `2` = "Female"), sex = as.factor(sex))  %>%
  select(-pop) %>%
  pivot_longer(!c(age, sex, year), names_to = "education", values_to = "pop") %>%
  mutate(education = replace(education, age == "0-4", "Under 15"),
         education = replace(education, age == "5-9", "Under 15"),
         education = replace(education, age == "10-14", "Under 15")) %>% 
  mutate(age = factor(age, levels = unique(age))) %>%
  mutate(education = factor(education, levels = unique(education))) %>%
  mutate(pop_pm = ifelse(test = sex == 2, yes = -pop, no = pop),
         pop_pm = pop_pm/1e6) %>%
  mutate(pop_max = ifelse(sex == 2, -max(pop/1e6), max(pop/1e6)))

# Preprocess the data for the plot
popedu_graph_rec_hf <- popedu_graph_rec_hf %>%
  mutate(pop_pm = if_else(sex == "Male", -pop_pm, pop_pm),
         education = factor(education, levels = rev(c("No Education", "Primary", "Secondary", "Post Secondary"))))

# Create the plot
popedu_graph_rec_hf$year <- factor(popedu_graph_rec_hf$year, levels = rev(unique(popedu_graph_rec_hf$year)))

graph_rec_hf <- ggplot(popedu_graph_rec_hf, aes(x = pop_pm, y = age, fill = education)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "white") +
  facet_wrap(~ year, ncol = 5) +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_viridis_d(option = "cividis", name = "Education") +  # Here we use the cividis color scale
  labs(x = "Colombian Population (millions)", y = "Age") +
  theme(panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin(b = 0, t = 0)),
        panel.background = element_blank())

print(graph_rec_hf)
ggsave("Output/graph_rec_hf.png", device = "png")


