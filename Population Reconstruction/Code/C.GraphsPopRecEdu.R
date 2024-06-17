################################################################################
# R Script Header
# 
# Created by: Felipe Sánchez
# Contact: anfesanz@gmail.com
# Date: Feb, May, Jul 2023
#
# Description:
#Pyramids graph of the population from Censuses and from Reconstruction
#
# Input:
#Objects for 2005, 2018 and the reconstruction
#
# Output:
#Graphs Output/p2018.png, Output/p2005.png and for the reconstruction
#
################################################################################

rm(list = ls(all = TRUE))  # Clear the workspace

# Working directory
getwd()
#setwd("/Users/felipesanchez/Library/CloudStorage/Dropbox-Personal/UoM/UoM from MA/Mortality by Education/")
setwd("/Users/felipesanchez/Documents/GitHub/PopEdu Reconstruction")

#Call packages
library(rjags)
library(dclone)
library(snow)
library(lemon)
library(tidyverse)
library(devtools)
library(wcde) #IASA information
library(gganimate) #Gift images
library(readxl)
library(writexl)
############################################################################

#Read the information
census2005a <- read_excel("Data Original/2005.xlsx") 
census2005a <- census2005a %>%
  rename("No Education"="Uneducated", "Post Secondary"="Tertiary", "sex"="Sex", "age"="age_group") %>%
  pivot_longer(!c(age, sex), names_to = "education", values_to = "pop") %>%
  mutate(education = replace(education, age == "0-4", "Under 15"),
         education = replace(education, age == "5-9", "Under 15"),
         education = replace(education, age == "10-14", "Under 15"))

census2005a <- census2005a %>% 
  mutate(age = factor(age, levels = unique(age))) %>%
  mutate(education = factor(education, levels = unique(education))) %>%
  mutate(pop_pm = ifelse(test = sex == "Female", yes = -pop, no = pop),
         pop_pm = pop_pm/1e6) %>%
  mutate(pop_max = ifelse(sex == "Female", -max(pop/1e6), max(pop/1e6)), year=2005)


p2005<-census2005a %>%
  filter(year == 2005) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = fct_rev(education))) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "white") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = wic_col4, name = "Education") +
  labs(x = "Colombian Population 2005 (millions)", y = "Age") +
  facet_wrap(facets = "sex", scales = "free_x", strip.position = "bottom") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))

################################################################################
################################################################################
################################################################################
################################################################################

census2018a <- read_excel("Data Original/2018.xlsx") 
census2018a <- census2018a %>%
  rename("No Education"="Uneducated", "Post Secondary"="Tertiary", "sex"="Sex", "age"="age_group") %>%
  pivot_longer(!c(age, sex), names_to = "education", values_to = "pop") %>%
  mutate(education = replace(education, age == "0-4", "Under 15"),
         education = replace(education, age == "5-9", "Under 15"),
         education = replace(education, age == "10-14", "Under 15"))

census2018a <- census2018a %>% 
  mutate(age = factor(age, levels = unique(age))) %>%
  mutate(education = factor(education, levels = unique(education))) %>%
  mutate(pop_pm = ifelse(test = sex == "Female", yes = -pop, no = pop),
         pop_pm = pop_pm/1e6) %>%
  mutate(pop_max = ifelse(sex == "Female", -max(pop/1e6), max(pop/1e6)), year=2018)


p2018<-census2018a %>%
  filter(year == 2018) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = fct_rev(education))) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "white") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = wic_col4, name = "Education") +
  labs(x = "Colombian Population 2018 (millions)", y = "Age") +
  facet_wrap(facets = "sex", scales = "free_x", strip.position = "bottom") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))

####
###
###
reconstruction <- read_excel("Data Created/Reconstruction.xlsx")
#2018
census2018 <- reconstruction %>%
    filter(year==2018) %>%
    select(-year)  %>%
    rename("No Education"="Uneducated", "Post Secondary"="Tertiary", "age"="age_group") %>%
    pivot_longer(!c(age, sex), names_to = "education", values_to = "pop") %>%
    mutate(education = replace(education, age == "0-4", "Under 15"),
           education = replace(education, age == "5-9", "Under 15"),
           education = replace(education, age == "10-14", "Under 15")) %>%
    mutate(age = factor(age, levels = unique(age))) %>%
    mutate(education = factor(education, levels = unique(education))) %>%
    mutate(pop_pm = ifelse(test = sex == "Female", yes = -pop, no = pop), pop_pm = pop_pm/1e6) %>%
    mutate(pop_max = ifelse(sex == "Female", -max(pop/1e6), max(pop/1e6)), year=2018)

p2018<-census2018 %>%
  filter(year == 2018) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = fct_rev(education))) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "black") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = wic_col4, name = "Education") +
  labs(x = "Colombian Population 2018 (millions)", y = "Age") +
  facet_wrap(facets = "sex", scales = "free_x", strip.position = "bottom") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))

#2013
census2013 <- reconstruction %>%
  filter(year==2013) %>%
  select(-year)  %>%
  rename("No Education"="Uneducated", "Post Secondary"="Tertiary", "age"="age_group") %>%
  pivot_longer(!c(age, sex), names_to = "education", values_to = "pop") %>%
  mutate(education = replace(education, age == "0-4", "Under 15"),
         education = replace(education, age == "5-9", "Under 15"),
         education = replace(education, age == "10-14", "Under 15")) %>%
  mutate(age = factor(age, levels = unique(age))) %>%
  mutate(education = factor(education, levels = unique(education))) %>%
  mutate(pop_pm = ifelse(test = sex == "Female", yes = -pop, no = pop), pop_pm = pop_pm/1e6) %>%
  mutate(pop_max = ifelse(sex == "Female", -max(pop/1e6), max(pop/1e6)), year=2013)

p2013<-census2013 %>%
  filter(year == 2013) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = fct_rev(education))) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "black") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = wic_col4, name = "Education") +
  labs(x = "Colombian Population 2013 (millions)", y = "Age") +
  facet_wrap(facets = "sex", scales = "free_x", strip.position = "bottom") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))

#2008
census2008 <- reconstruction %>%
  filter(year==2008) %>%
  select(-year)  %>%
  rename("No Education"="Uneducated", "Post Secondary"="Tertiary", "age"="age_group") %>%
  pivot_longer(!c(age, sex), names_to = "education", values_to = "pop") %>%
  mutate(education = replace(education, age == "0-4", "Under 15"),
         education = replace(education, age == "5-9", "Under 15"),
         education = replace(education, age == "10-14", "Under 15")) %>%
  mutate(age = factor(age, levels = unique(age))) %>%
  mutate(education = factor(education, levels = unique(education))) %>%
  mutate(pop_pm = ifelse(test = sex == "Female", yes = -pop, no = pop), pop_pm = pop_pm/1e6) %>%
  mutate(pop_max = ifelse(sex == "Female", -max(pop/1e6), max(pop/1e6)), year=2008)

p2008<-census2008 %>%
  filter(year == 2008) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = fct_rev(education))) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "black") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = wic_col4, name = "Education") +
  labs(x = "Colombian Population 2008 (millions)", y = "Age") +
  facet_wrap(facets = "sex", scales = "free_x", strip.position = "bottom") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))


#2003
census2003 <- reconstruction %>%
  filter(year==2003) %>%
  select(-year)  %>%
  rename("No Education"="Uneducated", "Post Secondary"="Tertiary", "age"="age_group") %>%
  pivot_longer(!c(age, sex), names_to = "education", values_to = "pop") %>%
  mutate(education = replace(education, age == "0-4", "Under 15"),
         education = replace(education, age == "5-9", "Under 15"),
         education = replace(education, age == "10-14", "Under 15")) %>%
  mutate(age = factor(age, levels = unique(age))) %>%
  mutate(education = factor(education, levels = unique(education))) %>%
  mutate(pop_pm = ifelse(test = sex == "Female", yes = -pop, no = pop), pop_pm = pop_pm/1e6) %>%
  mutate(pop_max = ifelse(sex == "Female", -max(pop/1e6), max(pop/1e6)), year=2003)

p2003<-census2003 %>%
  filter(year == 2003) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = fct_rev(education))) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "black") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = wic_col4, name = "Education") +
  labs(x = "Colombian Population 2013 (millions)", y = "Age") +
  facet_wrap(facets = "sex", scales = "free_x", strip.position = "bottom") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))

#1998
census1998 <- reconstruction %>%
  filter(year==1998) %>%
  select(-year)  %>%
  rename("No Education"="Uneducated", "Post Secondary"="Tertiary", "age"="age_group") %>%
  pivot_longer(!c(age, sex), names_to = "education", values_to = "pop") %>%
  mutate(education = replace(education, age == "0-4", "Under 15"),
         education = replace(education, age == "5-9", "Under 15"),
         education = replace(education, age == "10-14", "Under 15")) %>%
  mutate(age = factor(age, levels = unique(age))) %>%
  mutate(education = factor(education, levels = unique(education))) %>%
  mutate(pop_pm = ifelse(test = sex == "Female", yes = -pop, no = pop), pop_pm = pop_pm/1e6) %>%
  mutate(pop_max = ifelse(sex == "Female", -max(pop/1e6), max(pop/1e6)), year=1998)

p1998<-census1998 %>%
  filter(year == 1998) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = fct_rev(education))) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "black") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = wic_col4, name = "Education") +
  labs(x = "Colombian Population 1998 (millions)", y = "Age") +
  facet_wrap(facets = "sex", scales = "free_x", strip.position = "bottom") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))

#Exporting graphs to output
print(p2018)
ggsave("Output/p2018.png", device = "png")
dev.off()

print(p2005)
ggsave("Output/p2005.png", device = "png")
dev.off()


print(p2013)
ggsave("Output/p2013.png", device = "png")
dev.off()

print(p2008)
ggsave("Output/p2008.png", device = "png")
dev.off()

print(p2003)
ggsave("Output/p2003.png", device = "png")
dev.off()

print(p1998)
ggsave("Output/p1998.png", device = "png")
dev.off()




