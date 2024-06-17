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
library(tidyverse)
library(RColorBrewer)
############################################################################

#Read the information
popsex <- readRDS("Data Created/popsex.rds")
popedu <- readRDS("Data Created/popedu_15.rds")
popedu_w <- readRDS("Data Created/popedu_w.rds")


#
popedu_graph_w <- popedu_w %>% #get lp1 and lp2 and lp3 by sex
  pivot_longer(
    cols = starts_with("lp"), 
    names_to = c(".value", "sex"), 
    names_pattern = "(.*)_(.*)") 


popedu_graph_w_98_1 <- popedu_graph_w %>%
  filter(year==2013 & sex==1) %>%
  pivot_longer(
    cols = starts_with("lpi"),
    names_to = "lpi",
    values_to = "value"
  ) %>% 
  ggplot(mapping =  aes(x = age_group, y = value, color = lpi, group = lpi)) +
  geom_line() +
  labs(x = "Age Group", y = "logit Transformation of pi (1998)", color = "logit(PI)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_blank(), # Removes background
    plot.background = element_blank(),  # Removes background
    legend.background = element_blank() # Optional: Removes legend background
  )

popedu_graph_w_98_1 <- popedu_graph_w %>%
  filter(year==2013 & sex==1) %>%
  pivot_longer(
    cols = starts_with("lpi"),
    names_to = "lpi",
    values_to = "value"
  ) %>% # Create the line plot
  ggplot(mapping =  aes(x = age_group, y = value, color = lpi, group = lpi)) +
  geom_line() +
  labs(x = "Age Group", y = "logit Transformation of pi (1998)", color = "logit(PI)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

popedu_graph_w_98_1

popedu_graph_w_all_years_m <- popedu_graph_w %>%
  filter(sex == 1) %>%
  pivot_longer(
    cols = starts_with("lpi"),
    names_to = "lpi",
    values_to = "value"
  ) %>% 
  ggplot(mapping = aes(x = age_group, y = value, color = as.factor(year), linetype = lpi, group = interaction(year, lpi))) +
  geom_line() +
  labs(x = "Age Group", y = "Logit Transformation of pi", color = "Year", linetype = "LPI (Males)") +
  scale_color_manual(values = c("1998" = "lightgreen", 
                                "2003" = "lightblue", 
                                "2008" = "dodgerblue", 
                                "2013" = "darkorchid", 
                                "2018" = "darkred")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_blank(), # Removes background
    plot.background = element_blank()  # Removes background
  )

print(popedu_graph_w_all_years_m)
ggsave("Output/popedu_graph_lp_m.png", device = "png")
dev.off()

popedu_graph_w_all_years_f <- popedu_graph_w %>%
  filter(sex == 2) %>%
  pivot_longer(
    cols = starts_with("lpi"),
    names_to = "lpi",
    values_to = "value"
  ) %>% 
  ggplot(mapping = aes(x = age_group, y = value, color = as.factor(year), linetype = lpi, group = interaction(year, lpi))) +
  geom_line() +
  labs(x = "Age Group", y = "Logit Transformation of pi", color = "Year", linetype = "LPI (Females)") +
  scale_color_manual(values = c("1998" = "lightgreen", 
                                "2003" = "lightblue", 
                                "2008" = "dodgerblue", 
                                "2013" = "darkorchid", 
                                "2018" = "darkred")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_blank(), # Removes background
    plot.background = element_blank()  # Removes background
  )

print(popedu_graph_w_all_years_f)
ggsave("Output/popedu_graph_lp_f.png", device = "png")
dev.off()


################################################################################
################################################################################
################################################################################

popedu_graph_w_all_years_m_lm <- popedu_graph_w %>%
  filter(sex == 1) %>%
  pivot_longer(
    cols = starts_with("lpi"),
    names_to = "lpi",
    values_to = "value"
  ) %>% 
  ggplot(mapping = aes(x = age_group, y = value, color = as.factor(year), linetype = lpi, group = interaction(year, lpi))) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Age Group", y = "logit Transformation of pi", color = "Year", linetype = "LPI (Males)") +
  scale_color_manual(values = c("1998" = "lightgreen", 
                                "2003" = "lightblue", 
                                "2008" = "dodgerblue", 
                                "2013" = "darkorchid", 
                                "2018" = "darkred")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_blank(), # Removes background
    plot.background = element_blank()  # Removes background
  )

popedu_graph_w_all_years_m_lm
################################################################################
################################################################################
################################################################################




# Reverse the "Blues" palette
reversed_blues <- rev(brewer.pal(4, "Blues"))

#Add pop under 15
################################################################################
#Preparing the information to make the graphs
popedu_graph <- popedu  %>%
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
popedu_graph_selected <- popedu_graph %>%
  mutate(pop_pm = if_else(sex == "Male", -pop_pm, pop_pm),
         education = factor(education, levels = rev(c("No Education", "Primary", "Secondary", "Post Secondary"))))

# Create the plot
popedu_graph_all <-ggplot(popedu_graph_selected, aes(x = pop_pm, y = age, fill = education)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "white") +
  facet_wrap(~ year, ncol = 5) +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = reversed_blues, name = "Education") +
  labs(x = "Colombian Population (millions)", y = "Age") +
  theme(panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin(b = 0, t = 0)),
        panel.background = element_blank()) 

print(popedu_graph_all)
ggsave("Output/popedu_graph_all.png", device = "png")
dev.off()
 
################################################################################
#Graph 2018
popedu_graph2018 <- popedu_graph %>%
  filter(year == 2018) %>%
  mutate(pop_pm = if_else(sex == "Female", -pop_pm, pop_pm),
         education = factor(education, levels = rev(c("No Education", "Primary", "Secondary", "Post Secondary")))) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = education)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "white") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = reversed_blues, name = "Education") +  # Use the reversed palette
  labs(x = "Colombian Population 2018 (millions)", y = "Age") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.background = element_blank(),  # remove the background
        panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))



################################################################################
#Graph 2013
popedu_graph2013 <- popedu_graph %>%
  filter(year == 2013) %>%
  mutate(pop_pm = if_else(sex == "Female", -pop_pm, pop_pm),
         education = factor(education, levels = rev(c("No Education", "Primary", "Secondary", "Post Secondary")))) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = education)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "white") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = reversed_blues, name = "Education") +  # Use the reversed palette
  labs(x = "Colombian Population 2018 (millions)", y = "Age") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.background = element_blank(),  # remove the background
        panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))


################################################################################
#Graph 2008
popedu_graph2008 <- popedu_graph %>%
  filter(year == 2008) %>%
  mutate(pop_pm = if_else(sex == "Female", -pop_pm, pop_pm),
         education = factor(education, levels = rev(c("No Education", "Primary", "Secondary", "Post Secondary")))) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = education)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "white") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = reversed_blues, name = "Education") +  # Use the reversed palette
  labs(x = "Colombian Population 2018 (millions)", y = "Age") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.background = element_blank(),  # remove the background
        panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))

################################################################################
#Graph 2003
popedu_graph2003 <- popedu_graph %>%
  filter(year == 2003) %>%
  mutate(pop_pm = if_else(sex == "Female", -pop_pm, pop_pm),
         education = factor(education, levels = rev(c("No Education", "Primary", "Secondary", "Post Secondary")))) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = education)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "white") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = reversed_blues, name = "Education") +  # Use the reversed palette
  labs(x = "Colombian Population 2018 (millions)", y = "Age") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.background = element_blank(),  # remove the background
        panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))


################################################################################
#Graph 1998
popedu_graph1998 <- popedu_graph %>%
  filter(year == 1998) %>%
  mutate(pop_pm = if_else(sex == "Female", -pop_pm, pop_pm),
         education = factor(education, levels = rev(c("No Education", "Primary", "Secondary", "Post Secondary")))) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = education)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "white") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = reversed_blues, name = "Education") +  # Use the reversed palette
  labs(x = "Colombian Population 2018 (millions)", y = "Age") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.background = element_blank(),  # remove the background
        panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))




################################################################################
#Exporting graphs to output
print(popedu_graph1998)
ggsave("Output/popedu_graph1998.png", device = "png")
dev.off()
print(popedu_graph2003)
ggsave("Output/popedu_graph2003.png", device = "png")
dev.off()
print(popedu_graph2008)
ggsave("Output/popedu_graph2008.png", device = "png")
dev.off()
print(popedu_graph2013)
ggsave("Output/popedu_graph2013.png", device = "png")
dev.off()
print(popedu_graph2018)
ggsave("Output/popedu_graph2018.png", device = "png")
dev.off()




