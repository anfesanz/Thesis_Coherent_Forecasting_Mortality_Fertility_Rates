################################################################################
# R Script Header
# 
# Created by: Felipe Sánchez
# Contact: anfesanz@gmail.com
# Created: 2022
# Last updated: Feb, May, Jul 2023
#
# Description:
# This script generates heatmaps for each mortality rate
#
# Input:
# Objects reconstructions.
#
# Output:
# Graphs are saved in the Output folder with names: p2018.png, p2005.png, and 
# for the reconstruction graph_rec.png and graph_rec_hf.png.
#
################################################################################


rm(list = ls(all = TRUE))  # Clear the workspace
# # Working directory
# getwd()
# #setwd("/Users/felipesanchez/Library/CloudStorage/Dropbox-Personal/UoM/UoM from MA/Mortality by Education/")
 setwd("/Users/felipesanchez/Documents/UoM/PhD/Phd Second Year/PopEdu Reconstruction/")
# 
# #Call packages
 library(tidyverse)
 library(viridis)
 library(RColorBrewer)
# ############################################################################
# 
#read the information 
mredu    <- readRDS("Data Created/mredu.rds")
mredu_hf <- readRDS("Data Created/mredu_hf.rds")

mredu <- mredu %>%
  pivot_longer(cols = starts_with("m"), names_to = "variable", values_to = "value") %>%
  mutate(value=log(value), value = replace_na(value, 0)) %>%
  select(-pop)

mredu_hf <- mredu_hf %>%
  pivot_longer(cols = starts_with("m"), names_to = "variable", values_to = "value") %>%
  mutate(value=log(value), value = replace_na(value, 0)) %>%
  select(-pop)


# Define the variables to iterate over
sex_values <- c(1, 2)
m_values <- c("m1", "m2", "m3", "m4")

# Create an empty list to store the plots
heatmap_plots <- list()

# Iterate over sex and m values
for (sexv in sex_values) {
  for (m in m_values) {
    # Filter the data for the specific sex and m value
    filtered_data <- mredu %>%
      filter(sex == sexv, variable == m)
    
    # Create the clustered heatmap plot
    heatmap_plot <- ggplot(filtered_data, aes(x = year, y = age_group, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradientn(
        colors = viridisLite::cividis(7),  # Use cividis color palette
        breaks = scales::pretty_breaks(n = 7)(range(filtered_data$value))  # Increase number of breaks
      ) +
      labs(x = "Year", y = "Age Group", fill = "Value") +
      theme_minimal()
    
    # Save the plot as a PNG file
    filename <- paste("Output/Graph_heatmap_sex", sexv, "edu", m, ".png", sep = "_")
    ggsave(filename, plot = heatmap_plot, device = "png")
    
    # Store the plot in the list
    heatmap_plots[[paste("sex", sexv, "m", m, sep = "_")]] <- heatmap_plot
  }
}


# Iterate over sex and m values HF
for (sexv in sex_values) {
  for (m in m_values) {
    # Filter the data for the specific sex and m value
    filtered_data <- mredu_hf %>%
      filter(sex == sexv, variable == m)
    
    # Create the clustered heatmap plot
    heatmap_plot <- ggplot(filtered_data, aes(x = year, y = age_group, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradientn(
        colors = viridisLite::cividis(7),  # Use cividis color palette
        breaks = scales::pretty_breaks(n = 7)(range(filtered_data$value))  # Increase number of breaks
      ) +
      labs(x = "Year", y = "Age Group", fill = "Value") +
      theme_minimal()
    
    # Save the plot as a PNG file
    filename <- paste("Output/Graph_heatmapHF_sex", sexv, "edu", m, ".png", sep = "_")
    ggsave(filename, plot = heatmap_plot, device = "png")
    
    # Store the plot in the list
    heatmap_plots[[paste("sex", sexv, "m", m, sep = "_")]] <- heatmap_plot
  }
}

