################################################################################
# R Script Header
# 
# Created by: Felipe Sanchez
# Contact: anfesanz@gmail.com
# Date: July 2023
#
# Description:
# The purpose of this script is to analyze the spatial correlation between education and violence using Coropleth maps. The National Center of Historical Memory of Colombia (https://micrositios.centrodememoriahistorica.gov.co/observatorio/portal-de-datos/base-de-datos/) provides information on victims (deaths) and cases of registered violence within the civil war. By combining this information with the population data by municipality in Colombia, we calculate two indicators: the Proportion of people with post-secondary education over people uneducated (ppsu) and the victims per uneducated people (vpu) and victims per municipality (vpt). The municipality level is used as the maximum spatial disaggregation.
#
# Input:
# - PERSONAS_DEMOGRAFICO_Cuadros_CNPV_2018.xlsx
# - "VictimasAB_202206.xlsx" Victims of Acts of War (Víctimas Acciones Bélicas)
# - "VictimasAP_202206.xlsx" Victims of Attacks on Populations (Víctimas Ataques Poblaciones)
# - "VictimasAS_202206.xlsx" Victims of Selective Killings (Víctimas Asesinatos Selectivos)
# - "VictimasAT_202206.xlsx" Victims of Terrorist Attacks (Víctimas Atentados Terroristas)
# - "VictimasDB_202206.xlsx" Victims of Damage to Civilian Property (Víctimas Daños Bienes Civiles)
# - "VictimasDF_202206.xlsx" Victims of Forced Disappearances (Víctimas Desaparición Forzada)
# - "VictimasMA_202206.xlsx" Victims of Massacres (Víctimas Masacres)
# - "VictimasMI_202206.xlsx" Victims of Landmines (Víctimas Minas)
# - "VictimasRU_202206.xlsx" Victims of Recruitment and Use of Children and Adolescents (Víctimas Reclutamiento y utilización de niños niñas y adolescentes)
# - "VictimasSE_202206.xlsx" Victims of Kidnappings (Víctimas Secuestros)
# - "VictimasVS_202206.xlsx" Victims of Sexual Violence (Víctimas Violencia Sexual)

# Output:
# - "Output/ppsu.png" Spatial graph of the Proportion of people with post-secondary education over people uneducated
# - "Output/vpt.png" Spatial graph of the victims per municipality (vpt)
################################################################################

# Clear the workspace
rm(list = ls(all = TRUE))  

# Set the working directory
getwd()
#setwd("/Users/felipesanchez/Documents/GitHub/PopEdu Reconstruction/")
setwd("/Users/felipesanchez/Documents/UoM/PhD/Phd Second Year/PopEdu Reconstruction/")

# Load libraries
library(sf)
library(ggplot2)
library(tidyverse)
library(readxl)
library(viridis)

# Specify the URL of the Excel file
#excel_url <-"https://www.dane.gov.co/files/censo2018/informacion-tecnica/PERSONAS_DEMOGRAFICO_Cuadros_CNPV_2018.xlsx"

# Define the local file path for downloading the Excel file
local_file <- "Data Original/PERSONAS_DEMOGRAFICO_Cuadros_CNPV_2018.xlsx"

# Download the Excel file from the DANE website
#download.file(url = excel_url, destfile = local_file, mode = "wb")

# Read the downloaded Excel file into R
data <- read_excel(local_file, sheet = "17PM", skip = 10)

#Edu1-Ninguno	Preescolar Primaria Incompleta
#Edu2- Primaria Completa Secundaria Completa	Secundaria Incompleta	Media Incompleta	Normal Incompleta	
#Edu3- Media Completa Normal Completa Tecnico
#Edu4- Tecnologico	Universitario	Especialización, Maestria, Doctorado

#all
df <- data %>%
  rename(dpto = "...1", mpio="...2", region="...3",sex="...4") %>%
  fill(dpto, mpio, region, sex) %>%
  filter(dpto != "Total Nacional") %>%
  filter(region == "Total" & sex != "Total") %>%
  separate(mpio, into = c("id", "mpiosnames"), sep = "_", convert = FALSE) %>%
  mutate(across(-c("dpto", "id", "mpiosnames", "region", "sex", "Total"), as.numeric)) %>%
  group_by(id) %>%
  summarize(across(where(is.numeric), sum)) %>%
  mutate(edu1= Ninguno+Preescolar+`Primaria Incompleta`+ `Sin Información`/4, edu2= `Primaria Completa` + `Secundaria Incompleta`+ `Secundaria Completa`+`Media Incompleta`+`Normal Incompleta`+`Sin Información`/4,
         edu3= Tecnico+`Media Completa`+`Normal Completa`+`Sin Información`/4, edu4=Tecnologico+Universitario+ `Especialización, Maestria, Doctorado`+`Sin Información`/4) %>%
  mutate(pes=edu4/Total, pesw=1/max(pes)*pes, ppsu=edu4/edu1, ppsp=edu4/edu2, pp=(edu4+edu3)/(edu1+edu2)) %>% #percentage with postsecondary education pes, pesw=weighted max to one
  select(id, pes, pesw, ppsu, ppsp, pp, Total, edu1, edu2, edu3, edu4)


#what is ppsu? what are x and y axes?? you say it is a proportion but your legend is between 0-ish and 3+??? Text says it is a ratio???

#Belén de Bajirá was part of Riosucio (dane code 27615)
# Select the row you want to duplicate
belen <- df %>%
  filter(id==27615) %>%
  mutate(id = replace(id, id == 27615, 27086))

# Copy the selected row and append it to the dataframe
df <- bind_rows(df, belen)



# Read the shapefile of Colombia's administrative divisions
colombia <- st_read("Data Original/mpio.shp")
colombia <- colombia %>%
  rename(id=MPIOS)  %>%
  left_join(df, by = "id")

#Plot ppsu
# ppsu<-ggplot() +
#   geom_sf(data = colombia, aes(fill = ppsu)) +
#   scale_fill_gradient(low = "blue", high = "red") 
ppsu <- ggplot() +
  geom_sf(data = colombia, aes(fill = ppsu)) +
  scale_fill_viridis_c(option = "cividis") +
  labs(
    fill = "PPSU Ratio",  # Concise label
    caption = "Source: Census 2018 and CHM. PPSU represents the ratio of postsecondary to no/low education."
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text.x = element_blank(),   # Remove x-axis numbers
    axis.text.y = element_blank(),   # Remove y-axis numbers
    axis.ticks = element_blank(),    # Remove axis ticks
    panel.background = element_blank(),  # Remove the background
    plot.background = element_blank()    # Remove plot background
  )

# Display the plot
print(ppsu)

ppsu <- ggplot() +
  geom_sf(data = colombia, aes(fill = ppsu), color = "gray80", size = 0.2) +  # Light gray borders to enhance clarity
  scale_fill_viridis_c(option = "cividis", direction = -1, breaks = c(1, 2, 3), labels = c("Low", "Medium", "High")) +  # Inverse the cividis palette
  labs(
    fill = "PPSU Ratio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),  # Centered and larger title
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text.x = element_blank(),   # Remove x-axis numbers
    axis.text.y = element_blank(),   # Remove y-axis numbers
    axis.ticks = element_blank(),    # Remove axis ticks
    panel.background = element_blank(),  # Remove the background
    plot.background = element_blank(),    # Remove plot background
    legend.position = "right",  # Ensure legend is on the right
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 8)  # Adjust legend text size
  )

# Display the plot
print(ppsu)

#Victims Armed conflict Colombia
################################
# Victims of Sexual Violence
# Victims of Kidnappings
# Victims of Recruitment and Use of Children and Adolescents
# Victims of Landmines
# Victims of Massacres
# Victims of Forced Disappearance
# Victims of Civilian Property Damage
# Victims of Terrorist Attacks
# Victims of Selective Killings
# Victims of Population Attacks
# Victims of Acts of War
# Cases of Sexual Violence
# Cases of Kidnappings
# Cases of Recruitment and Use of Children and Adolescents
# Cases of Landmines
# Cases of Massacres
# Cases of Forced Disappearance
# Cases of Civilian Property Damage
# Cases of Terrorist Attacks
# Cases of Selective Killings
# Cases of Population Attacks
# Cases of Acts of War

# "VictimasAB_202206.xlsx" Víctimas Acciones Bélicas
# "VictimasAP_202206.xlsx" Víctimas Ataques Poblaciones
# "VictimasAS_202206.xlsx" Víctimas Asesinatos Selectivos
# "VictimasAT_202206.xlsx" Víctimas Atentados Terroristas
# "VictimasDB_202206.xlsx" Víctimas Daños Bienes Civiles
# "VictimasDF_202206.xlsx" Víctimas Desaparición Forzada
# "VictimasMA_202206.xlsx" Víctimas Masacres
# "VictimasMI_202206.xlsx" Víctimas Minas
# "VictimasRU_202206.xlsx" Víctimas Reclutamiento y utilización de niños niñas y adolescentes
# "VictimasSE_202206.xlsx" Víctimas Secuestros
# "VictimasVS_202206.xlsx" Víctimas Violencia Sexual


# File paths
file_paths <- c(
  "Data Original/casesviolence/2022-06-30/VictimasAB_202206.xlsx",
  "Data Original/casesviolence/2022-06-30/VictimasAP_202206.xlsx",
  "Data Original/casesviolence/2022-06-30/VictimasAS_202206.xlsx",
  "Data Original/casesviolence/2022-06-30/VictimasAT_202206.xlsx",
  "Data Original/casesviolence/2022-06-30/VictimasDB_202206.xlsx",
  "Data Original/casesviolence/2022-06-30/VictimasDF_202206.xlsx",
  "Data Original/casesviolence/2022-06-30/VictimasMA_202206.xlsx",
  "Data Original/casesviolence/2022-06-30/VictimasMI_202206.xlsx",
  "Data Original/casesviolence/2022-06-30/VictimasRU_202206.xlsx",
  "Data Original/casesviolence/2022-06-30/VictimasSE_202206.xlsx",
  "Data Original/casesviolence/2022-06-30/VictimasVS_202206.xlsx"
)

# Read and process files
casesviolence <- lapply(file_paths, function(file_path) {
  read_excel(file_path) %>%
    select(`Código DANE de Municipio`, Municipio, Departamento, Año) %>%
    rename(id = `Código DANE de Municipio`, year = Año) %>%
    mutate(victims=1)
})

# Combine data frames
casesviolence <- bind_rows(casesviolence)


# Collapse data by ID and Year and calculate the sum
dfviolence <- casesviolence %>%
  mutate(id = ifelse(substr(id, 3, 5) == "000", paste0(substr(id, 1, 2), "001"), id)) %>%
  group_by(id, year) %>%
  summarize(across(where(is.numeric), sum)) %>%
  filter(!grepl("^EX", id)) %>%
  filter(!grepl("00001", id)) %>%
  filter(year<2018) %>%
  group_by(id) %>%
  summarize(across(where(is.numeric), sum)) %>%
  mutate(victims = ifelse(is.na(victims), 0, victims))

# # frequency table
# frequency_table <- table(dfviolence$year)
# print(frequency_table)


# Read the shapefile of Colombia's administrative divisions
colombia <- st_read("Data Original/mpio.shp")
colombia <- colombia %>%
  rename(id=MPIOS)  %>%
  left_join(df, by = "id") %>%
  left_join(dfviolence, by = "id")

#Victims per uneducated people
colombia <- colombia %>%
  mutate(vpu=victims/edu1, vpt=victims/Total)

# vpt<-ggplot() +
#   geom_sf(data = colombia, aes(fill = vpt)) +
#   scale_fill_gradient(low = "blue", high = "red")

vpt <- ggplot() +
  geom_sf(data = colombia, aes(fill = vpt), color = "gray60", size = 0.3) +  # Slightly darker borders for clarity
  scale_fill_viridis_c(option = "cividis", direction = -1, 
                       breaks = c(0, 0.05, 0.1, 0.15, 0.20), 
                       labels = c("Very Low", "Low", "Medium", "High", "Very High")) +  # Custom breaks for better differentiation
  labs(
    fill = "VPT Ratio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),  # Centered and larger title
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text.x = element_blank(),   # Remove x-axis numbers
    axis.text.y = element_blank(),   # Remove y-axis numbers
    axis.ticks = element_blank(),    # Remove axis ticks
    panel.background = element_blank(),  # Remove the background
    plot.background = element_blank(),   # Remove plot background
    legend.position = "right",  # Ensure legend is on the right
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 8)  # Adjust legend text size
  )

# Display the plot
print(vpt)

#Saving graphs

print(ppsu)
ggsave("Output/ppsu.png", device = "png")
dev.off()

print(vpt)
ggsave("Output/vpt.png", device = "png")
dev.off()
