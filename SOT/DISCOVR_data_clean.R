# NREL - DISCOVR
# Author: Paige Norris
# Date: June 2022
# Goal: Importing, formatting, and cleaning data from OpenEI DISCOVR page 

# Reads in weather and cultivation & composition data from the DISCOVR OpenEI webiste
# Works in tandem with https://github.com/NRELAlgaeTeam/DISCOVR/blob/main/SOT/Analysis.Rmd, 
# a markdown that takes user input for the desired date span and strain(s). 
# Calulates PAR and DLI from weather data. Creates large df for all variables and smaller
# dfs for specific comparisons. The analysis markdown creates graphs using this cleaned data

library(readr) # reads in files
library(dplyr) # piping (%>%), mutate, group_by/summarise
library(lubridate) # Datetime conversion
library(tidyverse) 

# Import data from OpenEI DISCOVR page 
# https://apps.openei.org/DISCOVR/#summaryTables
the_weather <- read.csv(file = "https://data.openei.org/files/5693/DISCOVR_SOT_weather_2020.csv")
the_data <- read_csv(file = "https://data.openei.org/files/5693/DISCOVR_SOT_cultivation-composition_2020.csv")

# Transform Datetime into POSIXct for growth and weather data then strip time 
# from weather DateTime and isolate only date in new column
the_data$DateTime <- mdy_hm(the_data$DateTime, tz = "UTC")

the_weather$Date <- mdy_hm(the_weather$DATETIME, tz = "UTC")
the_weather <- the_weather %>%
  mutate(Date = as.Date(Date))

# Isolate desired date range
the_weather <- the_weather%>% 
  filter(Date >= as.Date(start_date) & Date <= as.Date(end_date))

the_data <- the_data %>% 
  filter(DateTime >= as.Date(start_date) & DateTime <= as.Date(end_date))

# Isolate desired strain - however, there is only one strain in June, P.Celerei
strain_weather<- the_weather[(the_weather$StrainID == strain),]
strain_data <- the_data[(the_data$StrainID == strain),]
strain_data <- strain_data[!(strain_data$PondID == "inoc"),]

# Find daily par by finding 24 hour average, then find DLI by multiplying PAR 
# by 24 hours in day and 0.0036 conversion factor. Scalings are done for graphing
# purposes
PAR <- strain_weather %>%
  group_by(Date) %>%
  summarise(PPFD.umol.m2s = mean(HOBO.PAR))%>%
  rowwise(PPFD.umol.m2s) %>%
  mutate(DLI.mol.m2.day = (sum(PPFD.umol.m2s * 24 * 0.0036))) %>%
  mutate(DLI.scaled.hundreth = (sum(PPFD.umol.m2s * 24 * 0.0036 * .01))) %>%
  mutate(DLI.scaled.quarter = (sum(PPFD.umol.m2s * 24 * 0.0036 * 0.25)))

# Create long df for each variable
strain_weather <- data.frame("DateTime" = PAR$Date, "Variable" = "DLI", "Value" = PAR$DLI.mol.m2.day, "PondID" = "DLI for all Ponds")
strain_weather_hundreth <- data.frame("DateTime" = PAR$Date, "Variable" = "DLI", "Value" = PAR$DLI.scaled.hundreth, "PondID" = "DLI for all Ponds") 
strain_weather_quarter <- data.frame("DateTime" = PAR$Date, "Variable" = "DLI", "Value" = PAR$DLI.scaled.quarter, "PondID" = "DLI for all Ponds")
strain_afdw <- data.frame("DateTime" = strain_data$DateTime, "Variable" = "AFDW.AVE", "Value" = strain_data$AFDW.AVE, "PondID" = strain_data$PondID)
strain_asp <- data.frame("DateTime" = strain_data$DateTime, "Variable" = "ASP", "Value" = strain_data$ASP........all.ponds, "PondID" = strain_data$PondID)
strain_nh4 <- data.frame("DateTime" = strain_data$DateTime, "Variable" = "NH4.AVE", "Value" = strain_data$NH4.AVE, "PondID" = strain_data$PondID)
strain_po4 <- data.frame("DateTime" = strain_data$DateTime, "Variable" = "PO4.AVE", "Value" = strain_data$PO4.AVE, "PondID" = strain_data$PondID)
strain_fame <- data.frame("DateTime" = strain_data$DateTime, "Variable" = "FAME", "Value" = strain_data$FAME, "PondID" = strain_data$PondID)
strain_carbs <- data.frame("DateTime" = strain_data$DateTime, "Variable" = "Carbohydrates", "Value" = strain_data$Carbohydrates, "PondID" = strain_data$PondID)
strain_protein <- data.frame("DateTime" = strain_data$DateTime, "Variable" = "Protein", "Value" = strain_data$Protein, "PondID" = strain_data$PondID)

# Combine each variable df into one long form df
all_variable <- rbind(strain_afdw, strain_asp, strain_fame, strain_nh4, strain_po4, strain_protein, strain_carbs, strain_weather) %>%
  na.omit((all_variable))

# Create smaller sub dfs for specifc comparisons
dli_afdw <- rbind(strain_afdw, strain_weather_hundreth) %>%
  na.omit((dli_afdw))
dli_asp <- rbind(strain_asp, strain_weather_quarter) %>%
  na.omit((dli_asp))
asp_po4 <- rbind(strain_asp, strain_po4) %>%
  na.omit((asp_po4))
asp_nh4 <- rbind(strain_asp, strain_nh4) %>%
  na.omit((asp_nh4))
