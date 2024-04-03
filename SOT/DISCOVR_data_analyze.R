# Author: Paige Norris
# Date: May 2022
# Create interactive graphs from composition and cultivation data formatted in DISCOVR_data_clean.R

# Takes user input and creates an interactive HTML with graphs pertaining to growth
# trends of specfic algae strain(s) over a set period of time. Works in tandem with 
# DISCVOR_data_clean.R.

library(ggplot2)
library(ggplotlyExtra)
library(plotly)
library(ggplotify)
library(usethis) # source_url
library(devtools) # source_url

# Enter the dates of the time period you would like to look at in YYYY-MM-DD format
start_date = "2020-05-01"
end_date = "2020-07-31"

# Enter desired algae strain with names exactly matching file
strain = "P.celeri"

# Pull cleaning script from GitHub
# NOTE: THE BELOW FILE IS PULLED FROM PAIGE NORRIS'S (paigeanorris@gmail.com) GITHUB 
# DUE TO THE PRIVATE SETTING ON THE NREL-ALGAE TEAM REPO
source_url("https://raw.githubusercontent.com/paiyge/DISCOVR/main/cleaning")

### All variables plotted against time in days
ggplotly(ggplot(all_variable) +
           aes(x = DateTime, y = Value, fill = Variable, colour = PondID) +
           geom_line(size = 0.5) +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
           scale_fill_hue(direction = 1) +
           scale_color_hue(direction = 1) +
           labs(x = "DateTime", y = "Value", fill = "Variable", color = "PondID") +
           theme_minimal())

### All variables plotted against time in days, by pond
ggplotly(ggplot(all_variable) +
           aes(x = DateTime, y = Value, colour = Variable) +
           geom_line(size = 0.5) +
           scale_color_hue(direction = 1) +
           labs(x = "DateTime", y = "Value", color = "PondID") +
           theme_minimal() +
           facet_wrap(vars(PondID)))

### AFDW vs Date 
ggplotly(ggplot(strain_afdw) +
           aes(x = DateTime, y = Value, colour = PondID) +
           geom_line() +
           scale_color_hue(direction = 1) +
           theme_minimal() +
           facet_wrap(vars(Variable)))

### AFDW vs Date with lines only
ggplotly(ggplot(strain_afdw) +
           aes(x = DateTime, y = Value, colour = PondID) +
           geom_line(size = 0.5) +
           scale_color_hue(direction = 1) +
           theme_minimal() +
           facet_wrap(vars(Variable)))

### AFDW with DLI
ggplotly(ggplot(dli_afdw) +
           aes(x = DateTime, y = Value, colour = PondID) +
           geom_point() +
           geom_line(size = 0.5) +
           scale_color_hue(direction = 1) +
           theme_minimal())

### DLI with slope productivity (ahyp)
ggplotly(ggplot(dli_asp) +
           aes(x = DateTime, y = Value, colour = PondID) +
           geom_line(size = 0.5) +
           scale_color_hue(direction = 1) +
           theme_minimal())

### AFDW vs Date facets by ponds
ggplotly(ggplot(strain_data) +
           aes(x = DateTime, y = AFDW.AVE, colour = PondID) +
           geom_line(size = 0.5) +
           scale_color_hue(direction = 1) +
           theme_minimal() +
           facet_wrap(vars(PondID)))

### Growth, Composition, and DLI over time for FY 2020
ggplot(all_variable) +
  aes(x = DateTime, y = Value, fill = Variable, colour = PondID) +
  geom_line(size = 0.5) +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  labs(title = "Growth, Composition, and DLI over time for FY 2020", 
       subtitle = "AFDW, ASP, NH4, PO4", fill = "all_variable$variable") +
  theme_minimal()

### Growth, Composition, and DLI over time
ggplot(all_variable) +
  aes(x = DateTime, y = Value, fill = Variable, colour = PondID) +
  geom_line(size = 0.5) +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  labs(title = "Growth, Composition, and DLI over time for FY 2020", 
       subtitle = "AFDW, ASP, NH4, PO4", fill = "all_variable$variable") +
  theme_minimal()

# DLI and Slope Productivity over time
ggplot(dli_asp) +
  aes(x = DateTime, y = Value, fill = Variable, colour = PondID) +
  geom_line(size = 0.5) +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  labs(title = "Growth, Composition, and DLI over time for FY 2020", 
       subtitle = "AFDW, ASP, NH4, PO4", fill = "all_variable$variable") +
  theme_minimal()
