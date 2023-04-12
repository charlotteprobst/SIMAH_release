# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: ALCOHOL POLICY BRFSS 
## Aim: Temporal availability restrictions US
## Author: Carolin Kilian
## Start Date: 03/01/2023
# ----------------------------------------------------------------
# ----------------------------------------------------------------

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# LIBARIES
# ----------------------------------------------------------------
# ----------------------------------------------------------------

library(tidyverse)
library(data.table)
library(dplyr)
library(openxlsx)
library("sf")
library(rgeos)
library(rnaturalearth)
library(ggplot2)
library(patchwork)
library(ggpattern)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230103

datAV <- data.table(read.xlsx("policies/LIBERAL_control_outlet_hours.xlsx", na.strings = c("NA", "")))
datAV <- copy(datAV[year >= 2000,.(state, year, no_sun_sales, controlstate)])

mapUS <- ne_states(country = "United States of America", returnclass = "sf")

# --------------------------------------------------------------------------------------
# notes:  the LIBERAL dataset assumes more availability if local options are allowed
#         no_sunday_sales = 1 is no spirits sales on sundays
#         controlstate = 1 is a state monopoly over the wholesaling or retailing of some or all categories of alcoholic beverages
#         => needs to be discussed with Bill: is this just about spirits or control state for all alcoholic beverages
# --------------------------------------------------------------------------------------

# merge datafiles 
datUS <- merge(mapUS, datAV, by.x = c("name"), by.y = "state", all.x = T, all.y = T)

# ----------------------------------------------------------------
# AVAILABILITY BY STATE
# ----------------------------------------------------------------

years <- c("2000", "2005", "2010", "2015", "2020")

for (i in 1:length(years)) { 
  
mainUS <- ggplot(data = datUS[datUS$year == years[i] & datUS$name != "Hawaii" & datUS$name != "Alaska",]) + 
  geom_sf_pattern(aes(fill = as.factor(no_sun_sales), pattern = as.factor(controlstate)), 
                  color = "white", pattern_color = "black", pattern_fill = "black", pattern_density = 0.1, pattern_spacing = 0.02) + 
  scale_fill_manual(values = c("#CEE2E6", "#8B3E31"), na.value = "white") +
  scale_pattern_manual(values = c("none", "stripe")) + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = 'white', color = 'white'),
        panel.grid=element_blank(),
        axis.text=element_blank(), 
        axis.title=element_blank(), 
        axis.ticks=element_blank()) + 
  coord_sf(xlim = c(-138,-60), ylim = c(20,50), expand = FALSE) 

hawaii <- ggplot(data = datUS[datUS$year == years[i] & datUS$name == "Hawaii",]) + 
  geom_sf_pattern(aes(fill = as.factor(no_sun_sales), pattern = as.factor(controlstate)), 
                  color = "white", pattern_color = "black", pattern_fill = "black", pattern_density = 0.1, pattern_spacing = 0.02) + 
  scale_fill_manual(values = c("#CEE2E6", "#8B3E31"), na.value = "white") +
  scale_pattern_manual(values = c("none", "stripe")) + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = 'white', color = 'white'),
        panel.grid=element_blank(),
        axis.text=element_blank(), 
        axis.title=element_blank(), 
        axis.ticks=element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  coord_sf(xlim = c(-161,-154), ylim = c(18,23), expand = FALSE) 

alaska <- ggplot(data = datUS[datUS$year == years[i] & datUS$name == "Alaska",]) + 
  geom_sf_pattern(aes(fill = as.factor(no_sun_sales), pattern = as.factor(controlstate)), 
                  color = "white", pattern_color = "black", pattern_fill = "black", pattern_density = 0.1, pattern_spacing = 0.02) + 
  scale_fill_manual(values = c("#CEE2E6", "#8B3E31"), na.value = "white") +
  scale_pattern_manual(values = c("none", "stripe")) + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = 'white', color = 'white'),
        panel.grid=element_blank(),
        axis.text=element_blank(), 
        axis.title=element_blank(), 
        axis.ticks=element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  coord_sf(xlim = c(-180,-128), ylim = c(54,73), expand = FALSE) 

mainUS + 
  inset_element(alaska, left = 0.05, bottom = -0.11, right = 0.37, top = 0.3) + 
  inset_element(hawaii, left = 0.32, bottom = -0.235, right = 0.49, top = 0.3)
  
ggsave(paste0("policies/figures/", DATE, "_NO SUNDAY SALES_US_", years[i], ".pdf"), dpi=300, width = 12, height = 7)

}

# ----------------------------------------------------------------
# CHANGES IN SUNDAY SALES BANS BY STATE AND YEAR
# ----------------------------------------------------------------

for (i in 1:length(unique(datAV$state))) {

  name <- unique(datAV$state)[i] 
  
  if (datAV[state == name, sum(no_sun_sales)] == 0) {
    print(paste0("Sunday sales were NEVER banned since 2000 in: ", name))
    datAV[state == name, change := "never"]
  }
  
  else if (datAV[state == name, sum(no_sun_sales)] == 22) {
    print(paste0("Sunday sales were ALWAYS banned since 2000 in: ", name))
    datAV[state == name, change := "always"]
  }
  
  else {
    
    for (m in 1:(length(unique(datAV$year))-2)) {
      
      c.y <- unique(datAV$year)[m] 
      f.y <- unique(datAV$year)[m+1]
      
      if (datAV[state == name & year == c.y]$no_sun_sales == datAV[state == name & year == f.y]$no_sun_sales) {
        datAV[state == name & year == f.y, change := "no change"]
      }
      
      if (datAV[state == name & year == c.y]$no_sun_sales < datAV[state == name & year == f.y]$no_sun_sales) {
        datAV[state == name & year == f.y, change := "new ban"]
      }
      
      if (datAV[state == name & year == c.y]$no_sun_sales > datAV[state == name & year == f.y]$no_sun_sales) {
        datAV[state == name & year == f.y, change := "new permission"]
      }
      
      else {}
    } 
  }
  datAV[state == name & year == 2000, change := NA]
}

# summary
datAV[change %like% "new ban", table(state, year)] # years: 2001, 2002, 2004, 2013, 2017
datAV[change %like% "new ban", table(state)] # new bans in 13 states, 10 x 1, 3 x 2 

datAV[change %like% "new permission", table(state, year)] # years: 2003, 2004, 2005, 2006, 2008, 2011, 2012, 2013, 2016, 2017
datAV[change %like% "new permission", table(state)] # new permissions in 24 states, 21 x 1, 3 x 2

datAV[change %like% "never", unique(state)] # never banned in 20 states
datAV[change %like% "always", unique(state)] # always banned in 5 states

