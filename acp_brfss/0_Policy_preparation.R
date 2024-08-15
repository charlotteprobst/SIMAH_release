# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: Sunday sales ban (Policy file)
## State: all US states
## Author: Carolin Kilian
## Start Date: 07/05/2023
# ----------------------------------------------------------------
# ----------------------------------------------------------------

# --------------------------------------------------------------------------------------

rm(list = ls())

# ----------------------------------------------------------------
# LIBARIES
# ----------------------------------------------------------------

library(tidyverse)
library(data.table)

# --------------------------------------------------------------------------------------

# set date
DATE <- 20230922

# define states
state <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
            "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
            "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
            "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
            "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
            "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
            "West Virginia", "Wisconsin", "Wyoming")

# expand states by years
data <- expand_grid(state, 2000:2020) %>% rename("year" = `2000:2020`)

# --------------------------------------------------------------------------------------

# PREPARE SUNDAY SALES DATA 
# data source: https://alcoholpolicy.niaaa.nih.gov

# load data
ban <- read_csv("/Users/carolinkilian/Desktop/SIMAH_workplace/acp_brfss/data/sunday sales data/download_policy_topic_data__merged_topic_data__07-05-2023_1015am.csv",
                col_types = cols(`Ban Repealed` = col_number(), `Exceptions: Local Option` = col_number(), `Exceptions: 3.2 Beer Sales Allowed` = col_number()))

# reorganise file
ban <- ban %>% mutate(ban_repealed = ifelse(is.na(`Ban Repealed`), 0, 1),
                       local_option = ifelse(is.na(`Exceptions: Local Option`), 0, 1),
                       beer_excepted = ifelse(is.na(`Exceptions: 3.2 Beer Sales Allowed`), 0, 1),
                      DatePolicy = `Date Range - Start`) %>%
  separate(`Date Range - Start`, c("start_month", "start_day", "start_year"), "/", convert = TRUE) %>%
  separate(`Date Range - End`, c("end_month", "end_day", "end_year"), "/", convert = TRUE) %>%
  select(c("Jurisdiction", "DatePolicy", "start_month", "start_year", "end_month", "end_year", "ban_repealed", "local_option", "beer_excepted"))
  
# recode with 2: total ban, 1: local options or beer sales, 0: no ban
pdat <- copy(ban)
pdat[,2:9] <- NA
pdat$sunsalesban <- NA
pdat$sunsalesban_exloc <- NA

for (c in ban$Jurisdiction) {
  
  # Sunday sales ban was repealed
  if (1 %in% ban[ban$Jurisdiction == c,]$ban_repealed) { 
    
    k <- 1 
    
    for (k in 1:length(ban[ban$Jurisdiction == c,]$ban_repealed)) { 
      
      pdat[pdat$Jurisdiction == c,][k,] <- ban %>% filter(Jurisdiction == c) %>% filter(row_number() == k) %>%
        mutate(sunsalesban = ifelse(ban_repealed == 1, 0, 
                                    ifelse(local_option == 1 | beer_excepted == 1, 0.5, 
                                           ifelse(local_option == 0 & beer_excepted == 0, 1, NA))),
               sunsalesban_exloc = ifelse(ban_repealed == 1, 0, 
                                          ifelse(beer_excepted == 1, 0.5, 
                                                 ifelse(beer_excepted == 0 | local_option == 0, 1, 
                                                        ifelse(local_option == 1, NA, NA)))))
      k <- k+1
    
    }
  }
  
  # Sunday sales ban was not repealed
  else if (0 %in% ban[ban$Jurisdiction == c,]$ban_repealed) {
    
    k <- 1 
    
    for (k in 1:length(ban[ban$Jurisdiction == c,]$ban_repealed)) { 
      
      pdat[pdat$Jurisdiction == c,][k,] <- ban %>% filter(Jurisdiction == c) %>% filter(row_number() == k) %>%
        mutate(sunsalesban = ifelse(local_option == 1 | beer_excepted == 1, 0.5, 
                                    ifelse(local_option == 0 & beer_excepted == 0, 1, NA)),
               sunsalesban_exloc = ifelse(local_option == 1, NA,
                                          ifelse(beer_excepted == 1, 0.5, 
                                                 ifelse(beer_excepted == 0 | local_option == 0, 1, NA))))
      k <- k+1
      
    }
    
  }
  
  else {
    
    pdat[pdat$Jurisdiction == c,] <- ban %>% filter(Jurisdiction == c) %>% mutate(sunsalesban <- NA)
    
  }
}

# MERGE SUNDAY SALES BAN WITH ALL STATES FILE

data$sunsalesban <- NA
data$sunsalesban_exloc <- NA
data$DatePolicy <- NA

for (c in unique(data$state)) {
  
  # State with change in Sunday sales ban
  if (c %in% unique(pdat$Jurisdiction)) {
    
      if (length(pdat[pdat$Jurisdiction==c,]$start_year) == 2) {
        
        # define year of policy change and increase by +1 if policy was changed in second half of the year 
        change_year <- max(pdat[pdat$Jurisdiction==c,]$start_year)
        change_year <- ifelse(pdat[pdat$Jurisdiction==c & pdat$start_year==change_year,]$start_month > 6, change_year + 1, change_year)
          
        data[data$state == c & data$year < change_year, ]$sunsalesban <- 
          pdat[pdat$Jurisdiction==c & pdat$start_year < max(pdat[pdat$Jurisdiction==c,]$start_year),]$sunsalesban 
        data[data$state == c & data$year >= change_year, ]$sunsalesban <- 
          pdat[pdat$Jurisdiction==c & pdat$start_year >= max(pdat[pdat$Jurisdiction==c,]$start_year),]$sunsalesban   
        
        data[data$state == c & data$year < change_year, ]$sunsalesban_exloc <- 
          pdat[pdat$Jurisdiction==c & pdat$start_year < max(pdat[pdat$Jurisdiction==c,]$start_year),]$sunsalesban_exloc 
        data[data$state == c & data$year >= change_year, ]$sunsalesban_exloc <- 
          pdat[pdat$Jurisdiction==c & pdat$start_year >= max(pdat[pdat$Jurisdiction==c,]$start_year),]$sunsalesban_exloc   
        
        data[data$state == c, ]$DatePolicy <- 
          unique(pdat[pdat$Jurisdiction==c & pdat$start_year >= max(pdat[pdat$Jurisdiction==c,]$start_year), ]$DatePolicy)
      }
      
      # State without change in Sunday sales ban
      else if (length(pdat[pdat$Jurisdiction==c,]$start_year) == 1) {
        
        data[data$state == c,]$sunsalesban <- pdat[pdat$Jurisdiction==c,]$sunsalesban
        data[data$state == c,]$sunsalesban_exloc <- pdat[pdat$Jurisdiction==c,]$sunsalesban_exloc
        data[data$state == c, ]$DatePolicy <- NA
      }
      
      
      else {
        
        data[data$state == c,]$sunsalesban <- NA
        data[data$state == c,]$sunsalesban_exloc <- NA
        data[data$state == c, ]$DatePolicy <- NA
      }
  }
    
  # State without change in Sunday sales ban
  else if (!c %in% unique(pdat$Jurisdiction)) {
    
    data[data$state == c,]$sunsalesban <- 0
    data[data$state == c,]$sunsalesban_exloc <- 0
    data[data$state == c, ]$DatePolicy <- NA
  } 
    
  else {
    
    data[data$state == c,]$sunsalesban <- NA
    data[data$state == c,]$sunsalesban_exloc <- NA
    data[data$state == c, ]$DatePolicy <- NA
    
  }
}

# --------------------------------------------------------------------------------------

# CONTROL STATES (based on Kerr, WC, Patterson, D, Williams, E (2023) Alcoholic Beverage Revenues and Taxes: 2020 Report, National Alcohol Beverage Association (NABCA), Arlington, VA)
data <- data %>% 
  mutate(controlstate = ifelse(state %like% "Alabama|Idaho|Iowa|Maine|Michigan|Mississippi|Montana|New Hampshire|North Carolina|Ohio|Oregon|Pennsylvania|Utah|Vermont|Virginia|West Virginia|Wyoming", 1, 0),
         controlstate = ifelse(state %like% "Washington" & year < 2012, 1, controlstate)) # Washington changed from control to license state

# --------------------------------------------------------------------------------------

# DRINKING CULTUR (Kerr 2010: http://www.mdpi.com/1660-4601/7/1/269)
data <- data %>% mutate(drinkculture = ifelse(state %like% "Alaska|Colorado|Illinois|Iowa|Kansas|Michigan|Minnesota|Montana|Missouri|Ohio|Nebraska|Dakota|Wisconsin|Wyoming", "wet",
                                              ifelse(state %like% "Maine|Massachusetts|Hampshire|Rhode Island|Vermont", "wet", 
                                                     ifelse(state %like% "Connecticut|Delaware|Maryland|New Jersey|New York|Pennsylvania", "moderate",
                                                            ifelse(state %like% "California|Hawaii|Idaho|Nevada|Oregon|Washington", "moderate",
                                                                   ifelse(state %like% "Arizona|Florida|Louisiana|New Mexico|South Carolina|Texas", "moderate",
                                                                          ifelse(state %like% "Alabama|Arkansas|Georgia|Indiana|Kentucky|Mississippi|North Carolina|Oklahoma|Tennessee|Utah|Virginia", "dry", NA)))))))

# --------------------------------------------------------------------------------------

# EXPORT

write.csv(data, paste0('/Users/carolinkilian/Desktop/SIMAH_workplace/acp_brfss/data/', DATE, "_ALCPOLICY_2019.csv"), row.names=FALSE)
