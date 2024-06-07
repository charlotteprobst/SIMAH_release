# Script to generate Table 1 - Demographics of sample

setwd("C:/Users/cmp21seb/Documents/SIMAH/")

source("SIMAH_code/education_transitions/2021/functions/0_setup_education_model_2021.R")

data <- read_csv("SIMAH_workplace/education_transitions/2021/data_to_model/new_PSID_weighted_IDs_2021.csv")

data$agecat <- ifelse(data$age==18, "18",
                      ifelse(data$age==19, "19",
                             ifelse(data$age==20, "20",
                                    ifelse(data$age>=21 & data$age<=25, "21-25","26+"))))

# Convert 'agecat' to a factor with specified levels
data$agecat <- factor(data$agecat, levels = c("18", "19", "20", "21-25", "26+"))

# COVID period 2013-2018
data_COVID <- data %>% filter(year>=2013)
data_COVID <- setup_education_model_2021(datat3)

datat3 <- datat3 %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 