setup_alcohol_model <- function(data){
  library(dplyr)

data <- data %>% drop_na(sex,age,education,final_race_using_method_hierarchy)
  
# remove anyone with only one year of data- this gives an error in MSM 
  data <- data %>% ungroup() %>% group_by(uniqueID) %>% add_tally(name="totalobservations") %>% 
    filter(totalobservations>1) 
  
# recategorise variables according to the categories for the model
  
  #Alc states
  data <- data %>% mutate(final_alc_cat = 
                            ifelse(final_alc_cat=="Very high risk", "High risk", final_alc_cat))
    data <- data %>%
    mutate(final_alc_cat = case_when(
      final_alc_cat == "Non-drinker" ~ 1,
      final_alc_cat == "Low risk" ~ 2,
      final_alc_cat == "Medium risk" ~ 3,
      final_alc_cat == "High risk" ~ 4,
      TRUE ~ NA_real_
    ))
  
# Education
data <- data %>% 
    mutate(education = case_when(
     education <= 12 ~ "Less than or equal to high school",
     education > 12 & education < 16 ~ "Some college",
     education >= 16 ~ "College +"
      ))
    
data <- data %>% 
    mutate(age_cat = case_when(
       age >= 18 & age <= 24 ~ "18-24",
       age >= 25 & age <= 64 ~ "25-64",
       age >= 65 ~ "65+"
     ))
    
# all data needs to be ordered by ID then year
  data <- as.data.frame(lapply(data, unlist))
  data <- data[order(data$uniqueID, data$year),]
  data$sex <- as.factor(data$sex)
  data$highestAlc <- data$final_alc_cat
  # source("SIMAH_code/alcohol_transitions_PSID/functions/0_identify_backward_alcohol_transitions.R")
  # backIDs <- getIDs(data) # Get the IDs of anyone who transitions backwards and remove those observations. 
  # data <- data[!data$uniqueID %in% backIDs,]
  data <- data %>% filter(age>=18)
  data$age <- round(data$age, digits=0)
  data$agesq <- data$age^2
  data$agescaled <- as.numeric(scale(data$age, center=T))  # scale the age data (generate a distribution with a mean of 0 and a standard deviation of 1.)
  data$agesqscaled <- as.numeric(scale(data$agesq, center=T)) # scale the age-squared data
  data$race <- as.character(data$final_race_using_method_hierarchy) 
  data$race <- ifelse(data$race=="Asian/PI","other",data$race) 
  data$race <- ifelse(data$race=="Native","other",data$race) 
  data$race <- as.factor(data$race) 
 data$race <- relevel(data$race, ref = "white") # Comment out when running race stratified models
  data <- data.frame(data)
  data <- as.data.frame(lapply(data, unlist))
# data$incomescaled <- as.numeric(scale(data$total_fam_income, center=T)) # income not in current dataset
  data <- data[order(data$uniqueID, data$year),]
  datat1$final_alc_cat <- factor(datat1$final_alc_cat, levels = c(1, 2, 3, 4))
  
  return(data)
}
