setup_education_model_2021 <- function(data){
  data$educNUM <- ifelse(data$education<=12, 1,
                                ifelse(data$education==13, 2,
                                       ifelse(data$education==14,3,
                                              ifelse(data$education==15,4,
                                                     ifelse(data$education>=16,5,NA)))))
  
  data <- data %>% drop_na(sex,age,education,final_race_using_method_hierarchy)
  
# remove anyone with only one year of data- this gives an error in MSM 
  data <- data %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
    filter(totalobservations>1) 
# %>% mutate(sex=factor(sex), sex=ifelse(sex=="female",1,0)) # Sex already a factor
# all data needs to be ordered by ID then year
  data <- as.data.frame(lapply(data, unlist))
  data <- data[order(data$newID, data$year),]
  data$sex <- as.factor(data$sex)
  data$highestEd <- data$education
  source("SIMAH_code/education_transitions/2_analysis/cleaning_education_function2.R")
  backIDs <- getIDs(data) # Get the IDs of anyone who transitions backwards and remove those observations. 
  data <- data[!data$newID %in% backIDs,]
  data <- data %>% filter(age>=18)
  data$age <- round(data$age, digits=0)
  data$agesq <- data$age^2
  data$agescaled <- as.numeric(scale(data$age, center=T))  # scale the age data (generate a distribution with a mean of 0 and a standard deviation of 1.)
  data$agesqscaled <- as.numeric(scale(data$agesq, center=T)) # scale the age-squared data
  data$racefinal2 <- as.character(data$final_race_using_method_hierarchy) # Comment out when running stratified models
  data$racefinal2 <- ifelse(data$racefinal2=="Asian/PI","other",data$racefinal2) # Comment out when running stratified models
  data$racefinal2 <- ifelse(data$racefinal2=="Native","other",data$racefinal2) # Comment out when running stratified models
  data$racefinal2 <- as.factor(data$racefinal2) # Comment out when running stratified models
  data$racefinal2 <- relevel(data$racefinal2, ref = "white") # Comment out when running stratified models
  data <- data.frame(data)
  data <- as.data.frame(lapply(data, unlist))
# data$incomescaled <- as.numeric(scale(data$total_fam_income, center=T)) # income not in current dataset
  data <- data[order(data$newID, data$year),]
  return(data)
}
