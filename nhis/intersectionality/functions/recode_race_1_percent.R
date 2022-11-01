# Function to recode race in NHIS dataset to groups that contain >1% of total data

recode_race_1_percent <- function(data){
  
    data <- data %>% mutate(
      
      race_1_percent = dplyr::case_when(
        
        race == 1 ~ 1, # Non-hispanic, White
        race == 2 ~ 2, # Non-hispanic, Black/African American
        race == 4 ~ 3, # Non-hispanic, Asian
        race == 6 ~ 4, # Non-hispanic, Race Group Not Releasable
        race == 7 ~ 5, # Hispanic,  White
        race == 3|race == 5|race == 8|race == 9|race == 10|race == 11|race == 12 ~ 5), # Other

)
             return(data)
}

