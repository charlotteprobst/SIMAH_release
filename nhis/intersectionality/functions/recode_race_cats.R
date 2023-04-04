# Function to recode race in NHIS dataset to groups that contain >1% of total data

recode_race_cats <- function(data){
  
    data <- data %>% mutate(
      
      race_4_cats = dplyr::case_when(
        
        race == 1 ~ 1, # Non-hispanic, White
        race == 2 ~ 2, # Non-hispanic, Black/African American
        race == 12 ~ 3,# Hispanic,  White
        race == 3 | race == 4 | race == 5 | race == 6 | race == 7| race == 8| race == 9 | 
        race == 10 | race == 11 | race == 13 | race == 14 | 
        race == 15 | race == 16 | race == 17 | race == 18 | race == 19 |
        race == 20 | race == 21 | race == 22 ~ 4), # Other

      race_5_cats = dplyr::case_when(
        
        race == 1 ~ 1, # Non-hispanic, White
        race == 2 ~ 2, # Non-hispanic, Black/African American
        race == 12 ~ 3,# Hispanic,  White
        race == 4 ~ 4, # Non-hispanic, Asian
        race == 3 | race == 5 | race == 6 | race == 7| race == 8| race == 9 | 
        race == 10 | race == 11 | race == 13 | race == 14 | 
        race == 15 | race == 16 | race == 17 | race == 18 | race == 19 |
        race == 20 | race == 21 | race == 22 ~ 5) # Other
      
)
             return(data)
}

