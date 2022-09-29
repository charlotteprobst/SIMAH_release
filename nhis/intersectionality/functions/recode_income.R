# Function to recode race in NHIS dataset to groups that contain >1% of total data

recode_income <- function(data){
  
    data <- data %>% mutate(
      
      income = dplyr::case_when(
        
        INCFAM97ON2 == 10 ~ 1, # $0 - $34,999
        INCFAM97ON2 == 20 ~ 2, # $35,000-$74,999
        INCFAM97ON2 == 31 ~ 3, # $75,000-$99,999
        INCFAM97ON2 == 32 ~ 4), # $100,000 and over
)
             return(data)
}

