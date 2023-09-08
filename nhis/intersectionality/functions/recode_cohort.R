# Function to generate groups based on cohort

recode_cohort <- function(data){
  
  data <- data %>% mutate(
    
   birth_cohort = dplyr::case_when(
      
     birth_year_est <= 1945 ~ 1, # silent
     birth_year_est >= 1946 & birth_year_est <= 1964 ~ 2, # baby boomers
     birth_year_est >= 1965 & birth_year_est <= 1980 ~ 3, # gen_x
     birth_year_est >= 1981 & birth_year_est <= 1996 ~ 4,# millenials
     birth_year_est >= 1997 ~ 5), # gen_z
  )
  return(data)
}

