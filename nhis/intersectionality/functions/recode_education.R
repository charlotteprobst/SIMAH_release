# Function to recode education in NHIS dataset

recode_education <- function(data){
  
  data <- data %>% mutate(
  
  # 5 categories
   education_5_cats = dplyr::case_when(
        
      EDUCREC2 == 10|EDUCREC2 == 20|EDUCREC2 == 30| EDUCREC2 == 31|EDUCREC2 == 32 ~ 1, # no high school (<= grade 8)
      EDUCREC2 == 40 | EDUCREC2 == 41 ~ 2, # Some high school (grades 9-11)
      EDUCREC2 == 42 ~ 3, # Finished high school (grade 12)
      EDUCREC2 == 50 | EDUCREC2 == 51 ~ 4, # Some college
      EDUCREC2 == 54 | EDUCREC2 == 60 ~ 5), # 4+ years college
   
   # 4 categories
   education_4_cats = dplyr::case_when(
     
     EDUCREC2 == 10|EDUCREC2 == 20|EDUCREC2 == 30| EDUCREC2 == 31|EDUCREC2 == 32 ~ 1, # no high school (<= grade 8)
     EDUCREC2 == 40 |EDUCREC2 == 41|EDUCREC2 == 42 ~ 2, # Some high school (incl. those who graduated)
     EDUCREC2 == 50 |EDUCREC2 == 51 ~ 3, # Some college
     EDUCREC2 == 54 | EDUCREC2 == 60 ~ 4), # 4+ years college
  
  # 3 categories
  education_3_cats = dplyr::case_when(
    
    EDUCREC2 == 10|EDUCREC2 == 20|EDUCREC2 == 30|EDUCREC2 == 31|EDUCREC2 == 32|EDUCREC2 == 41|EDUCREC2 == 42 ~ 1, # High school or less
    EDUCREC2 == 50 |EDUCREC2 == 51 ~ 2, # Some college
    EDUCREC2 == 54 | EDUCREC2 == 60 ~ 3), # 4+ years college
  )
             return(data)
}
