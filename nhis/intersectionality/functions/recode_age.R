## Script for the function to recode age into 3 categories:

recode_age <- function(data){
  
  data <- data %>% mutate(
    
    # 3 categories
    age_3_cats = dplyr::case_when(
      
      AGE <=24 ~ 1, # Adolesents and young adults (18-24)
      AGE > 24 & AGE <=69 ~ 2, # Adults (25-69)
      AGE > 69 ~ 3), # Older adults (70-99)
    
    age_diaz = dplyr::case_when(
      AGE <21 ~ 0,
      AGE >=21 & AGE <=24 ~ 1,
      AGE >24 & AGE <60 ~ 2,
      AGE >=60 ~3),
  
    # 3 categories
    age_3_cats_uneven_splits = dplyr::case_when(
      
      AGE <=21 ~ 1, # Under legal drinking age (18-20)
      AGE > 20 & AGE <=70 ~ 2, # Adults (21-69)
      AGE > 70 ~ 3), # Older adults (70-99)
    
    age_4_cats = dplyr::case_when(
    
    AGE <=20 ~ 1, # 18-20
    AGE >= 21 & AGE <=23 ~ 2, # Young adults (21-23)
    AGE >= 24 & AGE <=69 ~ 3, # Adults (24-69)
    AGE >=70 ~ 4) # Older adults (70-99)
  )
    
  return(data)
}
