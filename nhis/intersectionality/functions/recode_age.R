## Script for the function to recode age into 3 categories:

recode_age <- function(data){
  
  data <- data %>% mutate(
    
    # 3 categories
    age_3_cats = dplyr::case_when(
      
      AGE <=24 ~ 1, # Adolescents and young adults (18-24)
      AGE > 24 & AGE <=69 ~ 2, # Adults (25-69)
      AGE > 69 ~ 3), # Older adults (70-99)
    
    age_diaz = dplyr::case_when(
      AGE <21 ~ 0,
      AGE >=21 & AGE <=24 ~ 1,
      AGE >24 & AGE <60 ~ 2,
      AGE >=60 ~3)
  
  )
    
  return(data)
}
