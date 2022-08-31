# Create a new variable of general alcohol status, combining information from ALCSTAT1 and ALCSTAT2:

generate_ALCSTAT  <- function(data){
  
  data <- data %>% 
    
    mutate(ALCSTAT = dplyr::case_when(
      
    ALCSTAT1 == 1 | ALCSTAT2 == 10 ~ 1, # Lifetime abstainer
    ALCSTAT1 == 2 | ALCSTAT2 == 21 | ALCSTAT2 == 22 | ALCSTAT2 == 23 ~ 2, # Former drinker
    ALCSTAT1 == 3 | ALCSTAT2 == 31 | ALCSTAT2 == 32 | ALCSTAT2 == 33 | ALCSTAT2 == 34 | ALCSTAT2 == 35 ~ 3)) # Current drinker
  
  return(data)
}
