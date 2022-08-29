# Create a new variable of general alcohol status, combining information from ALCSTAT1 and ALCSTAT2:

generate_ALCSTAT  <- function(data){
  
  data <- data %>% 
    
    mutate(ALCSTAT = dplyr::case_when(
      
    ALCSTAT1 == 1 | ALCSTAT2 == 10| ALCSTAT2 == 40 | ALCSTAT2 == 41 | ALCSTAT2 == 42~ 1, # Lifetime abstainer (1/10) or very infrequent drinker (40/41/42)
    ALCSTAT1 == 2 | ALCSTAT2 == 20 | ALCSTAT2 == 21 | ALCSTAT2 == 22 | ALCSTAT2 == 23 ~ 2, # Former drinker
    ALCSTAT1 == 3 | ALCSTAT2 == 30 | ALCSTAT2 == 31 | ALCSTAT2 == 32 | ALCSTAT2 == 33 | ALCSTAT2 == 34 | ALCSTAT2 == 35 ~ 3)) # Current drinker
  
  return(data)
}

# Check face validity/ that no conflicts:
nhis_subset1_alcstat %>% count(ALCSTAT, ALCSTAT1, ALCSTAT2)