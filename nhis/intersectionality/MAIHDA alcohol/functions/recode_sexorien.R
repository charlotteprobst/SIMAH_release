# Function to recode sexual orientation in NHIS dataset

recode_sexorien <- function(data){
  
  data <- data %>% mutate(
    
    SEXORIEN = dplyr::case_when(
      
      SEXORIEN == 2 ~ 1, # Heterosexual
      SEXORIEN == 1 | SEXORIEN == 3 | SEXORIEN == 4 ~ 2), # Homosexual/bisexual/something else
)
      return(data)
}