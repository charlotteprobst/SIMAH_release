# Remove individuals with missing data 

remove_na <- function(data, variables_to_drop) {

    for (i in variables_to_drop) {
    
    data <- data %>% tidyr::drop_na(i)
  
  } 
  
  return (data)
}

