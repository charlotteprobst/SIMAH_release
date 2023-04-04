# A function to assign labels to intersectional groups # 

recode_intersectional_order <- function(data){
  
  data <- data %>% mutate(
    
    intersectional_order = dplyr::case_when(
        
      # Females, 18-24
      intersectional_group == "F, 18-24, Low, Black " ~ 1, 
      intersectional_group == "F, 18-24, Low, Hisp. " ~ 2, 
      intersectional_group == "F, 18-24, Low, Other " ~ 3,
      intersectional_group == "F, 18-24, Low, White " ~ 4,         

      intersectional_group == "F, 18-24, Med, Black " ~ 5, 
      intersectional_group == "F, 18-24, Med, Hisp. " ~ 6, 
      intersectional_group == "F, 18-24, Med, Other " ~ 7,
      intersectional_group == "F, 18-24, Med, White " ~ 8, 
        
      intersectional_group == "F, 18-24, High, Black " ~ 9, 
      intersectional_group == "F, 18-24, High, Hisp. " ~ 10, 
      intersectional_group == "F, 18-24, High, Other " ~ 11,
      intersectional_group == "F, 18-24, High, White " ~ 12, 


      # Females, 25-59
      intersectional_group == "F, 25-59, Low, Black " ~ 13, 
      intersectional_group == "F, 25-59, Low, Hisp. " ~ 14, 
      intersectional_group == "F, 25-59, Low, Other " ~ 15,
      intersectional_group == "F, 25-59, Low, White " ~ 16,         
      
      intersectional_group == "F, 25-59, Med, Black " ~ 17, 
      intersectional_group == "F, 25-59, Med, Hisp. " ~ 18, 
      intersectional_group == "F, 25-59, Med, Other " ~ 19,
      intersectional_group == "F, 25-59, Med, White " ~ 20, 
      
      intersectional_group == "F, 25-59, High, Black " ~ 21, 
      intersectional_group == "F, 25-59, High, Hisp. " ~ 22, 
      intersectional_group == "F, 25-59, High, Other " ~ 23,
      intersectional_group == "F, 25-59, High, White " ~ 24, 
        
      # Females, 60+
      intersectional_group == "F, 60+, Low, Black " ~ 25, 
      intersectional_group == "F, 60+, Low, Hisp. " ~ 26, 
      intersectional_group == "F, 60+, Low, Other " ~ 27,
      intersectional_group == "F, 60+, Low, White " ~ 28,         
      
      intersectional_group == "F, 60+, Med, Black " ~ 29, 
      intersectional_group == "F, 60+, Med, Hisp. " ~ 30, 
      intersectional_group == "F, 60+, Med, Other " ~ 31,
      intersectional_group == "F, 60+, Med, White " ~ 32, 
      
      intersectional_group == "F, 60+, High, Black " ~ 33, 
      intersectional_group == "F, 60+, High, Hisp. " ~ 34, 
      intersectional_group == "F, 60+, High, Other " ~ 35,
      intersectional_group == "F, 60+, High, White " ~ 36, 
        
      # Males, 18-24
      intersectional_group == "M, 18-24, Low, Black " ~ 37, 
      intersectional_group == "M, 18-24, Low, Hisp. " ~ 38, 
      intersectional_group == "M, 18-24, Low, Other " ~ 39,
      intersectional_group == "M, 18-24, Low, White " ~ 40,         
      
      intersectional_group == "M, 18-24, Med, Black " ~ 41, 
      intersectional_group == "M, 18-24, Med, Hisp. " ~ 42, 
      intersectional_group == "M, 18-24, Med, Other " ~ 43,
      intersectional_group == "M, 18-24, Med, White " ~ 44, 
      
      intersectional_group == "M, 18-24, High, Black " ~ 45, 
      intersectional_group == "M, 18-24, High, Hisp. " ~ 46, 
      intersectional_group == "M, 18-24, High, Other " ~ 47,
      intersectional_group == "M, 18-24, High, White " ~ 48, 
      
      
      # Males, 25-59
      intersectional_group == "M, 25-59, Low, Black " ~ 49, 
      intersectional_group == "M, 25-59, Low, Hisp. " ~ 50, 
      intersectional_group == "M, 25-59, Low, Other " ~ 51,
      intersectional_group == "M, 25-59, Low, White " ~ 52,         
      
      intersectional_group == "M, 25-59, Med, Black " ~ 53, 
      intersectional_group == "M, 25-59, Med, Hisp. " ~ 54, 
      intersectional_group == "M, 25-59, Med, Other " ~ 55,
      intersectional_group == "M, 25-59, Med, White " ~ 56, 
      
      intersectional_group == "M, 25-59, High, Black " ~ 57, 
      intersectional_group == "M, 25-59, High, Hisp. " ~ 58, 
      intersectional_group == "M, 25-59, High, Other " ~ 59,
      intersectional_group == "M, 25-59, High, White " ~ 60, 
      
      # Males, 60+
      intersectional_group == "M, 60+, Low, Black " ~ 61, 
      intersectional_group == "M, 60+, Low, Hisp. " ~ 62, 
      intersectional_group == "M, 60+, Low, Other " ~ 63,
      intersectional_group == "M, 60+, Low, White " ~ 64,         
      
      intersectional_group == "M, 60+, Med, Black " ~ 65, 
      intersectional_group == "M, 60+, Med, Hisp. " ~ 66, 
      intersectional_group == "M, 60+, Med, Other " ~ 67,
      intersectional_group == "M, 60+, Med, White " ~ 68, 
      
      intersectional_group == "M, 60+, High, Black " ~ 69, 
      intersectional_group == "M, 60+, High, Hisp. " ~ 70, 
      intersectional_group == "M, 60+, High, Other " ~ 71,
      intersectional_group == "M, 60+, High, White " ~ 72)
      
      )
      
      return(data)
}
      
      
      