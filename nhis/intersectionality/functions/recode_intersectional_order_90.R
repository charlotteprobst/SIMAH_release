# A function to assign labels to intersectional groups # 

recode_intersectional_order <- function(data){
  
  data <- data %>% mutate(
    
    intersectional_order = dplyr::case_when(
        
      # Females, 18-24
      intersectional_group == "Female 18-24 Other high school or less" ~ 1,
      intersectional_group == "Female 18-24 Asian high school or less" ~ 2, 
      intersectional_group == "Female 18-24 Hispanic, White high school or less" ~ 3,
      intersectional_group == "Female 18-24 Black/African American high school or less" ~ 4, 
      intersectional_group == "Female 18-24 White high school or less" ~ 5,
      
      intersectional_group == "Female 18-24 Other some college" ~ 6,      
      intersectional_group == "Female 18-24 Asian some college" ~ 7, 
      intersectional_group == "Female 18-24 Hispanic, White some college" ~ 8,
      intersectional_group == "Female 18-24 Black/African American some college" ~ 9, 
      intersectional_group == "Female 18-24 White some college" ~ 10,
      
      intersectional_group == "Female 18-24 Other 4+ years college" ~ 11,      
      intersectional_group == "Female 18-24 Asian 4+ years college" ~ 12, 
      intersectional_group == "Female 18-24 Hispanic, White 4+ years college" ~ 13,
      intersectional_group == "Female 18-24 Black/African American 4+ years college" ~ 14, 
      intersectional_group == "Female 18-24 White 4+ years college" ~ 15,
      
      
      # Females, 25-59
     
        
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
      
      
      