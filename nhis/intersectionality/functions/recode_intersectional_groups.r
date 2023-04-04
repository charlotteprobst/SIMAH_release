# A function to assign labels to intersectional groups # 

recode_intersectional_groups <- function(data){
  
  data <- data %>% mutate(
    
      intersectional_group = dplyr::case_when(
      
      # Females, 18-24
      v1 == 875 ~ "F, 18-24, High, Hisp. ", 
      v1 == 1114 ~ "F, 18-24, High, Black ", 
      v1 == 156 ~ "F, 18-24, High, White ", 
      v1 == 3453 ~ "F, 18-24, High, Other ", 
      v1 == 132 ~ "F, 18-24, Low, Hisp. ", 
      v1 == 89 ~ "F, 18-24, Low, Black ", 
      v1 == 40 ~ "F, 18-24, Low, White ", 
      v1 == 1002 ~ "F, 18-24, Low, Other ",
      v1 == 730 ~ "F, 18-24, Med, Hisp. ", 
      v1 == 840 ~ "F, 18-24, Med, Black ", 
      v1 == 198 ~ "F, 18-24, Med, White ", 
      v1 == 186 ~ "F, 18-24, Med, Other ", 
      
      # Females, 25-59
      v1 == 28 ~ "F, 25-59, High, Hisp. ", 
      v1 == 48 ~ "F, 25-59, High, Black ", 
      v1 == 7 ~ "F, 25-59, High, White ", 
      v1 == 185 ~ "F, 25-59, High, Other ", 
      v1 == 46 ~ "F, 25-59, Low, Hisp. ", 
      v1 == 92 ~ "F, 25-59, Low, Black ", 
      v1 == 18 ~ "F, 25-59, Low, White ", 
      v1 == 114 ~ "F, 25-59, Low, Other ",
      v1 == 4 ~ "F, 25-59, Med, Hisp. ", 
      v1 == 83 ~ "F, 25-59, Med, Black ", 
      v1 == 1 ~ "F, 25-59, Med, White ", 
      v1 == 11 ~ "F, 25-59, Med, Other ", 
      
      # Females, 60+
      v1 == 1255 ~ "F, 60+, High, Hisp. ", 
      v1 == 125 ~ "F, 60+, High, Black ", 
      v1 == 26 ~ "F, 60+, High, White ", 
      v1 == 1755 ~ "F, 60+, High, Other ", 
      v1 == 124 ~ "F, 60+, Low, Hisp. ", 
      v1 == 229 ~ "F, 60+, Low, Black ", 
      v1 == 27 ~ "F, 60+, Low, White ", 
      v1 == 97 ~ "F, 60+, Low, Other ",
      v1 == 223 ~ "F, 60+, Med, Hisp. ", 
      v1 == 491 ~ "F, 60+, Med, Black ", 
      v1 == 66 ~ "F, 60+, Med, White ", 
      v1 == 4309 ~ "F, 60+, Med, Other ",
      
      # Males, 18-24
      v1 == 1236 ~ "M, 18-24, High, Hisp. ", 
      v1 == 1965 ~ "M, 18-24, High, Black ", 
      v1 == 19 ~ "M, 18-24, High, White ", 
      v1 == 661 ~ "M, 18-24, High, Other ", 
      v1 == 56 ~ "M, 18-24, Low, Hisp. ", 
      v1 == 8 ~ "M, 18-24, Low, Black ", 
      v1 == 149 ~ "M, 18-24, Low, White ", 
      v1 == 404 ~ "M, 18-24, Low, Other ",
      v1 == 364 ~ "M, 18-24, Med, Hisp. ", 
      v1 == 163 ~ "M, 18-24, Med, Black ", 
      v1 == 3 ~ "M, 18-24, Med, White ", 
      v1 == 115 ~ "M, 18-24, Med, Other ", 
      
      # Males, 25-59
      v1 == 15 ~ "M, 25-59, High, Hisp. ", 
      v1 == 33 ~ "M, 25-59, High, Black ", 
      v1 == 9 ~ "M, 25-59, High, White ", 
      v1 == 219 ~ "M, 25-59, High, Other ", 
      v1 == 12 ~ "M, 25-59, Low, Hisp. ", 
      v1 == 24 ~ "M, 25-59, Low, Black ", 
      v1 == 6 ~ "M, 25-59, Low, White ", 
      v1 == 44 ~ "M, 25-59, Low, Other ",
      v1 == 172 ~ "M, 25-59, Med, Hisp. ", 
      v1 == 10 ~ "M, 25-59, Med, Black ", 
      v1 == 2 ~ "M, 25-59, Med, White ", 
      v1 == 397 ~ "M, 25-59, Med, Other ", 
      
      # Males, 60+
      v1 == 815 ~ "M, 60+, High, Hisp. ", 
      v1 == 3218 ~ "M, 60+, High, Black ", 
      v1 == 39 ~ "M, 60+, High, White ", 
      v1 == 832 ~ "M, 60+, High, Other ", 
      v1 == 54 ~ "M, 60+, Low, Hisp. ", 
      v1 == 864 ~ "M, 60+, Low, Black ", 
      v1 == 37 ~ "M, 60+, Low, White ", 
      v1 == 180 ~ "M, 60+, Low, Other ",
      v1 == 495 ~ "M, 60+, Med, Hisp. ", 
      v1 == 1397 ~ "M, 60+, Med, Black ", 
      v1 == 50 ~ "M, 60+, Med, White ", 
      v1 == 177 ~ "M, 60+, Med, Other ")
      
      )
      
      return(data)
}
      
      
      