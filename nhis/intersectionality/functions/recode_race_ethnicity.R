# Function to recode race in NHIS dataset

recode_race_ethnicity <- function(data){
  
    data <- data %>% mutate(
      
      race_5_cats = dplyr::case_when(
        
        HISPYN == 1 & RACENEW == 100 ~ 1, # Non-hispanic, White
        HISPYN == 1 & RACENEW == 200 ~ 2, # Non-hispanic, Black/African American
        HISPYN == 1 & RACENEW == 400 ~ 3, # Non-hispanic, Asian
        HISPYN == 1 & (RACENEW == 520 | RACENEW == 530 | RACENEW == 541 | RACENEW == 300) ~ 4, # Non-hispanic, Other 
        HISPYN == 2  ~ 5) # Hispanic

)
             return(data)
}



recode_race_ethnicity_all <- function(data){
  
  data <- data %>% mutate(
    
    race_ethnicity = dplyr::case_when(
      
      HISPYN == 1 & RACENEW == 100 ~ 1, # Non-hispanic, White only*
      HISPYN == 1 & RACENEW == 200 ~ 2, # Non-hispanic, Black/African American only
      HISPYN ==1  & RACENEW == 300 ~ 3, #	American Indian/Alaska Native only	
      HISPYN == 1 & RACENEW == 400 ~ 4, # Non-hispanic, Asian only
      HISPYN == 1 & RACENEW == 520 ~ 5, # Non-Hispanic, Other race *
      HISPYN == 1 & RACENEW == 530 ~ 6, # Non-Hispanic, Race group not releasable
      HISPYN == 1 & RACENEW == 541 ~ 7, # Non-hispanic, Multiple race *
      HISPYN == 2 & RACENEW == 100 ~ 8, # Hispanic, White only
      HISPYN == 2 & RACENEW == 200 ~ 9, # Hispanic, Black/African American only*
      HISPYN == 2  & RACENEW == 300 ~ 10, #	Hispanic, American Indian/Alaska Native only	
      HISPYN == 2 & RACENEW == 400 ~ 11, # Hispanic, Asian only*
      HISPYN == 2 & RACENEW == 520 ~ 12, # Hispanic, Other race 
      HISPYN == 2 & RACENEW == 530 ~ 13, # Hispanic, Race group not releasable
      HISPYN == 2 & RACENEW == 541 ~ 14, # Hispanic, Multiple race *
  
  ))
  return(data)
}

# Key:

# HISPYN:
# 1 Not hispanic
# 2 hispanic

# Racenew (codes used in the selected years):
# 100	White only	
# 200	Black/African American only	
# 300	American Indian/Alaska Native only	
# 400	Asian only	
# 520	Other Race
# 530	Race Group Not Releasable	
# 541	Multiple Race (1999-2018: Including American Indian/Alaska Native)

