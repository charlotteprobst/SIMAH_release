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
      HISPYN == 1 & RACENEW == 520 ~ 5, # Non-Hispanic, Other race* (excluded below)
      HISPYN == 1 & RACENEW == 530 ~ 6, # Non-Hispanic, Race group not releasable* (excluded below)
      HISPYN == 1 & RACENEW == 541 ~ 7, # Non-hispanic, Multiple race
      HISPYN == 2 & RACENEW == 100 ~ 8, # Hispanic, White only
      HISPYN == 2 & RACENEW == 200 ~ 9, # Hispanic, Black/African American only
      HISPYN == 2  & RACENEW == 300 ~ 10, #	Hispanic, American Indian/Alaska Native only	
      HISPYN == 2 & RACENEW == 400 ~ 11, # Hispanic, Asian only
      HISPYN == 2 & RACENEW == 520 ~ 12, # Hispanic, Other race 
      HISPYN == 2 & RACENEW == 530 ~ 13, # Hispanic, Race group not releasable
      HISPYN == 2 & RACENEW == 541 ~ 14), # Hispanic, Multiple race 
  
  race_6_cats = case_when(
    race_ethnicity==1 ~ "NH White",
    race_ethnicity==2 ~ "NH Black",
    race_ethnicity==4 ~ "NH Asian",
    race_ethnicity==7 ~ "NH Multiple race",
    race_ethnicity==3 ~ "NH AI/AN",
  race_ethnicity==8|race_ethnicity==9|race_ethnicity==10|race_ethnicity==11|
    race_ethnicity==12|race_ethnicity==13|race_ethnicity==14~ "Hispanic")
  )
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

