# Function to recode race in NHIS dataset

recode_race_ethnicity <- function(data){
  
    data <- data %>% mutate(
      
      race_5_cats = dplyr::case_when(
        
        HISPYN == 1 & RACENEW == 100 ~ 1, # Non-hispanic, White
        HISPYN == 1 & RACENEW == 200 ~ 2, # Non-hispanic, Black/African American
        HISPYN == 1 & RACENEW == 400 ~ 3, # Non-hispanic, Asian
        HISPYN == 1 & (RACENEW == 500 | RACENEW == 510 | RACENEW == 520 | RACENEW == 530 | 
                     RACENEW == 540 | RACENEW == 541 | RACENEW == 542 | RACENEW == 300) ~ 4, # Non-hispanic, Other 
        HISPYN == 2  ~ 5) # Hispanic

)
             return(data)
}

# Key:

# HISPYN:
# 1 Not hispanic
# 2 hispanic

# Racenew:
# 100	White only	
# 200	Black/African American only	
# 300	American Indian/Alaska Native only	
# 400	Asian only	
# 500	Other Race and Multiple Race	
# 510	Other Race and Multiple Race (2019-forward: Excluding American Indian/Alaska Native)	
# 520	Other Race
# 530	Race Group Not Releasable	
# 540	Multiple Race	
# 541	Multiple Race (1999-2018: Including American Indian/Alaska Native)
# 542	American Indian/Alaska Native and Any Other Race
