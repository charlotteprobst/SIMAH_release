# Function to recode race in NHIS dataset

recode_race <- function(data){
  
    data <- data %>% mutate(
      
      race = dplyr::case_when(
        
        hisp == 0 & RACENEW == 100 ~ 1, # Non-hispanic, White
        hisp == 0 & RACENEW == 200 ~ 2, # Non-hispanic, Black/African American
        hisp == 0 & RACENEW == 300 ~ 3, # Non-hispanic, American Indian/Alaska Native 
        hisp == 0 & RACENEW == 400 ~ 4, # Non-hispanic, Asian
        hisp == 0 & RACENEW == 530 ~ 5, # Non-hispanic, Multiple Race (Including American Indian/Alaska Native)
        hisp == 0 & RACENEW == 541 ~ 6, # Non-hispanic, Race Group Not Releasable
        
        hisp == 1 & RACENEW == 100 ~ 7,  # hispanic,  White
        hisp == 1 & RACENEW == 200 ~ 8,  # hispanic,  Black/African American
        hisp == 1 & RACENEW == 300 ~ 9,  # hispanic,  American Indian/Alaska Native 
        hisp == 1 & RACENEW == 400 ~ 10, # hispanic, Asian
        hisp == 1 & RACENEW == 530 ~ 11, # hispanic, Multiple Race (Including American Indian/Alaska Native)
        hisp == 1 & RACENEW == 541 ~ 12),# hispanic, Race Group Not Releasable
)
             return(data)
}

