# This script assigns alcohol use categories based on grams per day (WHO)classification

recode_alc_cats <- function(data){
          
  data %>% mutate(
     
    # 4 categories   
    alc_4_cats = dplyr::case_when(
      
      # Females
      SEX==2 & (ALCSTAT1 == 1 | ALCSTAT1 == 2)    ~ 1,  # Abstainers (lifetime abstainers and former drinkers combined)
      SEX==2 & alc_daily_g >0 & alc_daily_g <= 20 ~ 2,  # Category I
      SEX==2 & alc_daily_g >20 & alc_daily_g <=40 ~ 3,  # Category II
      SEX==2 & alc_daily_g >40                    ~ 4,  # Category III
      
      # Males
      SEX==1 & (ALCSTAT1 == 1 | ALCSTAT1 == 2)    ~ 1,  # Abstainers (lifetime abstainers and former drinkers combined)
      SEX==1 & alc_daily_g >0 & alc_daily_g <= 40 ~ 2,  # Category I
      SEX==1 & alc_daily_g >40 & alc_daily_g <=60 ~ 3,  # Category II
      SEX==1 & alc_daily_g >60                    ~ 4), # Category III  
    
    # 5 categories
    alc_5_cats = dplyr::case_when(

      # Females
      SEX==2 & ALCSTAT1 == 1 ~ 1,                        # Lifetime abstainer
      SEX==2 & ALCSTAT1 == 2 ~ 2,                        # Former drinker
      SEX==2 & alc_daily_g >0 & alc_daily_g <= 20 ~ 3,  # Category I
      SEX==2 & alc_daily_g >20 & alc_daily_g <=40 ~ 4,  # Category II
      SEX==2 & alc_daily_g >40                    ~ 5,  # Category III

      # Males
      SEX==1 & ALCSTAT1 == 1 ~ 1,                        # Lifetime abstainer
      SEX==1 & ALCSTAT1 == 2 ~ 2,                        # Former drinker
      SEX==1 & alc_daily_g >0 & alc_daily_g <= 40 ~ 3,  # Category I
      SEX==1 & alc_daily_g >40 & alc_daily_g <=60 ~ 4,  # Category II
      SEX==1 & alc_daily_g >60                    ~ 5), # Category III

  # 6 categories
    alc_6_cats = dplyr::case_when(

      # Females
      SEX==2 & ALCSTAT1 == 1 ~ 1,                        # Lifetime abstainer
      SEX==2 & ALCSTAT1 == 2 ~ 2,                        # Former drinker
      SEX==2 & alc_daily_g >0 & alc_daily_g <= 20 ~ 3,  # Category I
      SEX==2 & alc_daily_g >20 & alc_daily_g <=40 ~ 4,  # Category II
      SEX==2 & alc_daily_g >40 & alc_daily_g <=60 ~ 5,  # Category III
      SEX==2 & alc_daily_g >60                    ~ 6,  # Category IV

      # Males
      SEX==1 & ALCSTAT1 == 1 ~ 1,                        # Lifetime abstainer
      SEX==1 & ALCSTAT1 == 2 ~ 2,                        # Former drinker
      SEX==1 & alc_daily_g >0 & alc_daily_g <= 40 ~ 3,  # Category I
      SEX==1 & alc_daily_g >40 & alc_daily_g <=60 ~ 4,  # Category II
      SEX==1 & alc_daily_g >60 & alc_daily_g <=100~ 5,  # Category III
      SEX==1 & alc_daily_g >100                   ~ 6), # Category IV
  )
}  

