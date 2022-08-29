# This script assigns alcohol use categories based on grams per day (WHO)classification

# WORK IN PROGRESS # 

# 1) Four categories:
# non-drinker (no drinks in the past 12 months)
# category I (≤20g (women) or ≤40g (men))
# category II (21-40g (women) or 41-60g (men))
# and category III (≥41g (women) or ≥61g (men))



# 2) Five categories:
# non-drinker (no drinks in the past 12 months)
# category I (≤20g (women) or ≤40g (men))
# category II (21-40g (women) or 41-60g (men)
# category III (41-60g (women) or 61-100g (men)
# category IV (≥61g(women) or ≥101g (men))

recode_alc_cats <- function(data){
          
  data %>% mutate(
        
    alc_5_cats_grams = base::ifelse(SEX ==2 & alc_daily_g == 0 & ALCSTAT1 == 1, 1, 0))
    
    
                       elseif(SEX==2 & alc_daily_g == 0 & ALCSTAT1 == 2 ~ 2, # Former drinker
                        # Females
                        SEX==2 & alc_daily_g == 0 & ALCSTAT1 == 1 ~ 1, # Lifetime abstainer
                        SEX==2 & alc_daily_g >0 & alc_daily_g <= 20 ~ 3, # Category I
                        SEX==2 & alc_daily_g >20 & alc_daily_g <=40 ~ 4, # Category II
                        SEX==2 & alc_daily_g >40 & alc_daily_g <=60 ~ 5, # Category III
                        SEX==2 & alc_daily_g >60                    ~ 6, # Category IV),
  
 
  )
}
  
  

# 
#   
# # Create the Alcohol Use Categorical Variable
# alcohol6 = case_when(	
#   # Females
#   SEX==2 & alc_daily_g == 0 & ALCSTAT1 == 1 ~ 1, # Lifetime abstainer
#   SEX==2 & alc_daily_g == 0 & ALCSTAT1 == 2 ~ 2, # Former drinker
#   SEX==2 & alc_daily_g >0 & alc_daily_g <= 20 ~ 3, # Category I
#   SEX==2 & alc_daily_g >20 & alc_daily_g <=40 ~ 4, # Cateogry II
#   SEX==2 & alc_daily_g >40 & alc_daily_g <=60 ~ 5, # Category III
#   SEX==2 & alc_daily_g >60                    ~ 6, # Category IV
#   
#   # Males
#   SEX==2 & alc_daily_g == 0 & ALCSTAT1 == 1  ~ 1, # Lifetime abstainer
#   SEX==2 & alc_daily_g == 0 & ALCSTAT1 == 2  ~ 2, # Former drinker
#   SEX==2 & alc_daily_g >0 & alc_daily_g <= 40  ~ 3, # Category I
#   SEX==2 & alc_daily_g >40 & alc_daily_g <=60  ~ 4, # Cateogry II
#   SEX==2 & alc_daily_g >60 & alc_daily_g <=100 ~ 5, # Category III
#   SEX==2 & alc_daily_g >100                    ~6, # Category IV
#   TRUE ~ NA_real_)
# 
# alcohol5 = recode(alcohol6, `1`=1, `2`=2, `3`=3, `4`=4, `5`=5, `6`=5)  # merge category III and IV
# alcohol4 = recode(alcohol6, `1`=1, `2`=1, `3`=2, `4`=3, `5`=4, `6`=4)  # merge category III and IV, and abstainers/former drinkers
# 
# alc6 = factor(alcohol6, levels=c(3,1,2,4,5,6), labels=c("Category I", "Lifetime abstainer", "Former drinker", "Category II", "Category III", "Category IV")) # the first listed category is the reference
# alc5 = factor(alcohol5, levels=c(3,1,2,4,5), labels=c("Category I", "Lifetime abstainer", "Former drinker", "Category II", "Category III"))
# alc4 = factor(alcohol4, levels=c(2,1,2,4), labels=c("Category I", "Non-drinker", "Category II", "Category III"))
# 
# 
