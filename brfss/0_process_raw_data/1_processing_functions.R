# demographic functions 

recode_state <- function(data){
  data <- data %>% 
    mutate(
    State = recode(X.STATE,
                   "1"="Alabama", "2"="Alaska", "4"="Arizona","5"="Arkansas",
    "6"="California", "8"="Colorado", "9"="Connecticut", "10"="Delaware",
    "12"="Florida","13"="Georgia","15"="Hawaii","16"="Idaho","17"="Illinois",
    "18"="Indiana","19"="Iowa","20"="Kansas","21"="Kentucky","22"="Louisiana",
    "23"="Maine","24"="Maryland", "25"="Massachusetts", "26"="Michigan",
    "27"="Minnesota", "28"="Mississippi", "29"="Missouri", "30"="Montana",
    "31"="Nebraska","32"="Nevada", "33"="New Hampshire", "34"="New Jersey",
    "35"="New Mexico", "36"="New York", "37"="North Carolina", "38"="North Dakota",
    "39"="Ohio", "40"="Oklahoma", "41"="Oregon", "42"="Pennsylvania", "44"="Rhode Island",
    "45"="South Carolina", "46"="South Dakota", "47"="Tennessee", "48"="Texas", "49"="Utah",
    "50"="Vermont", "51"="Virginia", "53"="Washington", "54"="West Virginia", "55"="Wisconsin",
    "56"="Wyoming", "11"="DC", "66"="Guam", "72"="Puerto Rico",
    .default="territories"))
  return(data)
}

recode_education <- function(data){
  data <- data %>% 
    mutate(
      education_detailed = recode(EDUCA, 
                         "1"="no school",
                         "2"="elementary school",
                         "3"="some high school",
                         "4"="high school graduate",
                         "5"="some college",
                         "6"="college graduate",
                         "9"="NA",.default="NA"),
      education_detailed = ifelse(education_detailed=="NA",NA,
                                  education_detailed),
      education_summary = ifelse(EDUCA<=4, "LEHS",
                                 ifelse(EDUCA==5,"SomeC",
                                        ifelse(EDUCA==6, "College",NA))))
  return(data)
}

recode_race <- function(data){
  data <- data %>% 
    mutate(hispanic_comb = ifelse(YEAR<=2000, HISPANIC,
                                  ifelse(YEAR>=2001 & YEAR<=2012, HISPANC2,
                                         ifelse(YEAR>=2013, X.HISPANC, NA))),
      hispanic_comb = ifelse(hispanic_comb==1, 1,
                             ifelse(hispanic_comb==2, 0, NA)),
      race_comb = ifelse(YEAR<2001, ORACE,
                              ifelse(YEAR>=2001 & YEAR<=2012, RACE2,
                                     ifelse(YEAR>=2013, X.RACE, NA))),
      race_eth_detailed = ifelse(YEAR<2001 & race_comb == 1 & hispanic_comb==0, "Non-Hispanic White",
                        ifelse(YEAR<2001 & race_comb==2 & hispanic_comb==0, "Non-Hispanic Black",
                               ifelse(YEAR<2001 & race_comb==3 & hispanic_comb==0, "Non-Hispanic Asian/PI",
                                      ifelse(YEAR<2001 & race_comb==4 & hispanic_comb==0, "Non-Hispanic Native American",
                                             ifelse(YEAR<2001 & race_comb==5 & hispanic_comb==0, "Non-Hispanic Other",
                                                    ifelse(YEAR<2001 & hispanic_comb==1, "Hispanic",
                                                           ifelse(YEAR<2001 & race_comb>=7, NA, NA))))))),
      race_eth_detailed = ifelse(YEAR>=2001 & race_comb ==1, "Non-Hispanic White",
                                 ifelse(YEAR>=2001 & race_comb==2, "Non-Hispanic Black",
                                        ifelse(YEAR>=2001 & YEAR<=2012 & race_comb==3, "Non-Hispanic Asian/PI",
                                               ifelse(YEAR>=2001 & YEAR<=2012 & race_comb==4, "Non-Hispanic Asian/PI",
                                                      ifelse(YEAR>=2001 & YEAR<=2012 & race_comb==5, "Non-Hispanic Native American",
                                                             ifelse(YEAR>=2001 & race_comb==6, "Non-Hispanic Other",
                                                                    ifelse(YEAR>=2001 & race_comb==7, "Non-Hispanic Other",
                                                                           ifelse(YEAR>=2001 & race_comb==8, "Hispanic",
                                                                                  ifelse(YEAR>2012 & race_comb==3, "Non-Hispanic Native American",
                                                                                         ifelse(YEAR>2012 & race_comb==4, "Non-Hispanic Asian/PI",
                                                                                                ifelse(YEAR>2012 & race_comb==5, "Non-Hispanic Asian/PI",
                                                                                                       race_eth_detailed))))))))))),
      race_eth = ifelse(race_eth_detailed == "Non-Hispanic Asian/PI", "Non-Hispanic Other",
                        ifelse(race_eth_detailed=="Non-Hispanic Native American", "Non-Hispanic Other",
                               race_eth_detailed))
      
      
    )
                                      
  return(data)
}

recode_age <- function(data){
  data <- data %>% 
    mutate(age_var = ifelse(YEAR<=2012, AGE, X.AGE80),
           age_var = ifelse(age_var<=9, NA, age_var))
  return(data)
}

recode_sex <- function(data){
  data <- data %>% mutate(
    sex_var = ifelse(YEAR==2018, SEX1, 
                     ifelse(YEAR>=2019, SEXVAR, SEX)),
    sex_recode = ifelse(sex_var== 1, "Men",
                        ifelse(sex_var==2, "Women", NA))
  )
  return(data)
}

recode_employment <- function(data){
  data <- data %>% 
    mutate(employment_var = ifelse(YEAR<=2012, EMPLOY,EMPLOY1),
           employment_detailed = ifelse(employment_var<=2, "employed",
                                        ifelse(employment_var==3, "unemployed - 1+ years",
                                               ifelse(employment_var==4, "unemployed - <1 year",
                                                      ifelse(employment_var==5, "homemaker",
                                                             ifelse(employment_var==6, "student",
                                                                    ifelse(employment_var==7, "retired",
                                                                           ifelse(employment_var==8, "unable to work",
                                                                                  ifelse(employment_var==9, NA, NA)))))))),
           employment = ifelse(employment_detailed=="unemployed - 1+ years", "unemployed",
                               ifelse(employment_detailed=="unemployed - <1 year", "unemployed",
                                      ifelse(employment_detailed=="homemaker", "unemployed",
                                             ifelse(employment_detailed=="student", "unemployed",
                                                    ifelse(employment_detailed=="retired", "unemployed",
                                                           ifelse(employment_detailed=="unable to work", "unemployed",
                                                                  employment_detailed)))))))
  return(data)
}

recode_income <- function(data){
  data <- data %>% mutate(
    household_income = ifelse(INCOME2==1, "0-9999",
                        ifelse(INCOME2==2, "10000-14999",
                               ifelse(INCOME2==3, "15000-19999",
                                      ifelse(INCOME2==4, "20000-24999",
                                             ifelse(INCOME2==5, "25000-34999",
                                                    ifelse(INCOME2==6, "35000-49999",
                                                           ifelse(INCOME2==7, "50000-74999",
                                                                  ifelse(INCOME2==8, "75000+", NA)))))))))
    
return(data)
}

recode_weight <- function(data){
  if(data$YEAR[1]<2004){
  data <- data %>% 
    mutate(
      weight_kg = ifelse(WEIGHT ==777, NA,
                          ifelse(WEIGHT==999, NA,
                                 ifelse(WEIGHT==9999, NA, 
                                        ifelse(WEIGHT==7777, NA,
                                               WEIGHT*0.4535)))))
  }else if(data$YEAR[1]>=2004){
    data <- data %>% 
      mutate(
        weight_kg = ifelse(WEIGHT2==7777, NA,
                           ifelse(WEIGHT2==9999, NA,
                                  ifelse(WEIGHT2>=9000 & WEIGHT2<=9998, WEIGHT2-9000,
                                         WEIGHT2*0.4535))))
  }
  data$weight_kg <- ifelse(data$weight_kg>=400, NA, data$weight_kg)
  return(data)
}

recode_height <- function(data){
  data <- data %>% mutate(
    heightvar = ifelse(YEAR<=2003, HEIGHT,
                       ifelse(YEAR==2004, HEIGHT2, 
                              ifelse(YEAR>=2005, HEIGHT3, NA)))
  )
  if(data$YEAR[1]<=2004){
    data <- data %>% mutate(
      heightvar = ifelse(heightvar==777, NA,
                         ifelse(heightvar==999, NA,
                                ifelse(heightvar==7777, NA, 
                                       ifelse(heightvar==9999, NA,
                                              heightvar)))),
      feet = as.numeric(substr(heightvar, 1,1)),
      inches = as.numeric(substr(heightvar, 2,3)),
      height_cm = feet*0.3048 + inches*0.0254
    )
  }else if(data$YEAR[1]>=2005){
    data <- data %>% mutate(
      heightvar = ifelse(heightvar==7777, NA, 
                         ifelse(heightvar==9999, NA, heightvar)),
      feet <- ifelse(heightvar<=711, as.numeric(substr(heightvar, 1,1)),NA),
      inches <- ifelse(heightvar<=711, as.numeric(substr(heightvar, 2,3)),NA),
      height_cm = ifelse(heightvar>=9000 & heightvar<=9998, heightvar-9000,
                         ifelse(heightvar<=711, feet*0.3048 + inches*0.0254, heightvar))
    )
  }
  data$height_cm <- ifelse(data$height_cm>300, NA, data$height_cm)
  return(data)
}

recode_BMI <- function(data){
  data <- data %>% mutate(
    BMI = weight_kg / ((height_cm)^2)
  )
}


# alcohol functions 

recode_alc_prevalence <- function(data){
  data <- data %>%
    mutate(drinkingstatus = ifelse(YEAR<=2000, DRINKANY,
                                   ifelse(YEAR>=2005 & YEAR<=2010, DRNKANY4, NA)))
  return(data)
}

recode_alc_frequency <- function(data){
  data <- data %>% mutate(
  alc_frequency = ifelse(YEAR<=2000, ALCOHOL,
                         ifelse(YEAR==2001, ALCDAYS,
                                ifelse(YEAR>=2002 & YEAR<=2004, ALCDAY3,
                                       ifelse(YEAR>=2005 & YEAR<=2010, ALCDAY4, 
                                              ifelse(YEAR>=2011, ALCDAY5, NA))))))
  if(data$YEAR[1]==1999 | data$YEAR[1]==2000){
    data <- data %>% mutate(
      alc_frequency = ifelse(drinkingstatus==2, 0,
                             ifelse(alc_frequency<110, (alc_frequency-100)*52/12,
                                    ifelse(alc_frequency>200 & alc_frequency<=231, alc_frequency-200,
                                           ifelse(alc_frequency==777, NA,
                                                  ifelse(alc_frequency==888, 0,
                                                         ifelse(alc_frequency==999,NA, alc_frequency)))))))
    }else if(data$YEAR[1]>=2001 & data$YEAR[1]<=2004){
      data <- data %>% mutate(
        alc_frequency = ifelse(alc_frequency<110, (alc_frequency-100)*52/12,
                                      ifelse(alc_frequency>200 & alc_frequency<=231, alc_frequency-200,
                                             ifelse(alc_frequency==777, NA,
                                                    ifelse(alc_frequency==888, 0,
                                                           ifelse(alc_frequency==999,NA,alc_frequency))))))
    }else if(data$YEAR[1]>=2005 & data$YEAR[1]<=2010){
      data <- data %>% mutate(
        alc_frequency = ifelse(drinkingstatus==2, 0,
                               ifelse(alc_frequency<110, (alc_frequency-100)*52/12,
                                      ifelse(alc_frequency>200 & alc_frequency<=231, alc_frequency-200,
                                             ifelse(alc_frequency==777, NA,
                                                    ifelse(alc_frequency==888, 0,
                                                           ifelse(alc_frequency==999,NA, alc_frequency)))))))
    }else if(data$YEAR[1]>=2011){
      data <- data %>% mutate(
      alc_frequency = ifelse(alc_frequency<110, (alc_frequency-100)*52/12,
                                    ifelse(alc_frequency>200 & alc_frequency<=231, alc_frequency-200,
                                           ifelse(alc_frequency==777, NA,
                                                  ifelse(alc_frequency==888, 0,
                                                         ifelse(alc_frequency==999,NA,alc_frequency))))))
                                                    
    }
  data$alc_frequency <- round(data$alc_frequency,digits=0)
  data$alc_frequency <- ifelse(data$alc_frequency==31, 30, 
                               ifelse(data$alc_frequency>31, NA, data$alc_frequency))
  # recode the missing drinking prevalence values now we have frequency values 
  data$drinkingstatus <- ifelse(data$alc_frequency==0, 0,
                                ifelse(data$alc_frequency>=1, 1, data$drinkingstatus))
  
  
  return(data)
}

recode_alc_quantity <- function(data){
  data <- data %>% 
    mutate(quantity_per_occasion = ifelse(YEAR<=2000, NALCOCC,
                             ifelse(YEAR>=2001 & YEAR<=2004, AVEDRNK,
                                    ifelse(YEAR>=2005 & YEAR<=2018, AVEDRNK2,
                                           ifelse(YEAR>=2019, AVEDRNK3, NA)))),
           quantity_per_occasion = ifelse(quantity_per_occasion==88, 0,
                                 ifelse(quantity_per_occasion==77, NA,
                                        ifelse(quantity_per_occasion==99, NA, quantity_per_occasion))))
  data$quantity_per_occasion <- ifelse(data$alc_frequency==0, 0,
                          data$quantity_per_occasion)
  data$gramsperday <- ((data$quantity_per_occasion*data$alc_frequency)/30)*14
  return(data)
}

recode_hed <- function(data){
  data <- data %>% 
    mutate(hed = ifelse(YEAR<=2000, DRINKGE5,
                        ifelse(YEAR>=2001 & YEAR<=2005, DRNK2GE5,
                               ifelse(YEAR>=2006, DRNK3GE5, NA))),
           hed = ifelse(hed==99, NA, 
                        ifelse(hed==77, NA,
                               ifelse(hed==88, 0, hed))))
  data$hed <- ifelse(data$gramsperday==0, 0,
                     ifelse(data$hed>30, 30, data$hed))
  return(data)
}

# sample weights 
recode_sample_weights <- function(data){
  data <- data %>% 
    mutate(final_sample_weight = ifelse(YEAR<=2010, X.FINALWT,
                                        X.LLCPWT))
  return(data)
}


# select variables required 

subset_data <- function(data){
  data <- data %>% 
    dplyr::select(YEAR, State, final_sample_weight, race_eth, sex_recode, age_var,
                  education_summary, employment,
                  household_income, height_cm, weight_kg, BMI, drinkingstatus, 
                  alc_frequency, quantity_per_occasion, gramsperday, hed)
  return(data)
}
