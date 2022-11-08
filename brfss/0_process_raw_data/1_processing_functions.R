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
  if(data$YEAR[1]<=1992){
    data <- data %>% 
      mutate(
        education_detailed = recode(EDUCA, 
                                    "1"="eighth grade or less",
                                    "2"="some high school",
                                    "3"="high school graduate",
                                    "4"="some technical school",
                                    "5"="technical school graduate",
                                    "6"="some college",
                                    "7"="college graduate",
                                    "8"="postgraduate degree",
                                    "9"="NA",.default="NA"),
        education_detailed = ifelse(education_detailed=="NA",NA,
                                    education_detailed),
        education_summary = ifelse(EDUCA<=4, "LEHS",
                                   ifelse(EDUCA>=5 & EDUCA<=6,"SomeC",
                                          ifelse(EDUCA>=7 & EDUCA<=8, "College",NA))))
  }else{
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
  }
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

recode_marital <- function(data){
  data <- data %>% mutate(
    marital_status = ifelse(MARITAL==1, 1,
                            ifelse(MARITAL==9, NA, 0)))
    return(data)
}

recode_income <- function(data){
 # if(data$YEAR[1]>=1994 & data$YEAR[1]<=1995){
 #  data$INCOME2 <- data$INCOME
 # }
  if(data$YEAR[1]==1984){
    data <- data %>% mutate(
      household_income = ifelse(INCOME==1, "0-10000",
                                ifelse(INCOME==2, "10001-15000",
                                       ifelse(INCOME==3, "15001-20000",
                                              ifelse(INCOME==4, "20001-25000",
                                                     ifelse(INCOME==5, "25001-35000",
                                                            ifelse(INCOME==6, "35001-100000",
                                                                   NA)))))))
  }else if(data$YEAR[1]>=1985 & data$YEAR[1]<=1990){
    data <- data %>% mutate(
      household_income = ifelse(INCOME==1, "0-10000",
                                ifelse(INCOME==2, "10001-15000",
                                       ifelse(INCOME==3, "15001-20000",
                                              ifelse(INCOME==4, "20001-25000",
                                                     ifelse(INCOME==5, "25001-35000",
                                                            ifelse(INCOME==6, "35001-50000",
                                                                   ifelse(INCOME==8, "50000-100000",
                                                                          NA))))))))
  }else if(data$YEAR[1]>=1991 & data$YEAR[1]<=1994){
    data <- data %>% mutate(
      household_income = ifelse(INCOME==1, "0-10000",
                                ifelse(INCOME==2, "10001-15000",
                                       ifelse(INCOME==3, "15001-20000",
                                              ifelse(INCOME==4, "20001-25000",
                                                     ifelse(INCOME==5, "25001-35000",
                                                            ifelse(INCOME==6, "35001-50000",
                                                                   ifelse(INCOME==7, "50000-100000",
                                                                          NA))))))))
    }else if(data$YEAR[1]>=1994 & data$YEAR[1]<=2020){
  data <- data %>% mutate(
    INCOME2 = ifelse(YEAR==1995, INCOME95, INCOME2),
    household_income = ifelse(INCOME2==1, "0-9999",
                        ifelse(INCOME2==2, "10000-14999",
                               ifelse(INCOME2==3, "15000-19999",
                                      ifelse(INCOME2==4, "20000-24999",
                                             ifelse(INCOME2==5, "25000-34999",
                                                    ifelse(INCOME2==6, "35000-49999",
                                                           ifelse(INCOME2==7, "50000-74999",
                                                                  ifelse(INCOME2==8, "75000+", NA)))))))))
    }else if(data$YEAR[1]>=2021){
      data <- data %>% 
        mutate(
          household_income = ifelse(INCOME3==1, "0-9999",
                                    ifelse(INCOME3==2, "10000-14999",
                                           ifelse(INCOME3==3, "15000-19999",
                                                  ifelse(INCOME3==4, "20000-24999",
                                                         ifelse(INCOME3==5, "25000-34999",
                                                                ifelse(INCOME3==6, "35000-49999",
                                                                       ifelse(INCOME3==7, "50000-74999",
                                                                              ifelse(INCOME3==8, "75000-99999", 
                                                                                     ifelse(INCOME3==9, "100000-149000",
                                                                                            ifelse(INCOME3==10, "150000-199999", 
                                                                                                   ifelse(INCOME3==11, "200000+", NA))))))))))))
    }
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
                                               WEIGHT*0.4536)))))
  }else if(data$YEAR[1]>=2004){
    data <- data %>% 
      mutate(
        weight_kg = ifelse(WEIGHT2==7777, NA,
                           ifelse(WEIGHT2==9999, NA,
                                  ifelse(WEIGHT2>=9000 & WEIGHT2<=9998, WEIGHT2-9000,
                                         WEIGHT2*0.4536))))
    # check if weight in kg starts in 99 or 90 
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
  if(data$YEAR[1]<2004){
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
  }else if(data$YEAR[1]>=2004){
    data <- data %>% mutate(
      heightvar = ifelse(heightvar==7777, NA, 
                         ifelse(heightvar==9999, NA, 
                                ifelse(heightvar==777, NA, 
                                       ifelse(heightvar==999, NA,
                                              heightvar)))),
      feet = ifelse(heightvar<=9000, as.numeric(substr(heightvar, 1,1)),NA),
      inches = ifelse(heightvar<=9000, as.numeric(substr(heightvar, 3,4)),NA),
      height_cm = ifelse(heightvar>=9000 & heightvar<=9998, (heightvar-9000)/100,
                         ifelse(heightvar<=9000, feet*0.3048 + inches*0.0254,heightvar)),
      height_cm = ifelse(heightvar==777, NA, 
                         ifelse(heightvar==9777, NA, height_cm)))
  }
  data$height_cm <- ifelse(data$height_cm>300, NA, data$height_cm)
  return(data)
}

recode_BMI <- function(data){
  data <- data %>% mutate(
    BMI = weight_kg / ((height_cm)^2)
  )
}

for(i in names(dataFiles)){
  print(i)
  print(summary(dataFiles[[i]]$X.BMI))
  print(summary(dataFiles[[i]]$X.BMI2))
  print(summary(dataFiles[[i]]$X.BMI3))
  print(summary(dataFiles[[i]]$X.BMI4))
  print(summary(dataFiles[[i]]$X.BMI5))
  
}

# recoding derived BMI variable 
recode_derived_BMI <- function(data){
  data <- data %>% mutate(
    BMI_derived = ifelse(YEAR<1987, NA,
                         ifelse(YEAR>=1987 & YEAR<2000, X.BMI,
                         ifelse(YEAR>=2000 & YEAR<=2002, X.BMI2, 
                                ifelse(YEAR==2003, X.BMI3, 
                                       ifelse(YEAR>=2004 & YEAR<=2010, X.BMI4,
                                              ifelse(YEAR>=2011, X.BMI5, NA)))))),
    BMI_derived = ifelse(YEAR<1987, NA, 
                         ifelse(YEAR>=1987 & YEAR<=2000 & BMI_derived>=999, NA,
                                ifelse(YEAR>=1987 & YEAR<=2000, BMI_derived/10,
                                       ifelse(YEAR==2001 & BMI_derived==999999, NA,
                                              ifelse(YEAR==2001, BMI_derived/10000,
                                                     ifelse(YEAR>=2002 & YEAR<=2010 & BMI_derived==9999, NA,
                                                            ifelse(YEAR>=2002 & YEAR<=2010, BMI_derived/100,
                                                                   ifelse(YEAR>=2011 & BMI_derived>9000, NA,
                                                                          ifelse(YEAR>=2011, BMI_derived/100, NA))))))))),
    BMI_final = ifelse(is.na(BMI_derived), BMI, BMI_derived))
  
  return(data)
  }

impute_missing_BMI <- function(data){
  data <- data %>% 
    mutate(BMI = ifelse(BMI<15, NA,
                        ifelse(BMI>40, NA, BMI)))
  toimpute <- data %>% dplyr::select(State, age_var, sex_var, race_eth_detailed, education_detailed,
                                     employment_detailed, household_income, BMI)
  imputed <- mice(toimpute, method="pmm")
  completedata <- complete(imputed)
  data$BMI <- completedata$BMI
  return(data)
}


# alcohol functions 

recode_alc_prevalence <- function(data){
  data <- data %>%
    mutate(drinkingstatus = ifelse(YEAR<=2000, DRINKANY,
                                   ifelse(YEAR>=2005 & YEAR<=2010, DRNKANY4,NA)),
           drinkingstatus = ifelse(drinkingstatus==2, 0,
                                   ifelse(drinkingstatus==1, 1,
                                          ifelse(alc_frequency==0, 0, NA))))
  if(data$YEAR[1]>2000 & data$YEAR[1]<=2005){
    data$drinkingstatus <- ifelse(data$alc_frequency==0, 0,
                                  ifelse(data$alc_frequency>0, 1, NA))
  }else if(data$YEAR[1]>2010){
    data$drinkingstatus <- ifelse(data$alc_frequency==0, 0,
                                         ifelse(data$alc_frequency>0, 1, NA))
  }
  return(data)
}

recode_alc_frequency <- function(data){
  data <- data %>% mutate(
  alc_frequency = ifelse(YEAR<=2000 & YEAR>=1989, ALCOHOL,
                         ifelse(YEAR==2001, ALCDAYS,
                                ifelse(YEAR>=2002 & YEAR<=2004, ALCDAY3,
                                       ifelse(YEAR>=2005 & YEAR<=2010, ALCDAY4, 
                                              ifelse(YEAR>=2011, ALCDAY5, NA))))))
  if(data$YEAR[1]<=2000 & data$YEAR[1]>=1989){
    data <- data %>% mutate(
      alc_frequency = ifelse(DRINKANY==2, 0,
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
        alc_frequency = ifelse(DRNKANY4==2, 0,
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
    }else if(data$YEAR[1]<=1988){
      data <- data %>% mutate(
        beer_freq = ifelse(DRKBEER<110 & DRKBEER>=100, (DRKBEER-100)*52/12,
                                      ifelse(DRKBEER>200 & DRKBEER<=231, DRKBEER-200,
                                             ifelse(DRKBEER==777, NA,
                                                    ifelse(DRKBEER==888, 0,0)))),
        wine_freq = ifelse(DRKWINE<110 & DRKWINE>=100, (DRKWINE-100)*52/12,
                                  ifelse(DRKWINE>200 & DRKWINE<=231, DRKWINE-200,
                                         ifelse(DRKWINE==777, NA,
                                                ifelse(DRKWINE==888, 0,0)))),
        spirit_freq = ifelse(DRKLIQR<110 & DRKLIQR>=100, (DRKLIQR-100)*52/12,
                                    ifelse(DRKLIQR>200 & DRKLIQR<=231, DRKLIQR-200,
                                           ifelse(DRKLIQR==777, NA,
                                                  ifelse(DRKLIQR==888, 0, 0)))),
        beer_freq = ifelse(is.na(beer_freq),0,beer_freq),
        wine_freq = ifelse(is.na(wine_freq),0,wine_freq),
        spirit_freq = ifelse(is.na(spirit_freq),0,spirit_freq),
        # alc_frequency = beer_freq+wine_freq+spirit_freq,
        alc_frequency = ifelse(DRINKANY==2, 0, 
                               ifelse(DRINKANY==9, NA, 
                                      ifelse(DRINKANY==1, beer_freq + wine_freq + spirit_freq, 
                                             alc_frequency))))
    }
                                                  
  data$alc_frequency <- round(data$alc_frequency,digits=0)
  data$alc_frequency <- ifelse(data$alc_frequency==31, 30, 
                               ifelse(data$alc_frequency>31, 30, data$alc_frequency))
  # recode the missing drinking prevalence values now we have frequency values 
  # data$drinkingstatus <- ifelse(data$alc_frequency==0, 0,
  #                               ifelse(data$alc_frequency>=1, 1, data$drinkingstatus))
  
  return(data)
}

recode_alc_quantity <- function(data){
  if(data$YEAR[1]<=1988){
    data <- data %>% mutate(
      nbeer = ifelse(NBEEROCC>=77, NA, 
                     ifelse(beer_freq==0, 0, NBEEROCC)),
      nwine = ifelse(NWINEOCC>=77,NA,
                     ifelse(wine_freq==0, 0, NWINEOCC)),
      nspirit = ifelse(NLIQROCC>=77, NA, 
                       ifelse(spirit_freq==0, 0, NLIQROCC)),
      nbeer = ifelse(is.na(nbeer), 0, nbeer),
      nwine = ifelse(is.na(nwine),0,nwine),
      nspirit=ifelse(is.na(nspirit),0,nspirit),
      beerpermonth = ifelse(beer_freq==0, 0, nbeer*beer_freq),
      winepermonth = ifelse(wine_freq==0, 0,nwine*wine_freq),
      spiritpermonth = ifelse(spirit_freq==0, 0,nspirit*spirit_freq),
      drinkspermonth = beerpermonth + winepermonth + spiritpermonth,
      quantity_per_occasion = ifelse(DRINKANY==2, 0,
                                     ifelse(DRINKANY==9, NA,
                                     drinkspermonth/alc_frequency)))

  }
  else if(data$YEAR[1]>=1989){
  data <- data %>% 
    mutate(quantity_per_occasion = ifelse(YEAR<=2000, NALCOCC,
                             ifelse(YEAR>=2001 & YEAR<=2004, AVEDRNK,
                                    ifelse(YEAR>=2005 & YEAR<=2018, AVEDRNK2,
                                           ifelse(YEAR>=2019, AVEDRNK3, NA)))),
           quantity_per_occasion = ifelse(quantity_per_occasion==88, 0,
                                 ifelse(quantity_per_occasion==77, NA,
                                        ifelse(quantity_per_occasion==99, NA, 
                                               ifelse(drinkingstatus==0, 0, quantity_per_occasion)))))
  }
  data$quantity_per_occasion <- ifelse(data$alc_frequency==0, 0,
                          data$quantity_per_occasion)
  # data$alc_frequency <- ifelse(data$quantity_per_occasion==0, 0, data$alc_frequency)
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

recode_menthealth <- function(data){
  if(data$YEAR[1]>=1993){
  data <- data %>% 
    mutate(mentalhealth = ifelse(MENTHLTH==88, 0,
                                 ifelse(MENTHLTH>=77, NA,
                                        MENTHLTH)),
           physicalhealth = ifelse(PHYSHLTH==88, 0,
                                   ifelse(PHYSHLTH>=77, NA,
                                          PHYSHLTH)))
  }else if(data$YEAR[1]<1993){
    data$mentalhealth <- NA
    data$physicalhealth <- NA
  }
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
    dplyr::select(YEAR, State, final_sample_weight, race_eth, race_eth_detailed, sex_recode, age_var,
                  education_summary, employment, marital_status,
                  household_income,
                  height_cm, weight_kg, BMI_final, drinkingstatus, 
                  mentalhealth, physicalhealth,
                  alc_frequency, quantity_per_occasion, gramsperday, hed)
  return(data)
}

