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
    mutate(Hispanic = ifelse(HISPANIC==1, 1,
                             ifelse(HISPANIC==2, 0, NA)),
           race_comb = ifelse(YEAR<2001, ORACE,
                              ifelse(YEAR>=2001 & YEAR<=2012, RACE2, 
                                     ifelse(YEAR>=2013, RACE, NA))))
  return(data)
}
