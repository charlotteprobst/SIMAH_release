subset_SIMAH_states <- function(data){
# subset for SIMAH states and create new "STATE" for USA 
USA <- data %>% mutate(State="USA", sample_weight=final_sample_weight/1000) %>% 
  expandRows(.,"sample_weight")
data <- rbind(data, USA)
SIMAH_states <- c("California","Colorado","Florida","Indiana","Kentucky",
                  "Louisiana","Massachusetts","Michigan","Minnesota","Missouri",
                  "New York","Oregon","Pennsylvania","Tennessee","Texas","USA")
data <- data %>% ungroup() %>% filter(State %in% SIMAH_states)
return(data)
}

remove_missing <- function(data){
  data <- data %>% dplyr::select(YEAR, State, race_eth,
                                 sex_recode, age_var,
                                 employment, education_summary,
                                 household_income,
                                 BMI, drinkingstatus,
                                 alc_frequency, quantity_per_occasion,
                                 gramsperday,
                                 hed) %>% drop_na()
  return(data)
}

summariseprevalence <- function(data){
  data <- data %>% 
    mutate(agecat = cut(age_var,
                        breaks=c(0,34,64,100),
                        labels=c("18-34","35-64","65+"))) %>% 
    group_by(YEAR, State, sex_recode,agecat,drinkingstatus) %>% 
    tally(name="total") %>% 
    # summarise(total = sum(final_sample_weight)) %>% 
    ungroup() %>% 
    group_by(YEAR, State, sex_recode,agecat) %>% 
    mutate(percentage = total / sum(total))
  return(data)
  }

process_APC <- function(data){
  APC <- read.delim("SIMAH_workplace/brfss/processed_data/pcyr1970-2019.txt", skip=131, header=F, sep="")
  names(APC)[1] <- "Year"
  names(APC)[2] <- "State"
  names(APC)[3] <- "BeverageType"
  names(APC)[7] <- "Gallons per capita"
  APC <- APC %>% filter(BeverageType==4) %>% dplyr::select(Year, State, 'Gallons per capita') %>% 
    mutate(`Gallons per capita` = `Gallons per capita`/10000)
  USA <- APC %>% group_by(Year) %>% summarise(`Gallons per capita`= mean(`Gallons per capita`)) %>% 
    mutate(State="USA")
  APC <- rbind(APC,USA)
  APC <- APC %>% 
    mutate(State = recode(State,
                          "6"="California", "8"="Colorado", "12"="Florida",
                          "18"="Indiana", "21"="Kentucky", "22"="Louisiana",
                          "25"="Massachusetts", "26"="Michigan", "27"="Minnesota",
                          "29"="Missouri","36"="New York","41"="Oregon",
                          "42"="Pennsylvania","47"="Tennessee","48"="Texas",
                          "USA"="USA", .default="NA"),
           State = ifelse(State=="NA",NA,State)) %>% 
    drop_na() %>% 
    # filter(Year>=min(data$YEAR)) %>% 
    rename(Gallons = 'Gallons per capita') %>% 
    mutate(litrespercapita = Gallons*3.78541)
    
  # allocate 2020 data 2019 APC UNTIL 2020 IS RELEASED
  data2020 <- expand.grid(Year=2020, State=unique(data$State)) %>% 
    mutate(State=as.character(State))
  data2020$Gallons <- NA
  data2020$litrespercapita=NA
  APC <- rbind(APC, data2020)
  APC <- APC %>% group_by(State) %>% 
    fill(Gallons,litrespercapita) %>% mutate(Gallons=as.numeric(Gallons),
                                             litrespercapita=as.numeric(litrespercapita)) %>% 
    mutate(gramsperday = (litrespercapita*785.06)/365,
           gramsperday_adj1 = gramsperday - (gramsperday*0.0098),
           gramsperday_adj2 = gramsperday - (gramsperday*0.0098*1.5)) %>% 
    dplyr::select(Year, State, gramsperday_adj1) %>% 
    rename(YEAR=Year)
  return(APC)
}

impute_yearly_drinking <- function(data){
  #####STEP 1 
  ####TURNING THE NON 30-DAY DRINKERS INTO YEARLY DRINKERS, FORMER DRINKERS, ABSTAINERS
  
  ####read in the NAS distributions
  NASdistributions <- read.csv("SIMAH_workplace/brfss/processed_data/NAS_distributions.csv") %>% 
    mutate(Age = gsub("Age ", "", Age),
           YEARLYDRINKERS = Yearly...non.30d..Drinkers / (Yearly...non.30d..Drinkers+Former..Drinker+Abstainer),
           FORMERDRINKERS = Former..Drinker / (Yearly...non.30d..Drinkers+Former..Drinker+Abstainer),
           ABSTAINERS = Abstainer / (Yearly...non.30d..Drinkers+Former..Drinker+Abstainer),
           group=paste(Sex, Age, Race, sep="_")) %>% 
    dplyr::select(group, YEARLYDRINKERS, FORMERDRINKERS, ABSTAINERS) %>% 
    pivot_longer(cols=YEARLYDRINKERS:ABSTAINERS) %>% 
    group_by(group) %>% 
    mutate(cumsum = cumsum(value))
  
  data <- data %>% mutate(agecat = cut(age_var, 
                                       breaks=c(0,34,64,100),
                                       labels=c("18-34", "35-64", "65+")),
                          sex_recode=recode(sex_recode,"Men"="Male","Women"="Female"),
                          race_eth=recode(race_eth,"Non-Hispanic Black"="Black",
                                                   "Non-Hispanic White"="White",
                                                   "Non-Hispanic Other"="Other",
                                                   "Hispanic"="Hispanic"),
                          group = paste(sex_recode, agecat, race_eth, sep="_"))
  
  sample <- function(test,NASdistributions){
    test$prob <- runif(nrow(test))
    rates <- NASdistributions %>% filter(group==unique(test$group))
    test$drinkingstatus_detailed = ifelse(test$prob<=rates$cumsum[1], "Yearly drinker",
                                          ifelse(test$prob>rates$cumsum[1] & test$prob<=rates$cumsum[2], "Former drinker",
                                                 ifelse(test$prob>rates$cumsum[2] & test$prob<=rates$cumsum[3], "Lifetime abstainer",NA)))
    return(test)
  }
  
  datatoimpute <- data %>% filter(drinkingstatus==0)
  datatoimpute <- datatoimpute %>% group_by(YEAR, State, group) %>% 
    do(sample(.,NASdistributions)) %>% dplyr::select(-prob)
  datatojoin <- data %>% filter(drinkingstatus!=0) %>% mutate(drinkingstatus_detailed="Monthly drinker")
    
  data <- rbind(datatojoin, datatoimpute) %>% 
    mutate(drinkingstatus_updated = ifelse(drinkingstatus_detailed=="Yearly drinker", 1,
                                           ifelse(drinkingstatus_detailed=="Monthly drinker", 1,
                                                  ifelse(drinkingstatus_detailed=="Former drinker",0,
                                                         ifelse(drinkingstatus_detailed=="Lifetime abstainer",0,NA)))))
  return(data)
}  

add_brfss_regions <- function(data){
  division1 <- c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont")
  division2 <- c("New Jersey","New York","Pennsylvania")
  division3 <- c("Illinois","Indiana","Michigan","Ohio","Wisconsin")
  division4 <- c("Iowa","Kansas","Minnesota","Missouri","Nebraska","North Dakota","South Dakota")
  division5 <- c("Delaware","Florida","Georgia","Maryland","North Carolina","South Carolina","Virginia","DC","West Virginia")
  division6 <- c("Alabama","Kentucky","Mississippi","Tennessee")
  division7 <- c("Arkansas","Louisiana","Oklahoma","Texas")
  division8 <- c("Arizona","Colorado","Idaho","Montana","Nevada","New Mexico","Utah","Wyoming")
  division9 <- c("Alaska","California","Hawaii","Oregon","Washington")
  data$region <- ifelse(!is.na(match(data$State,division1)), "division1",
                                                ifelse(!is.na(match(data$State,division2)),"division2",
                                                       ifelse(!is.na(match(data$State,division3)),"division3",
                                                              ifelse(!is.na(match(data$State,division4)),"division4",
                                                                     ifelse(!is.na(match(data$State,division5)),"division5",
                                                                            ifelse(!is.na(match(data$State,division6)),"division6",
                                                                                   ifelse(!is.na(match(data$State,division7)),"division7",
                                                                                          ifelse(!is.na(match(data$State,division8)),"division8",
                                                                                                 ifelse(!is.na(match(data$State,division9)),"division9",
                                                                                                        ifelse(data$State=="USA","USA",
                                                                                                        NA))))))))))
  
  return(data)
}
