generate_population <- function(TPs, sizepop){
  race <- unique(TPs$race)
  sex <- unique(TPs$sex)
  age <- unique(TPs$age)

  samplecats <- expand.grid(race=race,sex=sex,age=age) %>% 
    mutate(samplecats = paste(race, sex,age, sep="_"))
  samplecats <- samplecats$samplecats
  sampleprobs <- rep(1/length(samplecats), times=length(samplecats))
  
  population1 <- data.frame(sample(samplecats, sizepop, replace=T, prob=sampleprobs))
  names(population1)[1] <- "cats"
  population1 <- population1 %>% separate(cats, into=c("race","sex","age"), sep="_") %>% 
    mutate(id = 1:nrow(population1)) %>%
    select(id, race=race,sex=sex,age=age) %>% 
          mutate(age=18,
           state="LEHS",
           cat = paste(race=race,sex=sex,age=age, "STATEFROM", state, sep="_")) %>%
    dplyr::select(-state) %>%
    
  return(population1)
}

generate_population_race_sex_interaction <- function(TPs, sizepop, race, sex){
  race <- race
  sex <- sex
  age <- unique(TPs$age)
  
  samplecats <- expand.grid(race=race,sex=sex,age=age) %>% 
    mutate(samplecats = paste(race, sex,age, sep="_"))
  samplecats <- samplecats$samplecats
  sampleprobs <- rep(1/length(samplecats), times=length(samplecats))
  
  population1 <- data.frame(sample(samplecats, sizepop, replace=T, prob=sampleprobs))
  names(population1)[1] <- "cats"
  population1 <- population1 %>% separate(cats, into=c("race","sex","age"), sep="_") %>% 
    mutate(id = 1:nrow(population1)) %>%
    select(id, race=race,sex=sex,age=age) %>% 
    mutate(age=18,
           state="LEHS",
           cat = paste(race=race,sex=sex,age=age, "STATEFROM", state, sep="_")) %>%
    dplyr::select(-state) %>%
    
    return(population1)
}

simulate_population <- function(population, TPs, timeperiod){
  if(timeperiod=="1999-2005"){
    minyear <- 1999
    maxyear <- 2007 # Simulate until all individuals aged 18 in minyear are aged 26
    transitions <- TPs %>% filter(time_period=="1999-2005") %>%
      mutate(timeperiod=timeperiod,
             cat = paste(race, sex,age, "STATEFROM", StateFrom, sep="_")) %>% 
      data.frame(.) %>% dplyr::select(cat, StateTo,prob) %>% 
      group_by(cat) %>% 
      mutate(cumsum=cumsum(prob),
             cumsum = ifelse(cumsum>=0.99, 1,cumsum))
    
  }else if(timeperiod=="2006-2011"){
    minyear <- 2006
    maxyear <- 2014 # Simulate until all individuals aged 18 in minyear are aged 26
    transitions <- TPs %>% filter(time_period=="2006-2011") %>% 
      mutate(cat = paste(race, sex,age, "STATEFROM", StateFrom, sep="_")) %>% 
      data.frame(.) %>% dplyr::select(cat, StateTo,prob) %>% 
      group_by(cat) %>% 
      mutate(cumsum=cumsum(prob),
             cumsum = ifelse(cumsum>=0.99, 1,cumsum))
    
  }else if(timeperiod=="2012-2018"){
    minyear <- 2012
    maxyear <- 2020 # Simulate until all individuals aged 18 in minyear are aged 26
    transitions <- TPs %>% filter(time_period=="2012-2018") %>% 
      mutate(cat = paste(race, sex,age,  "STATEFROM", StateFrom, sep="_")) %>% 
      data.frame(.) %>% dplyr::select(cat, StateTo,prob) %>% 
      group_by(cat) %>% 
      mutate(cumsum=cumsum(prob),
             cumsum = ifelse(cumsum>=0.99, 1,cumsum))
 
     }else if(timeperiod=="2019-2021"){
    minyear <- 2019
    maxyear <- 2027 # Simulate until all individuals aged 18 in min year are aged 26
    transitions <- TPs %>% filter(time_period=="2019-2021") %>% 
      mutate(cat = paste(race, sex,age,  "STATEFROM", StateFrom, sep="_")) %>% 
      data.frame(.) %>% dplyr::select(cat, StateTo,prob) %>% 
      group_by(cat) %>% 
      mutate(cumsum=cumsum(prob),
             cumsum = ifelse(cumsum>=0.99, 1,cumsum))
     }
  output <- list()
  for(i in minyear:maxyear){
    if(minyear==i){ #save a copy of the population in the initial year 
      output[[paste(i)]] <- population %>% 
        mutate(year=i,
               education="LEHS",
               agecat=case_when(age==18 ~ "18",
                                age==19 ~ "19",
                                age==20 ~ "20",
                                age>=21 & age<=25 ~ "21-25",
                                age>=26 ~ "26+"))
    }else if(i>minyear){ #simulate the cohort forwards in time transitioning education
    print(i)
      population$rand <- runif(nrow(population))
      population <- population %>% group_by(cat) %>%
      do(transition_ed(., transitions)) %>%
      mutate(age = age+1,
             agecat = case_when(age==18 ~ "18",
                               age==19 ~ "19",
                               age==20 ~ "20",
                               age>=21 & age<=25 ~ "21-25",
                               age>=26 ~ "26+"),
             year = i,
             cat = paste(race,sex,agecat,"STATEFROM", education, sep="_")) %>%
      dplyr::select(-rand) %>%
      data.frame(.)
    output[[paste(i)]] <- population
    }
  }
  
  output <- do.call(rbind, output) # combine all dataframes within the list into one large dataframe
 output$period <- timeperiod
  return(output)
}

transition_ed <- function(data, transitions){
  selected <- unique(data$cat) 
  rates <- transitions %>% filter(cat%in%selected) ### issue is here for year 2003 onwards transitions
  # the age group of 21 is present in the population file, whereas in the transitions file it is based on age groups 21-25
  data$education <- ifelse(data$rand<=rates$cumsum[1], rates$StateTo[1],
                           ifelse(data$rand<=rates$cumsum[2] & data$rand>rates$cumsum[1], rates$StateTo[2],
                                  ifelse(data$rand<=rates$cumsum[3] & data$rand>rates$cumsum[2],rates$StateTo[3],
                                         ifelse(data$rand<=rates$cumsum[4] & data$rand>rates$cumsum[3],rates$StateTo[4],
                                                ifelse(data$rand<=rates$cumsum[5] & data$rand>rates$cumsum[4],rates$StateTo[5],
                                                              NA)))))
  return(data)
}
  