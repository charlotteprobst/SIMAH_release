generate_population <- function(TPs, sizepop){
  races <- unique(TPs$racefinal)
  sex <- unique(TPs$sex)
  incomequintile = unique(TPs$incomequintile)
  incomecat <- unique(TPs$incomecat)
  
  samplecats <- expand.grid(race=races,sex=sex,income=incomequintile,incomecat=incomecat) %>% 
    mutate(samplecats = paste(race,sex,incomequintile,incomecat, sep="_"))
  samplecats <- samplecats$samplecats
  sampleprobs <- rep(1/length(samplecats), times=length(samplecats))
  
  
  population1 <- data.frame(sample(samplecats, sizepop, replace=T, prob=sampleprobs))
  names(population1)[1] <- "cats"
  population1 <- population1 %>% separate(cats, into=c("racefinal","sex","incomequintile","incomecat")) %>% mutate(id = 1:nrow(population1)) %>% 
    select(id, sex, racefinal, incomequintile, incomecat) %>% 
    mutate(age=16,
           state="LHS",
           cat = paste(age,sex,racefinal,incomequintile,incomecat, "STATEFROM", state, sep="_"))
  return(population1)
}

transition_ed <- function(data, transitions){
  selected <- unique(data$cat)
  rates <- transitions %>% filter(cat==selected)
  data$education <- ifelse(data$prob<=rates$cumsum[1], "LHS",
                       ifelse(data$prob<=rates$cumsum[2] & data$prob>rates$cumsum[1], "HS",
                              ifelse(data$prob<=rates$cumsum[3] & data$prob>rates$cumsum[2],"SomeC1",
                                     ifelse(data$prob<=rates$cumsum[4] & data$prob>rates$cumsum[3],"SomeC2",
                                            ifelse(data$prob<=rates$cumsum[5] & data$prob>rates$cumsum[4],"SomeC3",
                                                   ifelse(data$prob<=rates$cumsum[6] & data$prob>rates$cumsum[5],"College",
                                                          NA))))))
  return(data)
}

simulate_population <- function(population, TPs, timeperiod){
  if(timeperiod=="1999-2009"){
    minyear <- 1999
    maxyear <- 2009
    transitions <- TPs %>% filter(time=="1999 - 2009") %>% 
      mutate(cat = paste(age,sex,racefinal, incomequintile, incomecat, "STATEFROM", Transition1, sep="_")) %>% 
      data.frame(.) %>% dplyr::select(cat, Transition2,prob) %>% 
      group_by(cat) %>% 
      mutate(cumsum=cumsum(prob),
             cumsum = ifelse(cumsum>=0.99, 1,cumsum))

  }else if(timeperiod=="2009-2019"){
    minyear <- 2009
    maxyear <- 2019
    transitions <- TPs %>% filter(time=="2009 - 2019") %>% 
      mutate(cat = paste(age,sex,racefinal, incomequintile, incomecat, "STATEFROM", Transition1, sep="_")) %>% 
      data.frame(.) %>% dplyr::select(cat, Transition2, prob) %>% 
      group_by(cat) %>% 
      mutate(cumsum=cumsum(prob),
             cumsum = ifelse(cumsum>=0.99, 1,cumsum))
    }
  output <- list()
  for(i in minyear:maxyear){
    if(minyear==i){ #save a copy of the population in the initial year 
      output[[paste(i)]] <- population %>% mutate(year=i, education="LHS")
    }else if(i>minyear){ #simulate the cohort forwards in time transitioning education
    population$prob <- runif(nrow(population))
    population <- population %>% group_by(cat) %>% do(transition_ed(., transitions)) %>% 
      mutate(age = age+1,
             year = i,
             cat = paste(age,sex,racefinal, incomequintile, incomecat, "STATEFROM", education, sep="_")) %>% 
      dplyr::select(-prob) %>% 
      data.frame(.)
    output[[paste(i)]] <- population    
    }
  }
  
  output <- do.call(rbind, output)
  output$period <- timeperiod
  return(output)
}

  