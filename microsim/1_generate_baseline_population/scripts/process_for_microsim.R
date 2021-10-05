# select variables needed for microsim only
microsim <- microsim %>% select(microsim.init.age, microsim.init.race, microsim.init.sex,
                                microsim.init.education,
                                microsim.init.drinkingstatus, microsim.init.alc.gpd,
                                microsim.init.income, drinkingstatus_2) %>% 
  mutate(microsim.init.spawn.year=2000,
         agecat = cut(microsim.init.age,
                      breaks=c(0,24,29,34,39,44,49,54,59,64,69,100),
                      labels=c("18-24","25-29","30-34","35-39","40-44","45-49",
                               "50-54","55-59","60-64","65-69","70-80")),
         microsim.init.age = as.integer(microsim.init.age),
         microsim.init.sex = ifelse(microsim.init.sex=="F","f",
                                    ifelse(microsim.init.sex=="M","m",microsim.init.sex)),
         microsim.init.sex = as.character(microsim.init.sex),
         microsim.init.race = as.character(microsim.init.race),
         microsim.init.spawn.year = as.integer(microsim.init.spawn.year),
        formerdrinker = ifelse(drinkingstatus_2==1, 1,0)) %>% 
  dplyr::select(-c(drinkingstatus_2))

microsim <- data.frame(microsim)  
