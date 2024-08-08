# updating alcohol and BMI 

basepop <- basepop %>% 
  mutate(agecat = cut(microsim.init.age,
                      breaks=c(0,19,24,34,44,54,64,74,100),
                      labels=c("15-19","20-24","25-34","35-44","45-54","55-64",
                               "65-74","75+")))
newGPD <- updating_alcohol(basepop, brfssorig, y)
basepop <- left_join(basepop, newGPD, by=c("microsim.init.id"))

basepop$newformerdrinker <- ifelse(basepop$newGPD==0 & basepop$microsim.init.alc.gpd>0.46,1,
                                   ifelse(basepop$newGPD>0.46, 0, basepop$formerdrinker))

basepop$newyearsincedrink <- ifelse(basepop$newformerdrinker==1 & basepop$formerdrinker==0, 0,
                                    ifelse(basepop$newformerdrinker==1 & basepop$formerdrinker==1,
                                           basepop$yearsincedrink+1,
                                           ifelse(basepop$newGPD>0.46, 0, 0)))

basepop <-  basepop %>% 
  mutate(formerdrinker = newformerdrinker,
         yearsincedrink = newyearsincedrink,
         microsim.init.alc.gpd = newGPD,
         microsim.init.drinkingstatus = ifelse(microsim.init.alc.gpd<=0.46, 0, 1)) %>% 
  dplyr::select(-c(newGPD, newformerdrinker, newyearsincedrink))

newBMI <- updating_BMI(basepop, brfssorig, y)
basepop <- left_join(basepop, newBMI, by=c("microsim.init.id"))
basepop$microsim.init.BMI <- basepop$newBMI
basepop$newBMI <- NULL
