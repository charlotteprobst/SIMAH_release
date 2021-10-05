compare_output_target <- function(output){
  source("1_preprocessing_scripts/process_target_comparison.R")
  for(i in 2000:max(names(output))){
    output[[paste(i)]] <- output[[paste(i)]] %>% 
      mutate(YEAR= i,
             AGECAT = cut(microsim.init.age, breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,79),
                                          labels=c("18","19-24","25-29","30-34","35-39","40-44",
                                                   "45-49","50-54","55-59","60-64","65-69","70-74","75-79"))) %>% 
      group_by(YEAR, microsim.init.sex, microsim.init.race, AGECAT) %>% tally() %>% 
      rename(SEX = microsim.init.sex, RACE=microsim.init.race) %>%
      mutate(data="microsim",
             SEX = recode(SEX, "m"="M","f"="F"),
             RACE = recode(RACE, "BLA"="Black", "SPA"="Hispanic","WHI"="White","OTH"="Other"))
  }
  
  output <- do.call(rbind, output) %>% 
    rename(microsim=n) %>% select(YEAR, SEX, RACE, AGECAT, microsim)
  output$target <- NA
  target$microsim <- NA
  compare <- rbind(target,output) %>% group_by(YEAR, SEX, RACE, AGECAT) %>% 
    summarise(target=sum(target, na.rm=T), microsim=sum(microsim, na.rm=T)) %>% 
    mutate(diff = target-microsim,
           diffscaled = diff*(1/proportion)) %>% 
    rename(Year = YEAR, microsim.init.sex = SEX, microsim.init.race = RACE, agecat=AGECAT) %>% 
    mutate(microsim.init.sex = recode(microsim.init.sex, "M"="m","F"="f"),
           microsim.init.race = recode(microsim.init.race, "Black"="BLA","White"="WHI","Other"="OTH","Hispanic"="SPA"))
  return(compare)
}
