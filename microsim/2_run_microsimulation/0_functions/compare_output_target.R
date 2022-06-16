compare_output_target <- function(output){
  source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/process_target_comparison.R")
  # for(i in 2000:max(names(output))){
    output <- output %>% 
      mutate(YEAR= year,
             AGECAT = cut(microsim.init.age, breaks=c(0,24,34,44,54,64,74,100),
                          labels=c("18-24","25-34","35-44",
                                   "45-54","55-64","65-74","75-79")),
             AGECAT = cut(microsim.init.age, breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,100),
                 labels=c("18","19-24","25-29","30-34","35-39",
                          "40-44","45-49","50-54","55-59",
                          "60-64","65-69","70-74","75-79"))) %>% 
      group_by(YEAR, microsim.init.sex, microsim.init.race, AGECAT) %>% summarise(n=sum(n)) %>% 
      rename(SEX = microsim.init.sex, RACE=microsim.init.race) %>%
      mutate(data="microsim",
             SEX = recode(SEX, "m"="M","f"="F"),
             RACE = recode(RACE, "BLA"="Black", "SPA"="Hispanic","WHI"="White","OTH"="Other"))
    
  output <- output %>% 
    # rename(microsim=n) %>% 
    dplyr::select(YEAR, SEX, RACE, AGECAT, data,n) %>% 
    mutate(#target = NA,
           # lower_ci = NA,
           # upper_ci = NA,
           YEAR = as.numeric(as.character(YEAR)))

  target <- target %>% rename(n=target) %>% mutate(data="target")
    
  compare <- rbind(target,output) %>% 
    group_by(YEAR, SEX, RACE, AGECAT) %>% 
    # dplyr::select(-c(lower_ci, upper_ci)) %>% 
    pivot_wider(names_from=data, values_from=n) %>% 
    # summarise(target=sum(target, na.rm=T),
    #           lower_ci=unique(lower_ci),
    #           upper_ci=unique(upper_ci), 
    #           microsim=sum(microsim, na.rm=T)) %>% 
    mutate(diff = target-microsim,
           diffscaled = diff*(1/proportion)) %>%
    rename(Year = YEAR, microsim.init.sex = SEX, microsim.init.race = RACE, agecat=AGECAT) %>% 
    mutate(microsim.init.sex = recode(microsim.init.sex, "M"="m","F"="f"),
           microsim.init.race = recode(microsim.init.race, "Black"="BLA","White"="WHI","Other"="OTH","Hispanic"="SPA"))
  # compare <- compare %>% pivot_wider(names_from=data, values_from=n)
  
  # compare <- compare %>% group_by(Year, microsim.init.sex, microsim.init.race, agecat) %>% 
  #   summarise(lower_ci = mean(lower_ci,na.rm=T), upper_ci = mean(upper_ci,na.rm=T), 
  #             target=mean(target,na.rm=T), microsim=mean(microsim,na.rm=T))
  
  return(compare)
}
