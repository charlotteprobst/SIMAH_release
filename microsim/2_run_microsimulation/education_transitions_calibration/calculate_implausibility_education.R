# calculate implausibility education 

calculate_implausibility_education <- function(data, targets){
  data <- data %>% mutate(AGECAT = cut(microsim.init.age,
                      breaks=c(0,24,34,44,54,64,79),
                      labels=c("18-24","25-34","35-44","45-54",
                               "55-64","65-79")),
         SEX = ifelse(microsim.init.sex=="m", "Men","Women"),
         RACE = recode(microsim.init.race, "BLA"="Black","WHI"="White","SPA"="Hispanic",
                       "OTH"="Others")) %>% 
    rename(EDUC=microsim.init.education, YEAR=year) %>% 
    group_by(YEAR, samplenum, seed, SEX, AGECAT,RACE,
             EDUC) %>% 
    summarise(n=sum(n)) %>% 
    ungroup() %>% 
    group_by(YEAR, samplenum, SEX, AGECAT, EDUC,RACE) %>% 
    summarise(n=mean(n)) %>% 
    ungroup() %>% 
    group_by(YEAR, samplenum, SEX, AGECAT,RACE) %>% 
    mutate(prop=n/sum(n), YEAR=as.integer(YEAR)) %>% 
    dplyr::select(-n) %>% drop_na()
  data <- left_join(data,targets) %>% 
    mutate(EDUC = factor(EDUC, levels=c("LEHS","SomeC","College")))
  
  implausibility <- data %>% 
    # filter(AGECAT=="18-24") %>%
    filter(YEAR<=2019) %>% 
    group_by(YEAR, samplenum, SEX,AGECAT, RACE, EDUC) %>% 
    summarise(implausibility = abs(prop-target)/sqrt(SE)) %>% 
    group_by(samplenum) %>% 
    summarise(implausibility=max(implausibility, na.rm=T)) %>% 
    ungroup() %>% 
    mutate(percentile=ntile(implausibility,100))
  return(implausibility)
}