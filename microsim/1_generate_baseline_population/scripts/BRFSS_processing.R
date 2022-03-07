#####BRFSS processing for micro-synthesis 

brfss <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_reweighted_upshifted_1984_2020.RDS") %>% 
  filter(age_var<=79) %>% filter(YEAR==2000) %>% 
  mutate(RACE = recode(race_eth,"White"="WHI", 
                       "Black"="BLA", "Hispanic"="SPA", "Other"="OTH"),
         SEX = recode(sex_recode,"Male"="M","Female"="F"),
         EMPLOYED = employment,
         EDUCATION = education_summary,
         agecat = cut(age_var,
                      breaks=c(0,24,34,44,54,64,79),
                      labels=c("18.24","25.34","35.44","45.54","55.64","65.79")),
         frequency = ifelse(frequency==0 & gramsperday>0, 1, frequency),
         quantity_per_occasion = (gramsperday/14 * 30) / frequency,
         quantity_per_occasion = ifelse(gramsperday==0, 0, quantity_per_occasion),
         formerdrinker=ifelse(drinkingstatus_detailed=="Former drinker", 1,0))

selected <- brfss %>% filter(State==SelectedState) %>% 
  dplyr::select(region, SEX, RACE, age_var, agecat, EDUCATION, household_income, BMI, drinkingstatus, drinkingstatus_detailed,
                formerdrinker,gramsperday, frequency, quantity_per_occasion, hed)

# check that there is at least one BRFSS individual in each category in 2000 
nrow(selected %>% group_by(RACE, SEX, EDUCATION, agecat) %>% tally())==144

dropping <- F

# dropping groups not in BRFSS approach - for state-level only 
if(SelectedState!="USA"){
if(dropping==T){
  summary <- selected %>% mutate(SEX = as.factor(SEX),
                                 agecat = as.factor(agecat),
                                 EDUCATION=as.factor(EDUCATION),
                                 RACE=as.factor(RACE)) %>% 
    group_by(SEX,agecat, EDUCATION, RACE, .drop=FALSE) %>% 
    tally() %>% 
    mutate(cat = paste(RACE, SEX, agecat, EDUCATION, sep="")) %>% ungroup() %>% 
    dplyr::select(cat, n) %>% 
    pivot_wider(names_from=cat, values_from=n)
  summary <- summary %>% 
    dplyr::select(sort(tidyselect::peek_vars())) %>% mutate(var="BRFSS")
  
  cons <- cons %>% 
    dplyr::select(sort(tidyselect::peek_vars())) %>% mutate(var="census")
  
  compare <- rbind(cons, summary) %>% 
    pivot_longer(cols=c(BLAF18.24College:WHIM65.79SomeC)) %>% 
    pivot_wider(names_from=var, values_from=value) %>% 
    filter(BRFSS!=0) %>% dplyr::select(name,census) %>% 
    pivot_wider(names_from=name, values_from=census)
  cons <- compare
  
}else{
  missing <- selected %>% mutate(SEX = as.factor(SEX),
                                 agecat = as.factor(agecat),
                                 EDUCATION=as.factor(EDUCATION),
                                 RACE=as.factor(RACE)) %>% 
    group_by(SEX,agecat, EDUCATION, RACE, .drop=FALSE) %>% 
    tally() %>% 
    mutate(cat = paste(RACE, SEX, agecat, EDUCATION, sep="")) %>% ungroup() %>% 
    dplyr::select(cat, n) %>% filter(n==0)
  missingcats <- unique(missing$cat)
  100-round(rowSums(cons[c(missingcats)]) / rowSums(cons)*100,digits=2)
  if(length(missingcats>=1)){
    toreplace <- brfss %>% drop_na() %>% filter(region==unique(selected$region)) %>% mutate(cat=paste(RACE,SEX,agecat,EDUCATION,sep="")) %>% 
      filter(cat %in% missingcats) %>% dplyr::select(-c(cat))
    selected <- rbind(toreplace, selected)
  }
  brfss <- selected
}
}




