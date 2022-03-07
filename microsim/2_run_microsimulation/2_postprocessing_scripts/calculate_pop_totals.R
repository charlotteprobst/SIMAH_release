basepopsummary <- basepop %>% mutate(
  agecat = cut(microsim.init.age,
               breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,100),
               labels=c("18-24","25-29","30-34","35-39","40-44",
                        "45-49","50-54","55-59","60-64","65-69",
                        "70-74","75-79"))) %>% 
  group_by(microsim.init.sex, microsim.init.race,
                                       microsim.init.education, agecat) %>% 
    tally() %>% mutate(n=n*(1/proportion),
                       year=2000)

OutputSummary <- Output %>% filter(year!=2000) %>% mutate(n=n*(1/proportion)) %>% 
  dplyr::select(-seed) %>% mutate(year=as.numeric(as.character(year)))

OutputSummary <- rbind(OutputSummary, basepopsummary) %>% 
  mutate(age_gp = substr(agecat, 1,2),
         sex = ifelse(microsim.init.sex=="m",1,2),
         race = recode(microsim.init.race, "BLA"="Black",
                       "WHI"="White","SPA"="Hispanic",
                       "OTH"="Other"),
         edclass=microsim.init.education) %>% ungroup() %>% 
  dplyr::select(year, sex, age_gp, race, edclass, n) %>% 
  rename(TPop=n) %>% mutate(
                            TPop = round(TPop)) %>% distinct()
write.csv(OutputSummary, "SIMAH_workplace/microsim/2_output_data/pop_counts_simulation_2000_2020.csv",
          row.names=F)
compare <- read.csv("SIMAH_workplace/microsim/2_output_data/population_counts_census_ACS.csv") %>% 
  mutate(data="CPS")

compare <- rbind(compare, OutputSummary) %>% 
  filter(age_gp=="18") %>% filter(year==2000) %>% pivot_wider(names_from=data, values_from=TPop)

