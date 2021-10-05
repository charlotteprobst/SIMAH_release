# postprocessing deaths data 
PopPerYear <- Output[[1]]
DeathSummary <- Output[[2]]
# save as 1_microsim_deaths_summary
rm(list=setdiff(ls(), c("PopPerYear", "DeathSummary", "baseorig", "proportion")))

# total pop in each year
tpop <- list() 
for(i in names(PopPerYear)){
  tpop[[paste(i)]] <- PopPerYear[[paste(i)]] %>% mutate(agecat = cut(microsim.init.age,
                                                                     breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,100),
                                                                     labels=c("18-24","25-29","30-34","35-39","40-44","45-49",
                                                                              "50-54","55-59","60-64","65-69","70-74","75-79"))) %>% 
    mutate(microsim.init.sex=as.factor(microsim.init.sex),
           agecat=as.factor(agecat),
           microsim.init.education=as.factor(microsim.init.education),
           microsim.init.race = as.factor(microsim.init.race)) %>% 
    group_by(microsim.init.sex, agecat, microsim.init.education, microsim.init.race) %>% tally() %>% 
    mutate(year=paste(i),
           year=as.integer(year)) %>% rename(sex=microsim.init.sex,
                                     edclass=microsim.init.education,
                                     raceeth = microsim.init.race)
}
tpop <- do.call(rbind, tpop)
summary(tpop)
tpop$n <- tpop$n*(1/proportion)

Summary <- do.call(rbind,DeathSummary)

Summary <- Summary %>% mutate(year=as.factor(year),
                              microsim.init.sex = as.factor(microsim.init.sex),
                              microsim.init.race= as.factor(microsim.init.race),
                              microsim.init.education=as.factor(microsim.init.education),
                              agecat=as.factor(agecat),
                              cause=as.factor(cause)) %>% 
  group_by(year, microsim.init.sex, microsim.init.race, microsim.init.education, agecat, cause, .drop=FALSE) %>% summarise(totaldeaths=sum(dead),
                                                                                          totaldeaths = totaldeaths*(1/proportion)) %>% 
  mutate(datatype="microsim") %>% rename(sex=microsim.init.sex,
                                         edclass=microsim.init.education,
                                         raceeth=microsim.init.race)

tpop$year <- as.factor(tpop$year)
Summary <- left_join(Summary,tpop)

# compare to the target data - read in the original death rates
deathrates <- read.csv("1_input_data/allethn_sumCOD_0019_final.csv") %>% 
  mutate(edclass=recode(edclass, "4+yrs"="College"),
         race = recode(race, "White"="WHI","Black"="BLA","Hispanic"="SPA","Other"="OTH")) %>% rename(raceeth=race)
tpop <- read.csv('1_input_data/allethn_rates_0019_final.csv') %>% select(year,race,edclass,sex,age_gp,TPop)
deathrates <- left_join(deathrates,tpop)
causes <- names(deathrates)[7:16]
deathrates <- deathrates %>% filter(age_gp!="80") %>% filter(year!=2019) %>% 
  mutate(sex=recode(sex, "1"="m","2"="f"),
         raceeth = recode(race, "White"="WHI","Black"="BLA","Hispanic"="SPA","Other"="OTH")) %>% 
  select(year,sex,edclass,raceeth,age_gp,TPop, c(causes)) %>% 
  pivot_longer(cols=LVDCmort:RESTmort, names_to="cause", values_to="deaths") %>% mutate(cause=gsub("mort","",cause)) %>% 
  group_by(year, sex, age_gp, raceeth, edclass, cause) %>% summarise(totaldeaths=sum(deaths),
                                                    n=sum(TPop)) %>% mutate(datatype="target") %>% 
  mutate(agecat = recode(age_gp,
                         "18"="18-24","25"="25-29","30"="30-34","35"="35-39",
                         "40"="40-44","45"="45-49","50"="50-54","55"="55-59",
                         "60"="60-64","65"="65-69","70"="70-74","75"="75-79")) %>% ungroup() %>% select(year, sex, edclass, raceeth, agecat, cause,
                                                                                          totaldeaths, n, datatype)


setdiff(names(Summary),names(deathrates))
setdiff(names(deathrates),names(Summary))
summary(deathrates)
deathrates$year <- as.factor(deathrates$year)

Summary <- rbind(Summary, deathrates)
summary(Summary)
options(digits=2)
Summary$totaldeaths <- round(Summary$totaldeaths, digits=2)
Summary$n <- round(Summary$n, digits=2)

write.csv(Summary, "3_output_data/1_microsim_deaths_summary.csv", row.names=FALSE)

