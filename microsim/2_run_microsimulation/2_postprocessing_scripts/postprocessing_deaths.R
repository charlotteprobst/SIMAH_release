# postprocessing deaths data 
# rm(list=setdiff(ls(), c("PopPerYear", "DeadSummary", "baseorig", "proportion")))

# PopPerYear <- readRDS("output_data/PopPerYear.RDS")
PopPerYear <- Output[[1]]
# total pop in each year
tpop <- list() 
for(i in names(PopPerYear)){
  tpop[[paste(i)]] <- PopPerYear[[paste(i)]] %>% mutate(agecat = cut(microsim.init.age,
                                                                     breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,100),
                                                                     labels=c("18-24","25-29","30-34","35-39","40-44","45-49",
                                                                              "50-54","55-59","60-64","65-69","70-74","75-79"))) %>% 
    group_by(microsim.init.sex, agecat, microsim.init.education, microsim.init.race) %>% tally() %>% 
    mutate(year=paste(i),
           year=as.integer(year)) %>% rename(Sex=microsim.init.sex,
                                     edclass=microsim.init.education,
                                     raceeth = microsim.init.race)
}
tpop <- do.call(rbind, tpop)
summary(tpop)

tpop%>% filter(year==2000) %>% group_by(raceeth, edclass) %>% summarise(sum(n))


tpop$n <- tpop$n*(1/proportion)

DeadSummary <- Output[[2]]
Summary <- do.call(rbind,DeadSummary)

Summary <- Summary %>% 
  group_by(year, microsim.init.sex, microsim.init.race, microsim.init.education, agecat, cause) %>% summarise(totaldeaths=sum(dead),
                                                                                          totaldeaths = totaldeaths*(1/proportion)) %>% 
  mutate(datatype="microsim") %>% rename(Sex=microsim.init.sex,
                                         edclass=microsim.init.education,
                                         raceeth=microsim.init.race)
Summary <- left_join(Summary,tpop)

summary(Summary)

# compare to the target data - read in the original death rates
deathrates <- read.csv("SIMAH_workplace/microsim/1_input_data/allethn_sumCOD_0019_final.csv")
causes <- names(deathrates)[7:16]
deathrates <- deathrates %>% filter(age_gp!="80") %>% filter(year!=2019) %>% 
  mutate(Sex=recode(sex, "1"="m","2"="f"),
         raceeth = recode(race, "White"="WHI","Black"="BLA","Hispanic"="SPA","Other"="OTH")) %>% 
  dplyr::select(year,Sex,edclass,raceeth,age_gp, c(causes)) %>% 
  pivot_longer(cols=LVDCmort:RESTmort, names_to="cause", values_to="deaths") %>% mutate(cause=gsub("mort","",cause)) %>% 
  group_by(year, Sex, age_gp, raceeth, edclass, cause) %>% summarise(totaldeaths=sum(deaths)) %>% mutate(datatype="target") %>% 
  mutate(agecat = recode(age_gp,
                         "18"="18-24","25"="25-29","30"="30-34","35"="35-39",
                         "40"="40-44","45"="45-49","50"="50-54","55"="55-59",
                         "60"="60-64","65"="65-69","70"="70-74","75"="75-79")) %>% ungroup() %>% dplyr::select(year, Sex, edclass, raceeth, agecat, cause,
                                                                                          totaldeaths, datatype)

TPop <- read.csv("SIMAH_workplace/microsim/1_input_data/allethn_rates_0019_final.csv") %>% 
  filter(age_gp!="80") %>% filter(year!=2019) %>% 
  dplyr::select(year, race, sex, edclass, age_gp, TPop) %>% 
  rename(Sex=sex, raceeth=race, agecat=age_gp) %>% 
  mutate(Sex=recode(Sex, "1"="m","2"="f"),
         raceeth = recode(raceeth, "White"="WHI","Black"="BLA","Hispanic"="SPA","Other"="OTH")) %>% 
  mutate(agecat = recode(agecat,
                         "18"="18-24","25"="25-29","30"="30-34","35"="35-39",
                         "40"="40-44","45"="45-49","50"="50-54","55"="55-59",
                         "60"="60-64","65"="65-69","70"="70-74","75"="75-79")) %>% ungroup() %>% 
  group_by(year, raceeth, Sex, agecat) %>% summarise(n=sum(TPop))
deathrates <- left_join(deathrates, TPop)
                                                                                                               

setdiff(names(Summary),names(deathrates))
setdiff(names(deathrates),names(Summary))
summary(deathrates)

Summary <- rbind(Summary, deathrates)
summary(Summary)

# percentages by age category

TotalPops <- Summary %>% dplyr::select(year, Sex, edclass, agecat, datatype, n) %>% 
  group_by(year, Sex, edclass, agecat, datatype) %>% summarise(n=sum(n)) %>% ungroup() %>% 
  group_by(year, Sex, edclass, datatype) %>% mutate(percent=n/sum(n)) %>% dplyr::select(year, Sex, edclass, agecat, datatype, percent)

test <- TotalPops %>% filter(year==2000 & Sex=="f" & edclass=="College")
sum(test$percent)

Summary <- Summary %>% rename(sex=Sex)

write.csv(Summary, "SIMAH_workplace/protocol/output_data/1_microsim_deaths_summary2.csv", row.names=FALSE)

options(digits=2)


library(ggplot2)
ggplot(data=subset(Summary, cause!="REST"), aes(x=year, y=totaldeaths, colour=as.factor(cause), linetype=as.factor(datatype))) + 
  geom_line() + 
  geom_point() + facet_wrap(~Sex+edclass, ncol=3, scales="free") + theme_bw() + theme(legend.title=element_blank()) + 
  scale_colour_manual("", values=c(1,2,3,4,5,6,7,8,9))+
  scale_linetype_manual("", values=c(1,2))

ggsave("CausesOfDeath_byeducationnew.png", dpi=300, width=33, height=19, units="cm")

# remove IHD to see the rest better
ggplot(data=subset(Summary, cause!="REST" & cause!="IHD"), aes(x=year, y=totaldeaths, colour=as.factor(cause), linetype=as.factor(datatype))) + 
  geom_line() + 
  geom_point() + facet_wrap(~Sex+edclass, ncol=3, scales="free") + theme_bw() + theme(legend.title=element_blank()) + 
  scale_colour_manual("", values=c(1,2,3,4,5,6,7,8,9))+
  scale_linetype_manual("", values=c(1,2))


ggsave("CausesOfDeath_byeducation_removednew.png", dpi=300, width=33, height=19, units="cm")


