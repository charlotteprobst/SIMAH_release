# postprocessing alcohol transitions data 

df <- Output[[1]]

for(i in 2000:2018){
  df[[paste(i)]]$Year <- i
}

df <- do.call(rbind, df)

brfss <- read_csv("SIMAH_workplace/microsim/brfss_data/BRFSS_upshift_BMIUSA.csv") %>% filter(YEAR>=2000) %>% 
  mutate(agecat = cut(AGE,
                      breaks=c(0,24,34,44,54,64,100),
                      labels=c("18-24","25-34","35-44",
                               "45-54","55-64","65+"))) %>% 
  dplyr::select(YEAR, STATE, AGE, agecat, RACE, SEX, EDUCATION, DRINKINGSTATUS_NEW, alcgpd_new,
                imputeddrinking,INCOMENEW) %>%
  rename(microsim.init.age=AGE, microsim.init.race=RACE, microsim.init.sex=SEX, microsim.init.education=EDUCATION,
         microsim.init.drinkingstatus=DRINKINGSTATUS_NEW, microsim.init.alc.gpd=alcgpd_new, microsim.init.income=INCOMENEW) %>%
  mutate(microsim.init.race = recode(microsim.init.race, "Black"="BLA","Hispanic"="SPA","Other"="OTH","White"="WHI"),
         microsim.init.sex = recode(microsim.init.sex, "Female"="f","Male"="m"),
         microsim.init.education = recode(microsim.init.education, "1"="LEHS","2"="SomeC","3"="College"),
         AlcCAT = ifelse(imputeddrinking==1, "Former drinker",
                       ifelse(imputeddrinking!=1 & microsim.init.alc.gpd==0, "Lifetime abstainer",
                              ifelse(imputeddrinking!=1 & microsim.init.sex=="m" & microsim.init.alc.gpd>0 & 
                                       microsim.init.alc.gpd<=40, "Low risk",
                                     ifelse(imputeddrinking!=1 & microsim.init.sex=="f" & microsim.init.alc.gpd>0 &
                                              microsim.init.alc.gpd<=20, "Low risk",
                                            ifelse(imputeddrinking!=1 & microsim.init.sex=="m" & microsim.init.alc.gpd>40 &
                                                     microsim.init.alc.gpd<=60, "Medium risk",
                                                   ifelse(imputeddrinking!=1 & microsim.init.sex=="f" & microsim.init.alc.gpd>20 & 
                                                            microsim.init.alc.gpd<=40, "Medium risk",
                                                          ifelse(imputeddrinking!=1 & microsim.init.sex=="m" & microsim.init.alc.gpd>60,
                                                                 "High risk",
                                                                 ifelse(imputeddrinking!=1 & microsim.init.sex=="f" & microsim.init.alc.gpd>40,
                                                                        "High risk", NA)))))))))


summary <- df %>% mutate(agecat = cut(microsim.init.age,
                                      breaks=c(0,24,34,44,54,64,100),
                                      labels=c("18-24","25-34","35-44",
                                               "45-54","55-64","65+"))) %>% 
  group_by(Year, microsim.init.sex, AlcCAT) %>% tally() %>% 
  mutate(data="microsimulation") %>% ungroup() %>% group_by(Year, microsim.init.sex) %>% 
  mutate(proportion=n/sum(n))

summarybrfss <- brfss %>% group_by(YEAR, microsim.init.sex,
                                   AlcCAT)%>% 
  tally() %>% rename(Year=YEAR) %>% 
  mutate(data="brfss") %>% ungroup() %>% 
  group_by(Year, microsim.init.sex) %>% 
  mutate(proportion=n/sum(n))

summary <- rbind(summary, summarybrfss) %>% 
  mutate(AlcCAT = factor(AlcCAT,
                         levels=c("Lifetime abstainer","Former drinker",
                                  "Low risk", "Medium risk", "High risk")),
                         microsim.init.sex = recode(microsim.init.sex,
                                                    "m"="Men","f"="Women"))

ggplot(data=summary, aes(x=Year, y=proportion, colour=AlcCAT, linetype=data)) + 
  geom_line() + facet_grid(cols=vars(microsim.init.sex), scales="free") + 
  scale_y_continuous(labels=scales::percent, limits=c(0,NA)) +   theme_bw() +
  theme(legend.title=element_blank(),
        text = element_text(size=12),
        legend.position="bottom") + 
  scale_colour_brewer(type="seq", palette="Dark2")
ggsave("SIMAH_workplace/microsim/2_output_data/plots/alcohol_bysex.png",
       dpi=300, width=33, height=17, units="cm")
  

summary <- df %>% mutate(agecat = cut(microsim.init.age,
                                      breaks=c(0,24,34,44,54,64,100),
                                      labels=c("18-24","25-34","35-44",
                                               "45-54","55-64","65+"))) %>% 
  group_by(Year, microsim.init.sex, microsim.init.race, AlcCAT) %>% tally() %>% 
  mutate(data="microsimulation") %>% ungroup() %>% group_by(Year, microsim.init.sex, microsim.init.race) %>% 
  mutate(proportion=n/sum(n))

summarybrfss <- brfss %>% group_by(YEAR, microsim.init.sex,
                                   microsim.init.race,
                                   AlcCAT)%>% 
  tally() %>% rename(Year=YEAR) %>% 
  mutate(data="brfss") %>% ungroup() %>% 
  group_by(Year, microsim.init.sex, microsim.init.race) %>% 
  mutate(proportion=n/sum(n))

summary <- rbind(summary, summarybrfss) %>% 
  mutate(AlcCAT = factor(AlcCAT,
                         levels=c("Lifetime abstainer","Former drinker",
                                  "Low risk", "Medium risk", "High risk")),
         microsim.init.sex = recode(microsim.init.sex,
                                    "m"="Men","f"="Women"),
         microsim.init.race = recode(microsim.init.race,
                                     "BLA"="Black","WHI"="White",
                                     "SPA"="Hispanic","OTH"="Others"))

ggplot(data=summary, aes(x=Year, y=proportion, colour=AlcCAT, linetype=data)) + 
  geom_line() + facet_grid(cols=vars(microsim.init.sex),rows=vars(microsim.init.race), scales="free") + 
  scale_y_continuous(labels=scales::percent, limits=c(0,NA)) +   theme_bw() +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        text = element_text(size=12)) + 
  scale_colour_brewer(type="seq", palette="Dark2")
ggsave("SIMAH_workplace/microsim/2_output_data/plots/alcohol_byrace.png",
       dpi=300, width=33, height=17, units="cm")

summary <- df %>% mutate(agecat = cut(microsim.init.age,
                                      breaks=c(0,24,34,44,54,64,100),
                                      labels=c("18-24","25-34","35-44",
                                               "45-54","55-64","65+"))) %>% 
  group_by(Year, microsim.init.sex, microsim.init.education, AlcCAT) %>% tally() %>% 
  mutate(data="microsimulation") %>% ungroup() %>% group_by(Year, microsim.init.sex, microsim.init.education) %>% 
  mutate(proportion=n/sum(n))

summarybrfss <- brfss %>% group_by(YEAR, microsim.init.sex,
                                   microsim.init.education,
                                   AlcCAT)%>% 
  tally() %>% rename(Year=YEAR) %>% 
  mutate(data="brfss") %>% ungroup() %>% 
  group_by(Year, microsim.init.sex, microsim.init.education) %>% 
  mutate(proportion=n/sum(n))

summary <- rbind(summary, summarybrfss) %>% 
  mutate(AlcCAT = factor(AlcCAT,
                         levels=c("Lifetime abstainer","Former drinker",
                                  "Low risk", "Medium risk", "High risk")),
         microsim.init.sex = recode(microsim.init.sex,
                                    "m"="Men","f"="Women"),
         microsim.init.education = recode(microsim.init.education,
                                     "LEHS"="High school or less",
                                     "SomeC"="Some College",
                                     "College"="College+"))

ggplot(data=summary, aes(x=Year, y=proportion, colour=microsim.init.education, linetype=data)) + 
  geom_line() + facet_grid(cols=vars(microsim.init.sex),rows=vars(AlcCAT), scales="free") + 
  scale_y_continuous(labels=scales::percent, limits=c(0,NA)) +   theme_bw() +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        text = element_text(size=12)) + 
  scale_colour_brewer(type="seq", palette="Dark2")
ggsave("SIMAH_workplace/microsim/2_output_data/plots/alcohol_byeducation.png",
       dpi=300, width=33, height=17, units="cm")
