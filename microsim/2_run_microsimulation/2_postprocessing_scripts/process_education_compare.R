# process ACS, PSID and Census data for comparison to MSM model output

ACS <- read.csv("1_input_data/ACS_Education_summary_detailed.csv") %>% 
  rename(microsim.init.sex=SEX,
         microsim.init.race=RACE,
         year=YEAR,
         microsim.init.education=EDUC) %>% 
  mutate(datatype="ACS",
         microsim.init.sex = recode(microsim.init.sex, "M"="m","F"="f")) %>% group_by(year, microsim.init.sex,microsim.init.race,
                                                                                      microsim.init.education) %>% 
  summarise(n=sum(sum)) %>%
  mutate(datatype="ACS",
         year=as.numeric(year))

PSID <- read.csv("1_input_data/PSID_processed.csv")

PSID <- PSID %>% rename(microsim.init.sex=sex,
                        microsim.init.education=educationCAT,
                        microsim.init.race=racefinal) %>% 
  mutate(microsim.init.sex=recode(microsim.init.sex, "male"="m","female"="f"),
         microsim.init.race = recode(microsim.init.race, "black"="BLA","white"="WHI",
                                     "hispanic"="SPA", "other"="OTH")) %>% 
  group_by(year, microsim.init.sex, microsim.init.education, microsim.init.race) %>% tally() %>% 
  ungroup() %>% group_by(year, microsim.init.sex,microsim.init.race) %>%
  mutate(datatype="PSID",year=as.numeric(year))

summary <- rbind(ACS, PSID)
options(digits=2)
# add the census data
census <- read.csv("1_input_data/education_constraints.csv") %>% 
  rename(microsim.init.sex=sex,
         microsim.init.education=educationCAT,
         microsim.init.race = race) %>% 
  mutate(microsim.init.sex=recode(microsim.init.sex, "F"="f","M"="m"))

summary <- rbind(summary, census)

# plot1 <- ggplot(data=summary, aes(x=year, y=percentage, colour=as.factor(microsim.init.education),
#                                   shape=as.factor(datatype))) + geom_line() + 
#   geom_point(size=3) + facet_grid(~microsim.init.sex) + ylim(0,0.7) + theme_bw() + 
#   theme(legend.title=element_blank()) 
# plot1

census2010 <- read.csv("1_input_data/census2010education.csv") %>% filter(STATE=="USA") %>% 
  group_by(sex, educationCAT) %>% summarise(n = sum(value)) %>% ungroup() %>% 
  group_by(sex) %>%  
  rename(microsim.init.sex = sex,
         microsim.init.education=educationCAT) %>% 
  mutate(microsim.init.sex=recode(microsim.init.sex, "female"="f","male"="m"),
         datatype="Census",
         microsim.init.race = NA,
         year=2010)

summary <- rbind(summary, census2010)
summary$microsim.init.sex <- ifelse(summary$microsim.init.sex=="f","Female","Male")
rm(ACS, census, census2010, PSID)
