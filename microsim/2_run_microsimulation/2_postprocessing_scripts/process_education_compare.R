# # process ACS, PSID and Census data for comparison to MSM model output

target <- read.csv("SIMAH_workplace/microsim/1_input_data/ACS_popcounts_unweighted_indage.csv") %>% 
  rename(microsim.init.sex=SEX,
         microsim.init.race=RACE,
         year=YEAR,
         microsim.init.education=EDUC) %>% 
  mutate(microsim.init.sex = recode(microsim.init.sex, "M"="m","F"="f")) %>% 
  group_by(STATE, year, microsim.init.sex, microsim.init.education) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% 
  group_by(STATE, year, microsim.init.sex) %>% 
  mutate(targetpercent=n/sum(n)) %>% ungroup() %>% 
  dplyr::select(-n) %>% filter(STATE==SelectedState) %>% 
  dplyr::select(-STATE)
# 
# ACS <- read.csv("SIMAH_workplace/microsim/education_data/ACS_Education_summary_detailed.csv") %>%
#   rename(microsim.init.sex=SEX,
#          microsim.init.race=RACE,
#          year=YEAR,
#          microsim.init.education=EDUC) %>%
#   mutate(datatype="ACS",
#          microsim.init.sex = recode(microsim.init.sex, "M"="m","F"="f")) %>% 
#   group_by(year, microsim.init.sex,microsim.init.race,
#            microsim.init.education) %>%
#   summarise(n=sum(sum)) %>%
#   mutate(datatype="ACS",
#          year=as.numeric(year))
# # 
# # PSID <- read.csv("SIMAH_workplace/education_transitions/PSID_reweighted_2019.csv")
# # 
# # PSID <- PSID %>% rename(microsim.init.sex=sex,
# #                         microsim.init.education=educationCAT,
# #                         microsim.init.race=racefinal) %>% 
# #   mutate(microsim.init.sex=recode(microsim.init.sex, "male"="m","female"="f"),
# #          microsim.init.race = recode(microsim.init.race, "black"="BLA","white"="WHI",
# #                                      "hispanic"="SPA", "other"="OTH", "Asian/PI"="OTH",
# #                                      "Native"="OTH")) %>% 
# #   group_by(year, microsim.init.sex, microsim.init.education, microsim.init.race) %>% tally() %>% 
# #   ungroup() %>% group_by(year, microsim.init.sex,microsim.init.race) %>%
# #   mutate(datatype="PSID",year=as.numeric(year))
# # 
# # summary <- rbind(ACS, PSID)
# # options(digits=2)
# # add the census data
# census <- read.csv("SIMAH_workplace/microsim/1_generating_population/constraints_IPF_final.csv") %>% 
#   pivot_longer(cols=c(BLAF18.24College:WHIM65.79SomeC)) %>% 
#   separate(name, into=c("microsim.init.race","microsim.init.sex",
#                         "age",
#                        "microsim.init.education"), sep=c(3,4,9)) %>% 
#   mutate(microsim.init.sex = recode(microsim.init.sex, "F"="f","M"="m")) %>% 
#   group_by(STATE, microsim.init.sex, microsim.init.education) %>% 
#   summarise(n=sum(value)) %>% ungroup() %>% mutate(year=2000)
#   
# # census <- read.csv("SIMAH_workplace/microsim/education_data/education_constraints.csv") %>% 
# #   rename(microsim.init.sex=sex,
# #          microsim.init.education=educationCAT,
# #          microsim.init.race = race) %>% 
# #   mutate(microsim.init.sex=recode(microsim.init.sex, "F"="f","M"="m")) %>% 
# #   group_by(STATE, microsim.init.sex, microsim.init.education) %>% 
# #   summarise(n=sum(n)) %>% mutate(year=2000)
# 
# # summary <- rbind(summary, census)
# 
# # plot1 <- ggplot(data=summary, aes(x=year, y=percentage, colour=as.factor(microsim.init.education),
# #                                   shape=as.factor(datatype))) + geom_line() + 
# #   geom_point(size=3) + facet_grid(~microsim.init.sex) + ylim(0,0.7) + theme_bw() + 
# #   theme(legend.title=element_blank()) 
# # plot1
# 
# census2010 <- read.csv("SIMAH_workplace/microsim/education_data/census2010education.csv") %>% 
#   group_by(STATE, sex, educationCAT) %>% summarise(n = sum(value)) %>% ungroup() %>% 
#   group_by(STATE, sex) %>%  
#   rename(microsim.init.sex = sex,
#          microsim.init.education=educationCAT) %>% 
#   mutate(microsim.init.sex=recode(microsim.init.sex, "female"="f","male"="m"),
#          year=2010)
# 
# target <- rbind(census, census2010) %>% 
#   group_by(STATE, microsim.init.sex, year) %>% 
#   mutate(percent = n/sum(n)) %>% filter(STATE==SelectedState)
# rm(census, census2010)
# 
# sex <- c("m","f")
# education <- c("LEHS","SomeC","College")
# Year <- 2001:2009
# 
# missing <- expand.grid(SelectedState, sex, education, Year)
# names(missing) <- c("STATE","microsim.init.sex","microsim.init.education", "year")
# missing$n <- NA
# 
# target <- rbind(target,missing)
# 
# target <- target %>% group_by(microsim.init.sex, microsim.init.education) %>% 
#   arrange(microsim.init.sex, microsim.init.education, year) %>% 
#   mutate(n = approx(year, n, year)$y,
#          percent = approx(year, percent, year)$y) %>% 
#   dplyr::select(year, microsim.init.sex, microsim.init.education, percent) %>% 
#   rename(targetpercent=percent)
# 
# rm(missing, sex, education, Year)

# summary <- rbind(summary, census2010)
# summary$microsim.init.sex <- ifelse(summary$microsim.init.sex=="f","Female","Male")
# rm(ACS, census, census2010, PSID)


