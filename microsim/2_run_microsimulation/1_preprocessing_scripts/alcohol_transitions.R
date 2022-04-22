# processing the alcohol transition probabilities for including in the microsimulation 
# AlctransitionProbability <- read.csv("SIMAH_workplace/microsim/1_input_data/Transition Probabilities_Alcohol.csv") %>% 
#   mutate(sex = recode(sex, "Men"="m","Women"="f"),
#          race = recode(race, "Hispanic"="SPA", "Black, non-Hispanic"="BLA",
#                        "Other, non-Hispanic"="OTH","White, non-Hispanic"="WHI"),
#          edu = recode(edu, "Low"="LEHS","Med"="SomeC","High"="College"),
#          age= round(age, digits=0)) %>%
#   mutate(cat = paste(age, sex, race, edu, From, sep="_")) %>% 
#   dplyr::select(cat, To, Probability) %>% group_by(cat) %>% 
#   mutate(cumsum = cumsum(Probability))

# AlctransitionProbability <- read.csv("SIMAH_workplace/microsim/1_input_data/AlcUse Annual TP, age continuous.csv") %>% 
#   mutate(sex = recode(sex, "Men"="m","Women"="f"),
#          race = recode(race, "Hispanic"="SPA", "Black, non-Hispanic"="BLA",
#                        "Other, non-Hispanic"="OTH","White, non-Hispanic"="WHI"),
#          edu = recode(edu, "Low"="LEHS","Med"="SomeC","High"="College"),
#          age= round(age, digits=0),
#          From = recode(From, "Abstainer"="Lifetime abstainer", "Former"="Former drinker",
#                        "Category I" = "Low risk", "Category II" = "Medium risk",
#                        "Category III" = "High risk"),
#          To = recode(To, "Abstainer"="Lifetime abstainer", "Former"="Former drinker",
#                        "Category I" = "Low risk", "Category II" = "Medium risk",
#                        "Category III" = "High risk")) %>%
#   mutate(cat = paste(age, sex, race, edu, From, sep="_")) %>% 
#   dplyr::select(cat, To, Probability) %>% group_by(cat) %>% 
#   mutate(cumsum = cumsum(Probability))
# 
# AlctransitionProbability <- read.csv("SIMAH_workplace/microsim/1_input_data/AlcUse 2-year TP, age continuous.csv") %>% 
#   mutate(sex = recode(sex, "Men"="m","Women"="f"),
#          race = recode(race, "Hispanic"="SPA", "Black, non-Hispanic"="BLA",
#                        "Other, non-Hispanic"="OTH","White, non-Hispanic"="WHI"),
#          edu = recode(edu, "Low"="LEHS","Med"="SomeC","High"="College"),
#          age= round(age, digits=0),
#          From = recode(From, "Abstainer"="Lifetime abstainer", "Former"="Former drinker",
#                        "Category I" = "Low risk", "Category II" = "Medium risk",
#                        "Category III" = "High risk"),
#          To = recode(To, "Abstainer"="Lifetime abstainer", "Former"="Former drinker",
#                      "Category I" = "Low risk", "Category II" = "Medium risk",
#                      "Category III" = "High risk")) %>%
#   mutate(cat = paste(age, sex, race, edu, From, sep="_")) %>% 
#   dplyr::select(cat, To, Probability) %>% group_by(cat) %>% 
#   mutate(cumsum = cumsum(Probability))

AlctransitionProbability <- readRDS("SIMAH_workplace/microsim/2_output_data/final_alc_transitionsUSA-1.RDS") %>% 
  rename(To=StateTo)

# AlctransitionProbability <- read.csv("SIMAH_workplace/microsim/1_input_data/AlcUse4, Ages7 2-yr TP.csv") %>% 
#   mutate(sex = recode(sex, "Men"="m","Women"="f"),
#          race = recode(race, "Hispanic"="SPA", "Black, non-Hispanic"="BLA",
#                        "Other, non-Hispanic"="OTH","White, non-Hispanic"="WHI"),
#          edu = recode(edu, "Low"="LEHS","Med"="SomeC","High"="College"),
#          age_cat = gsub("Ages ", "", age_cat),
#          From = recode(From, "Abstainer"="Lifetime abstainer", "Former"="Former drinker",
#                        "Category I" = "Low risk", "Category II" = "Medium risk",
#                        "Category III" = "High risk"),
#          To = recode(To, "Abstainer"="Lifetime abstainer", "Former"="Former drinker",
#                      "Category I" = "Low risk", "Category II" = "Medium risk",
#                      "Category III" = "High risk")) %>%
#   mutate(cat = paste(age_cat, sex, race, edu, From, sep="_")) %>% 
#   dplyr::select(cat, To, Probability) %>% group_by(cat) %>% 
#   mutate(cumsum = cumsum(Probability))

# AlctransitionProbability <- read.csv("SIMAH_workplace/microsim/1_input_data/AlcUse5, Ages7, 2-yr TP.csv") %>% 
#   mutate(sex = recode(sex, "Men"="m","Women"="f"),
#          race = recode(race, "Hispanic"="SPA", "Black, non-Hispanic"="BLA",
#                        "Other, non-Hispanic"="OTH","White, non-Hispanic"="WHI"),
#          edu = recode(edu, "Low"="LEHS","Med"="SomeC","High"="College"),
#          age_cat = gsub("Ages ", "", age_cat),
#          From = recode(From, "Abstainer"="Lifetime abstainer", "Former"="Former drinker",
#                        "Category I" = "Low risk", "Category II" = "Medium risk",
#                        "Category III" = "High risk"),
#          To = recode(To, "Abstainer"="Lifetime abstainer", "Former"="Former drinker",
#                      "Category I" = "Low risk", "Category II" = "Medium risk",
#                      "Category III" = "High risk")) %>%
#   mutate(cat = paste(age_cat, sex, race, edu, From, sep="_")) %>% 
#   dplyr::select(cat, To, Probability) %>% group_by(cat) %>% 
#   mutate(cumsum = cumsum(Probability))


# set up the grams per day categories for the baseline population and brfss donor populations 
basepop <- basepop %>% 
  mutate(AlcCAT = ifelse(formerdrinker==1, "Former drinker",
                         ifelse(formerdrinker!=1 & microsim.init.alc.gpd==0, "Lifetime abstainer",
                                ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>0 & 
                                         microsim.init.alc.gpd<=40, "Low risk",
                                       ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>0 &
                                                microsim.init.alc.gpd<=20, "Low risk",
                                              ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>40 &
                                                       microsim.init.alc.gpd<=60, "Medium risk",
                                                     ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>20 & 
                                                              microsim.init.alc.gpd<=40, "Medium risk",
                                                            ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>60,
                                                                   "High risk",
                                                                   ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>40,
                                                                          "High risk", NA)))))))),
         AlcCAT = ifelse(AlcCAT=="Former drinker", "Non-drinker",
                         ifelse(AlcCAT=="Lifetime abstainer", "Non-drinker",AlcCAT))
         )
brfss <- brfss %>% 
  mutate(AlcCAT = ifelse(formerdrinker==1, "Former drinker",
                         ifelse(formerdrinker!=1 & microsim.init.alc.gpd==0, "Lifetime abstainer",
                                ifelse(formerdrinker!=1 & microsim.init.sex=="m" & microsim.init.alc.gpd>0 & 
                                         microsim.init.alc.gpd<=40, "Low risk",
                                       ifelse(formerdrinker!=1 & microsim.init.sex=="f" & microsim.init.alc.gpd>0 &
                                                microsim.init.alc.gpd<=20, "Low risk",
                                              ifelse(formerdrinker!=1 & microsim.init.sex=="m" & microsim.init.alc.gpd>40 &
                                                       microsim.init.alc.gpd<=60, "Medium risk",
                                                     ifelse(formerdrinker!=1 & microsim.init.sex=="f" & microsim.init.alc.gpd>20 & 
                                                              microsim.init.alc.gpd<=40, "Medium risk",
                                                            ifelse(formerdrinker!=1 & microsim.init.sex=="m" & microsim.init.alc.gpd>60,
                                                                   "High risk",
                                                                   ifelse(formerdrinker!=1 & microsim.init.sex=="f" & microsim.init.alc.gpd>40,
                                                                          "High risk", NA)))))))),
         AlcCAT=ifelse(AlcCAT=="Former drinker","Non-drinker",
                       ifelse(AlcCAT=="Lifetime abstainer","Non-drinker",AlcCAT))
         )
# sum1 <- basepop %>% group_by(microsim.init.sex, AlcCAT) %>% tally() %>% ungroup() %>% group_by(microsim.init.sex) %>% 
#   mutate(proportion=n/sum(n), data='basepop')
# sum2 <- brfss %>% group_by(microsim.init.sex, AlcCAT) %>% tally() %>% ungroup() %>% group_by(microsim.init.sex) %>% 
#   mutate(proportion=n/sum(n), data='brfss')
# sum <- rbind(sum1, sum2) %>% dplyr::select(microsim.init.sex, AlcCAT,proportion,data) %>% 
#   pivot_wider(names_from=data,values_from=proportion)
