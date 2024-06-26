# Set up COVID education transition probabilities to align with existing TPs

transitionProbability <- read.csv("SIMAH_workplace/education_transitions/2021/annual_education_TPs_2019_2021_detail.csv")

transitionProbability$StateFrom <- as.character(transitionProbability$StateFrom)
transitionProbability$StateTo <- as.character(transitionProbability$StateTo)

transitionProbability$StateFrom <- parse_number(transitionProbability$StateFrom)
transitionProbability$StateTo <- parse_number(transitionProbability$StateTo)

transitionProbability$Time <- "2019-2021"

# transitionProbability %>% group_by(agecat, sex, racefinal2, StateFrom, Time) %>% summarise(sum(Upper))

transitionProbability <- transitionProbability %>% 
  rename(Prob = estimates.State, 
         Lower = L, 
         Upper = U,
         race = racefinal2,
         age = agecat)

# summary <- transitionProbability %>% dplyr::select(Time, Prob, Lower, Upper, age, sex, race, Transition)

transitions <- transitionProbability %>% group_by(age, sex, race, StateFrom) %>%
  mutate(cumsum=cumsum(Prob),
         sex=recode(sex,"1"="female", "0"="male"), #check
         cat = paste(Time, age, sex, race, "STATEFROM", StateFrom, sep="_"))

transitions <- data.frame(transitions)
transitions <- transitions %>% dplyr::select(cat, StateTo, Prob) %>% 
  arrange(cat, StateTo) %>% 
  group_by(cat) %>% mutate(cumsum=cumsum(Prob)) %>% ungroup() %>% dplyr::select(-c(Prob))
transitions$cumsum <- ifelse(transitions$cumsum>=0.9999, 1, transitions$cumsum)

saveRDS(transitions, paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/final_ed_transitions_covid.RDS"))

