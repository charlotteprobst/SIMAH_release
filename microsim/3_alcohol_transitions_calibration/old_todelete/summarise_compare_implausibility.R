summarise_compare <- function(directory){
  # read in the output file 
  Output <- read_csv(paste0(directory, "/output-",wave, ".csv"))
  output <- summarise_model_alcohol_output(Output)
  implausibility <- calculate_implausibility_alcohol(output, targets)
  return(implausibility)
}

# NESARC main 
targets <- generate_targets_alcohol(brfss)
directory <- "/Users/charlottebuckley/Google Drive/SIMAH Sheffield/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/NESARC_compare"
implausibility_NESARC_main <- summarise_compare(directory)

sub <- implausibility_NESARC_uninformed %>% 
  filter(AlcCAT=="Medium risk") %>% 
  filter(microsim.init.race=="WHI") %>% 
  filter(agecat=="65+" & microsim.init.education=="College") %>% 
  group_by(samplenum, microsim.init.education, agecat) %>% 
  mutate(max = max(implausibility),
         flag = ifelse(max==implausibility,1,0)) %>% 
  filter(max<=3)

directory <- "/Users/charlottebuckley/Google Drive/SIMAH Sheffield/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/NESARC_raceunin"
implausibility_NESARC_race <- summarise_compare(directory)
TPs_race <- read_TPs(directory) %>% mutate(type="raceuninformed")

directory <- "/Users/charlottebuckley/Google Drive/SIMAH Sheffield/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/NESARC_under79"
implausibility_NESARC_under79 <- summarise_compare(directory)
TPs_under79 <- read_TPs(directory) %>% mutate(type="under79model")

directory <- "/Users/charlottebuckley/Google Drive/SIMAH Sheffield/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/NESARC_eduunin"
implausibility_NESARC_edu <- summarise_compare(directory)
TPs_edu <- read_TPs(directory) %>% mutate(type="eduuninformed")


directory <- "/Users/charlottebuckley/Google Drive/SIMAH Sheffield/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/uninformed_prior"
implausibility_NESARC_uninformed <- summarise_compare(directory)
TPs_uninformed <- read_TPs(directory) %>% mutate(type="uninformed")

directory <- "/Users/charlottebuckley/Google Drive/SIMAH Sheffield/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/NESARC_ageunin"
implausibility_NESARC_age <- summarise_compare(directory)
TPs_age <- read_TPs(directory) %>% mutate(type="ageuninformed")


# compare the TPs
TPs_compare <- rbind(TPs_race, TPs_edu, TPs_race, TPs_age, TPs_uninformed, TPs_under79)

summary_TP <- TPs_compare %>% group_by(sex, agecat, race, education, StateFrom, StateTo, type) %>% 
  summarise(min = min(probability),
            max = max(probability),
            mean = mean(probability))

test <- summary_TP %>% filter(race=="White" & StateFrom=="Medium risk" & 
                                education=="College" & agecat=="65+") %>% 
  mutate(StateTo= factor(StateTo,
                         levels=c("Non-drinker","Low risk","Medium risk","High risk")))

test <- left_join(test, TPs_unin_sub)


ggplot(data=test, aes(x=type, y=mean, ymin=min, ymax=max)) + 
  geom_pointrange() + 
  geom_hline(aes(yintercept=intercept),linetype="dashed") + 
  coord_flip() +
  theme_bw() + 
  facet_grid(cols=vars(StateTo), rows=vars(sex)) + xlab("transition probability") + 
  ggtitle("TPs for moving from Medium risk category")



