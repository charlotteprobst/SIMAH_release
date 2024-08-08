# generate target file 
library(tidyverse)
library(plotrix)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
setwd(paste(WorkingDirectory))

SelectedState <- "USA"

brfss <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_1984_2020_final.RDS") %>% 
  filter(age_var<=80) %>% filter(State==SelectedState) %>% 
  mutate(microsim.init.race = recode(race_eth,"White"="WHI","Black"="BLA", "Hispanic"="SPA", "Other"="OTH"),
         microsim.init.sex = recode(sex_recode,"Male"="m","Female"="f"))
  
prevalence <- brfss %>% group_by(YEAR, microsim.init.sex) %>% 
  summarise(prevalenceM = mean(drinkingstatus),
            prevalenceSE = std.error(drinkingstatus))

others <- brfss %>% ungroup() %>% group_by(YEAR, microsim.init.sex) %>% 
  filter(drinkingstatus==1) %>% 
  add_tally() %>% 
  summarise(frequencyM = mean(frequency),
            frequencySE = std.error(frequency),
            quantityM = mean(gramsperday),
            quantitySE = std.error(gramsperday))

targets <- left_join(prevalence, others) %>% 
  pivot_longer(prevalenceM:quantitySE) %>% 
  mutate(target_type = ifelse(grepl("prevalence",name), "prevalence",
                              ifelse(grepl("quantity",name), "quantity",
                                     ifelse(grepl("frequency",name), "frequency", NA))),
         target_measure = ifelse(grepl("SE",name), "SE","Mean")) %>% 
  dplyr::select(-name) %>% 
  pivot_wider(names_from = target_measure, values_from=value)

ggplot(data=targets, aes(x=YEAR, y= Mean, colour=microsim.init.sex)) + geom_line() + 
  facet_grid(rows=vars(target_type), scales="free") + ylim(0,NA)

write.csv(targets, paste("SIMAH_workplace/microsim/1_input_data/agent_files/",SelectedState, "target_alcoholCASCADE", sep="", ".csv"), row.names=FALSE)
