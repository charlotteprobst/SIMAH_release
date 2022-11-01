
toplhsagesp <- read.csv("SIMAH_workplace/microsim/2_output_data/calibration_output_decay_agesp/lhsSamples_wave15.csv") %>% 
  pivot_longer(BETA_MALE_MORTALITY:DECAY_LENGTH) %>% 
  group_by(name) %>% 
  summarise(min = round(min(value),digits=10),
            max = round(max(value),digits=10),
            mean = round(mean(value),digits=10),
            sd = round(sd(value),digits=10)) %>% mutate(type="age specific") %>% 
  mutate(name = factor(name, 
                       levels=c("BETA_MALE_MORTALITY","BETA_FEMALE_MORTALITY",
                                "BETA_FORMER_DRINKERS_MEN","BETA_FORMER_DRINKERS_WOMEN",
                                "METABOLIC_BETA1_MALE","METABOLIC_BETA2_MALE",
                                "METABOLIC_BETA1_FEMALE","METABOLIC_BETA2_FEMALE",
                                "BETA_HEPATITIS","THRESHOLD","THRESHOLD_MODIFIER",
                                "DECAY_SPEED","DECAY_LENGTH","IRR_correlation"))) %>% 
  arrange(name) %>% 
  mutate(summarystat = ifelse(name=="THRESHOLD" | name=="THRESHOLD_MODIFIER" | name=="DECAY_SPEED" | name=="DECAY_LENGTH",
                              paste0(min, "-", max), paste0(sd))) %>% 
  dplyr::select(name, mean, summarystat) %>% mutate(type="age specific")

toplhsagest <-  read.csv("SIMAH_workplace/microsim/2_output_data/calibration_output_decay/lhsSamples_wave15.csv") %>% 
  pivot_longer(BETA_MALE_MORTALITY:DECAY_LENGTH) %>% 
  group_by(name) %>% 
  summarise(min = round(min(value),digits=10),
            max = round(max(value),digits=10),
            mean = round(mean(value),digits=10),
            sd = round(sd(value),digits=10)) %>% mutate(type="age standardized") %>% 
  mutate(name = factor(name, 
                       levels=c("BETA_MALE_MORTALITY","BETA_FEMALE_MORTALITY",
                                "BETA_FORMER_DRINKERS_MEN","BETA_FORMER_DRINKERS_WOMEN",
                                "METABOLIC_BETA1_MALE","METABOLIC_BETA2_MALE",
                                "METABOLIC_BETA1_FEMALE","METABOLIC_BETA2_FEMALE",
                                "BETA_HEPATITIS","THRESHOLD","THRESHOLD_MODIFIER",
                                "DECAY_SPEED","DECAY_LENGTH","IRR_correlation"))) %>% 
  arrange(name) %>% 
  mutate(summarystat = ifelse(name=="THRESHOLD" | name=="THRESHOLD_MODIFIER" | name=="DECAY_SPEED" | name=="DECAY_LENGTH",
                              paste0(min, "-", max), paste0(sd))) %>% 
  dplyr::select(name, mean, summarystat) %>% mutate(type="age standardized")

toplhs_table <- rbind(toplhsagesp, toplhsagest) %>% 
  pivot_wider(names_from=type, values_from=c(mean,summarystat)) %>% 
  dplyr::select(name, `mean_age standardized`, `summarystat_age standardized`,
                `mean_age specific`, `summarystat_age specific`)
write.csv(toplhs_table, "SIMAH_workplace/microsim/2_output_data/publication/Table2.csv", row.names=F)

toplhs_forriskfunctions = rbind(toplhsagesp, toplhsagest) %>% 
  filter(name=="BETA_MALE_MORTALITY" | name =="BETA_FEMALE_MORTALITY" 
         | name=="METABOLIC_BETA1_MALE" | name=="METABOLIC_BETA2_MALE" |
           name=="METABOLIC_BETA1_FEMALE" | name=="METABOLIC_BETA2_FEMALE" |
           name=="BETA_HEPATITIS")


PE <- data.frame(name = c("BETA_MALE_MORTALITY","BETA_FEMALE_MORTALITY","METABOLIC_BETA1_MALE",
                          "METABOLIC_BETA2_MALE", "METABOLIC_BETA1_FEMALE", "METABOLIC_BETA2_FEMALE",
                          "BETA_HEPATITIS"),
                 mean = c(0.0227414,0.0396643,-1.02011,-0.1274623,3.03,-4.31,0.009854),
                 summarystat = c(0.0111,0.05370378,0.3083,0.0440,1.0536,2.2322,0.0009),
                 type = "prior")




toplhs_forriskfunctions <- rbind(toplhs_forriskfunctions, PE)
write.csv(toplhs_forriskfunctions, 
          "SIMAH_workplace/microsim/2_output_data/publication/riskfunctions.csv", row.names=F)

