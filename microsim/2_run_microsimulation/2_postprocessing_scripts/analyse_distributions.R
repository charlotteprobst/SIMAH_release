
toplhsagesp <- read.csv("SIMAH_workplace/microsim/2_output_data/calibration_output_fixed_agesp/lhsSamples_wave15.csv") %>% 
  pivot_longer(BETA_MALE_MORTALITY:DECAY_SPEED) %>% 
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
                                "DECAY_SPEED","IRR_correlation"))) %>% 
  arrange(name) %>% 
  mutate(summarystat = ifelse(name=="THRESHOLD" | name=="THRESHOLD_MODIFIER" | name=="DECAY_SPEED",
                              paste0(min, "-", max), paste0(sd))) %>% 
  dplyr::select(name, mean, summarystat) %>% mutate(type="age specific")

toplhsagest <-  read.csv("SIMAH_workplace/microsim/2_output_data/calibration_output_fixed/lhsSamples_wave15.csv") %>% 
  pivot_longer(BETA_MALE_MORTALITY:DECAY_SPEED) %>% 
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
                                "DECAY_SPEED","IRR_correlation"))) %>% 
  arrange(name) %>% 
  mutate(summarystat = ifelse(name=="THRESHOLD" | name=="THRESHOLD_MODIFIER" | name=="DECAY_SPEED",
                              paste0(min, "-", max), paste0(sd))) %>% 
  dplyr::select(name, mean, summarystat) %>% mutate(type="age standardized")

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

