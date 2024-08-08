# recalculate error - just split by education 
summary <- read.csv("3_output_data/1_mortality_rates_data_education.csv") %>% select(-X) %>% 
  pivot_wider(names_from=datatype, values_from=rate) %>% 
  mutate(error=microsim-target) %>% 
  group_by(edclass, cause) %>% summarise(meanerror=round(abs(mean(error)),digits=2),
                                         sd = round(abs(sd(error)),digits=2)) %>% 
  pivot_wider(names_from=edclass, values_from=c(meanerror,sd)) %>% 
  select(cause, meanerror_LEHS, sd_LEHS, meanerror_SomeC, sd_SomeC,
         meanerror_College, sd_College) %>% 
  
  mutate(cause= factor(cause, levels=c("AUD","LVDC","IJ","MVACC","UIJ","IHD","HYPHD","STR","DM","REST"))) %>% 
  ungroup() %>% group_by(cause) %>% arrange(desc(meanerror_LEHS), .by_group=T)
write.csv(summary, "3_output_data/SuppTable3.csv", row.names=FALSE)

summary$cause
# recalculate error - just split by race/ethnicity 
summary <- read.csv("3_output_data/1_mortality_rates_data_race.csv") %>%select(-X) %>% 
  pivot_wider(names_from=datatype, values_from=rate) %>% 
  mutate(error=microsim-target) %>% 
  group_by(raceeth, cause) %>% summarise(meanerror=round(abs(mean(error)),digits=2),
                                         sd = round(abs(sd(error)),digits=2)) %>% 
  pivot_wider(names_from=raceeth, values_from=c(meanerror,sd)) %>% 
  select(cause, meanerror_WHI, sd_WHI, meanerror_BLA, sd_BLA,
         meanerror_SPA, sd_SPA, meanerror_OTH, sd_OTH) %>% 
  mutate(cause= factor(cause, levels=c("AUD","LVDC","IJ","MVACC","UIJ","IHD","HYPHD","STR","DM","REST"))) %>% 
  ungroup() %>% group_by(cause) %>% arrange(desc(meanerror_BLA), .by_group=T)
write.csv(summary, "3_output_data/SuppTable4.csv", row.names=FALSE)


# recalculate error - split by both education and race/ethnicity
summary <- read.csv("3_output_data/1_mortality_rates_data_raceeducation.csv") %>% select(-X) %>% 
  pivot_wider(names_from=datatype, values_from=rate) %>% mutate(microsim=ifelse(is.na(microsim),0,microsim)) %>%
  mutate(error=microsim-target) %>% 
  group_by(raceeth, edclass, cause) %>% summarise(meanerror=round(abs(mean(error)),digits=2),
                                         sd = round(abs(sd(error)),digits=2)) %>% 
  pivot_wider(names_from=c(raceeth), values_from=c(meanerror,sd)) %>% 
  select(cause, meanerror_WHI, sd_WHI, meanerror_BLA, sd_BLA,
         meanerror_SPA, sd_SPA, meanerror_OTH, sd_OTH) %>% 
  mutate(cause= factor(cause, levels=c("AUD","LVDC","IJ","MVACC","UIJ","IHD","HYPHD","STR","DM","REST"))) %>% 
  ungroup() %>% group_by(edclass) %>% arrange(cause, .by_group=T)
write.csv(summary, "3_output_data/SuppTable5.csv", row.names=FALSE)
