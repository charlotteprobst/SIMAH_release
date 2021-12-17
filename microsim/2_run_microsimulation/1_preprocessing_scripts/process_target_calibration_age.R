cirrhosismortality <- read.csv("SIMAH_workplace/microsim/1_input_data/LC_deaths_CDC_2.csv")[c(1:3,35:48)]
cirrhosismortality <- cirrhosismortality %>% mutate(CDC..80 = CDC..80/5,
                                                    CDC.75.=CDC.75_79+CDC..80,
                                                    CDC.25_34 = CDC.25_29+CDC.30_34,
                                                    CDC.35_44 = CDC.35_39+CDC.40_44,
                                                    CDC.45_54 = CDC.45_49+CDC.50_54,
                                                    CDC.55_64 = CDC.55_59+CDC.60_64,
                                                    CDC.65_74 = CDC.65_69+CDC.70_74) %>% dplyr::select(States, Sex, Year, CDC.15_19, CDC.20_24, CDC.25_34,
                                                                                                CDC.35_44, CDC.45_54, CDC.55_64,
                                                                                                CDC.65_74, CDC.75.) %>% 
  pivot_longer(cols=CDC.15_19:CDC.75., names_to="agegroup", values_to="deaths") %>% 
  mutate(agegroup=gsub("CDC.", "", agegroup),
         agegroup=gsub("_","-", agegroup),
         sex=recode(Sex, "1"="m","2"="f"),
         State=recode(States, "USA"="USA",
                      "CA"="California",
                      "MN"="Minnesota",
                      "NY"="New York",
                      "TN"="Tennessee",
                      "TX"="Texas")) %>% filter(State==SelectedState) %>% 
  mutate(Deaths=deaths*proportion,
         count=ifelse(agegroup=="15-19",Deaths/5*2,Deaths),
         cat=paste(agegroup, sex, sep="_")) %>% dplyr::select(Year, cat, sex, agegroup, count) %>% group_by(Year,sex,agegroup) %>% 
  summarise(count=sum(count)*100)
target <- cirrhosismortality
# variance <- read.csv("input_data/seed_variance.csv")
# target <- left_join(target,variance) %>% group_by(sex,agegroup)
# target <- as.numeric(cirrhosismortality$count)[1:27]
# rm(cirrhosismortality)
