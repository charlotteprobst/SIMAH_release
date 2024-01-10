
library(splitstackshape)
library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
setwd("~/Google Drive/SIMAH Sheffield")

alldata <- read_csv("SIMAH_workplace/PSID/psid_data_1999_2021.csv") %>% 
  # filter(age>=18 & age<=34) %>% 
  drop_na(sex, education, age, final_race_using_priority_order, `individualweight_cross-sectional`) %>% 
  dplyr::select(uniqueID, year, `individualweight_cross-sectional`, individualweight_longitudinal, sex, age, education, 
                final_race_using_priority_order, final_race_using_method_hierarchy) %>% 
  distinct() %>%
  mutate(educNUM = ifelse(education<=12, 1,
                          ifelse(education==13, 2,
                                 ifelse(education==14, 3,
                                        ifelse(education==15, 4, 
                                               ifelse(education>=16, 5, NA))))))


changes <- alldata %>% 
  group_by(uniqueID) %>% 
  mutate(change = educNUM - lag(educNUM)) %>% 
  mutate(changebin = ifelse(change>0, "changed education",
                            "no change")) %>% 
  ungroup() %>% 
  group_by(age,changebin) %>% 
  tally() %>% drop_na() %>% 
  ungroup() %>% 
  group_by(age) %>% 
  mutate(proportion_change = n/sum(n)) %>% 
  filter(changebin=="changed education") %>% 
  filter(age<=50) %>% filter(age>=18)



ggplot(data=changes, aes(x=age, y=proportion_change)) + geom_line(stat="identity") + 
  facet_grid(rows=vars(changebin)) +
  scale_x_continuous("age", labels = as.character(changes$age), breaks = changes$age) + 
  theme_bw() + 
  theme(text = element_text(size=16))
ggsave("SIMAH_workplace/microsim/2_output_data/education_calibration/prop_education_change.png",
       dpi=300, width=33, height=19, units="cm")
