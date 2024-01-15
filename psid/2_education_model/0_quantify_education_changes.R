
library(splitstackshape)
library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(ggplot2)
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
                                               ifelse(education>=16, 5, NA))))),
         birthyear = year-age,
         cohort = ifelse(birthyear<1980, "<1980",
                         ifelse(birthyear>=1980 & birthyear<1990, "1980-1989",
                                ifelse(birthyear>=1990 & birthyear<=2000, "1990-1999",">2000"))))

changes <- alldata %>% 
  group_by(uniqueID) %>% 
  mutate(change = educNUM - lag(educNUM)) %>% 
  mutate(changebin = ifelse(change>0, "changed education - up",
                            ifelse(change <0, "changed education - down",
                            "no change"))) %>% 
  ungroup() %>% 
  group_by(age,cohort,changebin) %>% 
  tally() %>% drop_na() %>% 
  ungroup() %>% 
  group_by(age,cohort) %>% 
  mutate(proportion_change = n/sum(n)) %>% 
  # filter(changebin=="changed education") %>% 
  filter(age>=18)  %>% filter(age<=50)


ggplot(data=changes, aes(x=age, y=proportion_change, colour=changebin)) + geom_line(stat="identity") + 
  facet_grid(rows=vars(cohort)) +
  scale_x_continuous("age", labels = as.character(changes$age), breaks = changes$age) + 
  theme_bw() + 
  theme(text = element_text(size=16))
ggsave("SIMAH_workplace/microsim/2_output_data/education_calibration/prop_education_change.png",
       dpi=300, width=33, height=19, units="cm")

# difference between change up and change down 
error <- changes %>% dplyr::select(-n) %>% 
  pivot_wider(names_from=changebin, values_from=proportion_change) %>% 
  mutate(diff = `changed education - up` - `changed education - down`) %>% 
  ungroup() %>% 
  mutate(lagged = diff-lag(diff))

ggplot(data=error, aes(x=age, y=diff)) + geom_line(stat="identity") + 
  # facet_grid(rows=vars(changebin)) +
  scale_x_continuous("age", labels = as.character(changes$age), breaks = changes$age) + 
  theme_bw() + 
  theme(text = element_text(size=16))
