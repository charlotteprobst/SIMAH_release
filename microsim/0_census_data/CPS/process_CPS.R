# process CPS data 2000 to 2021 - for comparison with ACS population counts 

library(dplyr)
library(tidyr)
library(ipumsr)
library(haven)
library(labelled)

setwd("~/Google Drive/SIMAH Sheffield/")

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("SIMAH_workplace/CPS/cps_00005.xml")
data <- read_ipums_micro(ddi)
data <- remove_labels(data)

data <- data %>% 
  mutate(edclass = ifelse(EDUC<=73, "LEHS",
                       ifelse(EDUC==999, NA,
                       ifelse(EDUC>=110, "College", "SomeC"))),
         race = ifelse(RACE=="100","White",
                       ifelse(RACE==200, "Black",
                              ifelse(RACE==801 | RACE==805 | RACE==806 | RACE==807 | RACE==810 | RACE==811 |
                                       RACE==814 | RACE==816 | RACE==818, "Black",
                              "Other"))),
         race = ifelse(HISPAN==0, race, 
                       ifelse(HISPAN>=901, race, "Hispanic")),
         weight = ifelse(is.na(WTFINL), ASECWT, WTFINL),
         age_gp = cut(AGE, 
                      breaks=c(0,17,24,29,34,39,44,49,54,59,64,69,74,79,100),
                      labels=c("0-17","18","25","30","35","40","45",
                               "50","55","60","65","70","75","80")),
         age_gp=as.character(age_gp),
         flag = ifelse(HFLAG==0, 0, 1),
         flag = ifelse(is.na(flag), 1, flag)) %>% filter(flag==1)
summary(as.factor(data$edclass))
summary(as.factor(data$race))

summary <- data %>% filter(AGE>=18) %>% 
  group_by(YEAR, age_gp, SEX, race, edclass) %>% 
  summarise(TPop = round(sum(weight),digits=0)) %>% 
  rename(year=YEAR, sex=SEX) %>% 
  mutate(age_gp=as.integer(age_gp)) %>% data.frame() %>% 
  mutate(type="CB_CPS") %>% filter(year<=2020) %>% filter(year>=2000)

final_series <- summary %>% 
  dplyr::select(-type)
  
write.csv(final_series, "SIMAH_workplace/CPS/CPS_2000_2020_agegp.csv", row.names=F)

# compare with Yu's data 
CPS <- read.csv("SIMAH_workplace/CPS/allethn_rates_0018_final.csv") %>% 
  dplyr::select(year, race, edclass, sex, age_gp, TPop) %>% 
  mutate(type="CP_CPS") 

# CPS_dta <- read_dta("SIMAH_workplace/CPS/cepr_march_0018_final.dta")

compare <- rbind(summary, CPS)

ggplot(data=subset(compare, age_gp==18), aes(x=year, y=TPop, colour=type)) + geom_line() + 
  facet_wrap(~sex + race + edclass, scales="free") + theme_bw() + 
  ggtitle("population totals for individuals aged 18-24")
ggsave("SIMAH_workplace/CPS/CPS_compare_totals.png", dpi=300, width=33, height=19, units='cm')

compare <- rbind(summary, CPS) %>% pivot_wider(names_from=type, values_from=TPop) %>% 
  mutate(pct_difference = (abs(CP_CPS-CB_CPS) / (CP_CPS+CB_CPS)/2)*100)

# for YU 
write.csv(summary, "SIMAH_workplace/CPS/CPS_2015_2020_agegp.csv", row.names=F)


summary <- data %>% filter(AGE>=18) %>% 
  group_by(YEAR, AGE, SEX, race, edclass) %>% 
  summarise(TPop = round(sum(weight),digits=0)) %>% 
  rename(year=YEAR, sex=SEX, age=AGE) %>% 
  mutate(age=as.integer(age)) %>% data.frame() %>% 
  filter(year<=2020) %>% filter(year>=2015)

write.csv(summary, "SIMAH_workplace/CPS/CPS_2015_2020_indage.csv", row.names=F)
