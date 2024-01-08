# SIMAH project Sep 2023 
# analysing the run to run variability of education transitions
library(tidyverse)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/education_calibration")

# load in microsim R package
setwd(paste(WorkingDirectory))

data <- read_csv(paste0(DataDirectory, "/prior_range_inflated_year2005-fixed.csv")) %>% 
  mutate(AGECAT = cut(microsim.init.age,
                      breaks=c(0,24,34,44,54,64,79),
                      labels=c("18-24","25-34","35-44","45-54",
                               "55-64","65-79")),
         SEX = ifelse(microsim.init.sex=="m", "Men","Women"),
         RACE = recode(microsim.init.race, "BLA"="Black","WHI"="White","SPA"="Hispanic",
                       "OTH"="Others")) %>% 
  rename(EDUC=microsim.init.education, YEAR=year) %>% 
  group_by(YEAR, samplenum, seed, SEX, AGECAT,RACE,
           EDUC) %>% 
  summarise(n=sum(n)) %>% 
  ungroup() %>% 
  group_by(YEAR, samplenum, SEX, AGECAT, EDUC,RACE) %>% 
  summarise(n=mean(n)) %>% 
  ungroup() %>% 
  group_by(YEAR, samplenum, SEX, AGECAT,RACE) %>% 
  mutate(prop=n/sum(n), YEAR=as.integer(YEAR)) %>% 
  dplyr::select(-n) %>% drop_na()

# read in target data 
targets <- read.csv(paste0(DataDirectory, "/target_data.csv")) %>% 
  dplyr::select(YEAR, SEX, AGECAT, EDUC, RACE, prop, SE) %>% 
  rename(target=prop)
  # group_by(YEAR,SEX,AGECAT,EDUC,RACE) %>% 
  # summarise(n=sum(n)) %>% 
  # ungroup() %>% group_by(YEAR,SEX,AGECAT,RACE) %>% 
  # mutate(target=n/sum(n))

# psid <- read_csv("SIMAH_workplace/education_transitions/new_PSID_weighted_IDs.csv") %>% 
#   mutate(agecat = cut(age, breaks=c(0,24,34),
#                       labels=c("18-24","25-34")),
#          race = recode(race_new_unique, "black"="Black",
#          "white"="White","hispanic"="Hispanic","Native"="Others",
#          "Asian/PI"="Others","other"="Others"),
#          education = ifelse(education<=12, "LEHS",
#                             ifelse(education>=13 & education<=15, "SomeC",
#                                    ifelse(education>=16, "College", NA))),
#          sex = recode(sex, "female"="Women","male"="Men")) %>% 
#   group_by(year, sex, agecat, education, race) %>% 
#     tally() %>% 
#   rename(YEAR=year, SEX=sex, AGECAT=agecat, EDUC=education, RACE=race) %>% 
#   ungroup() %>% 
#   group_by(YEAR, SEX, AGECAT, RACE) %>% 
#   mutate(PSID = n/sum(n)) %>% dplyr::select(-n)
# 
# # PSID new version
newpsid <- read_csv("SIMAH_workplace/PSID/psid_data_1999_2021.csv") %>%
  filter(age<=34) %>%
  mutate(agecat = cut(age, breaks=c(0,24,34),
                      labels=c("18-24","25-34")),
         race = recode(final_race_using_priority_order, "black"="Black",
                       "white"="White","hispanic"="Hispanic","Native"="Others",
                       "Asian/PI"="Others","other"="Others"),
         education = ifelse(education<=12, "LEHS",
                            ifelse(education>=13 & education<=15, "SomeC",
                                   ifelse(education>=16, "College", NA))),
         sex = recode(sex, "female"="Women","male"="Men")) %>%
  group_by(year, sex, agecat, education, race) %>%
  drop_na(race,education) %>%
  summarise(n=sum(`individualweight_cross-sectional`)) %>%
  rename(YEAR=year, SEX=sex, AGECAT=agecat, EDUC=education, RACE=race) %>%
  ungroup() %>%
  group_by(YEAR, SEX, AGECAT, RACE) %>%
  mutate(PSID_new = n/sum(n)) %>% dplyr::select(-n)
# 
targets <- left_join(targets, newpsid)
# targets <- left_join(targets, psid) 
# 
# targets$magnitudeold <- targets$target/targets$PSID
# targets$magnitudenew <- targets$target/targets$PSID_new
# 
# magnitude <- targets %>% dplyr::select(YEAR,SEX, AGECAT, EDUC,RACE,target,
#                                        PSID_new, PSID, magnitudenew, magnitudeold) %>% 
#   drop_na()
# # %>% 
# #   pivot_longer(target:PSID)
# 
# test <- targets %>% filter(AGECAT=="18-24") %>% drop_na()
# 
# ggplot(subset(test, AGECAT=="18-24"), aes(x=YEAR, y=value, colour=as.factor(EDUC), linetype=as.factor(name))) + 
#   geom_line() + 
#   facet_grid(cols=vars(RACE), rows=vars(SEX)) + 
#   ylim(0,1) + 
#   scale_linetype_manual(values=c("dashed","dotted","solid"))
# ggsave(paste0(DataDirectory, "/compare_PSID_ACS_incnew.png"), dpi=300, width=33, height=19, units="cm")
# 
#   
  # 
  # dplyr::select(-n) %>% 
  # rename(target=prop)

data <- left_join(data,targets) %>% 
  mutate(EDUC = factor(EDUC, levels=c("LEHS","SomeC","College")))

ggplot(data=subset(data, SEX=="Men" & AGECAT=="18-24"), 
       aes(x=as.numeric(YEAR), y=prop, colour=as.factor(samplenum))) + 
  geom_line(linewidth=1) + 
  geom_line(aes(x=YEAR,y=target), colour="darkblue",linewidth=1) +
  # geom_line(aes(x=YEAR,y=PSID_new), colour="purple",linewidth=1, linetype="dashed") +
  facet_grid(cols=vars(RACE), rows=vars(EDUC)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  ggtitle("Women, 18-24 model fit 2005-2019") + 
  xlab("Year") + ylim(0,1)

ggsave(paste0(DataDirectory, "/plot_prior_women_allyears2005.png"), dpi=300, width=33, height=19, units="cm")

ggplot(data=subset(data, SEX=="Women" & AGECAT=="18-24"), 
       aes(x=as.numeric(YEAR), y=prop, colour=as.factor(samplenum))) + 
  geom_line(linewidth=1) + 
  geom_line(aes(x=YEAR,y=target), colour="darkblue",linewidth=1) + 
  facet_grid(cols=vars(RACE), rows=vars(EDUC)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  ggtitle("Women, 18-24 non-inflated prior") + 
  xlab("Year") + ylim(0,1)

ggsave(paste0(DataDirectory, "/plot_prior_women_allyears.png"), dpi=300, width=33, height=19, units="cm")

ggplot(data=subset(data, SEX=="Men" & AGECAT=="18-24"), 
       aes(x=as.numeric(YEAR), y=prop, colour=as.factor(samplenum))) + 
  geom_line(linewidth=1) + 
  geom_line(aes(x=YEAR,y=target), colour="darkblue",linewidth=1) + 
  facet_grid(cols=vars(RACE), rows=vars(EDUC)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  ggtitle("Men, 18-24 non-inflated prior") + 
  xlab("Year") + ylim(0,1)

ggsave(paste0(DataDirectory, "/plot_prior_men_newn.png"), dpi=300, width=33, height=19, units="cm")

# calculate implausibility 
implausibility <- data %>% 
  filter(AGECAT=="18-24") %>% 
  filter(YEAR<=2019) %>% 
  group_by(YEAR, samplenum, SEX,AGECAT, RACE, EDUC) %>% 
  summarise(implausibility = abs(prop-target)/sqrt(SE))

maxmean <- implausibility %>% 
  group_by(samplenum) %>%
  summarise(maximplausibility = max(implausibility),
            meanimplausibility = mean(implausibility)) %>% 
  mutate(percentile=ntile(maximplausibility,200))

topsample <- subset(maxmean, percentile==1)$samplenum

best <- data %>% filter(samplenum %in% topsample) %>% 
  pivot_longer(prop:target)

ggplot(data=subset(best, AGECAT=="18-24"), 
       aes(x=as.numeric(YEAR), y=value, colour=as.factor(EDUC), linetype=as.factor(name))) + 
  geom_line(linewidth=1) + 
  # geom_line(aes(x=YEAR,y=target, colour=as.factor(EDUC)), linetype="dashed",linewidth=1) + 
  facet_grid(cols=vars(RACE), rows=vars(SEX)) + 
  scale_linetype_manual(values=c("dotdash","solid"), labels=c("simulation","target")) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title=element_blank()) + 
  ggtitle("age 18-24") + 
  xlab("Year") +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) 

ggsave(paste0(DataDirectory, "/best_implausibility_allyears.png"), dpi=300, width=33, height=19, units="cm")


ggplot(data=subset(max, SEX=="Men" & AGECAT=="18-24"), 
       aes(x=as.numeric(YEAR), y=prop, colour=as.factor(EDUC))) + 
  geom_line(linewidth=1) + 
  geom_line(aes(x=YEAR,y=target, colour=as.factor(EDUC)), linetype="dashed",linewidth=1) + 
  facet_grid(cols=vars(typesample), rows=vars(RACE)) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title=element_blank()) + 
  ggtitle("Men, 18-24") + 
  xlab("Year") +
  scale_y_continuous(labels=scales::percent)

ggsave(paste0(DataDirectory, "/max_mean_men.png"), dpi=300, width=33, height=19, units="cm")


