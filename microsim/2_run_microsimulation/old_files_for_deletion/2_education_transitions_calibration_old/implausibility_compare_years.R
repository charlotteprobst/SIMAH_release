library(tidyverse)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/education_calibration")

# load in microsim R package
setwd(paste(WorkingDirectory))

data_allyears <- read_csv(paste0(DataDirectory, "/prior_range_inflated_allyears.csv")) %>% 
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

data_year2005 <- read_csv(paste0(DataDirectory, "/prior_range_inflated_allyears-2005.csv")) %>% 
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

data_allyears <- left_join(data_allyears,targets) %>% 
  mutate(EDUC = factor(EDUC, levels=c("LEHS","SomeC","College")))

data_year2005 <- left_join(data_year2005,targets) %>% 
  mutate(EDUC = factor(EDUC, levels=c("LEHS","SomeC","College")))

implausibility_allyears <- data_allyears %>% 
  filter(AGECAT=="18-24") %>% 
  filter(YEAR<=2019) %>% 
  group_by(YEAR, samplenum, SEX,AGECAT, RACE, EDUC) %>% 
  summarise(implausibility = abs(prop-target)/sqrt(SE)) %>% 
  group_by(samplenum) %>% 
  summarise(maximplausibility = max(implausibility),
            meanimplausibility = mean(implausibility)) %>% 
  mutate(percentile=ntile(maximplausibility,200))

implausibility_year2005 <- data_year2005 %>% 
  filter(AGECAT=="18-24") %>% 
  filter(YEAR<=2019) %>% 
  group_by(YEAR, samplenum, SEX,AGECAT, RACE, EDUC) %>% 
  summarise(implausibility = abs(prop-target)/sqrt(SE)) %>% 
  group_by(samplenum) %>% 
  summarise(maximplausibility = max(implausibility),
            meanimplausibility = mean(implausibility)) %>% 
  mutate(percentile=ntile(maximplausibility,200))

topsample <- subset(implausibility_year2005, percentile==1)$samplenum

best <- data_year2005 %>% filter(samplenum %in% topsample) %>% 
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

ggsave(paste0(DataDirectory, "/best_implausibility_year2005.png"), dpi=300, width=33, height=19, units="cm")



