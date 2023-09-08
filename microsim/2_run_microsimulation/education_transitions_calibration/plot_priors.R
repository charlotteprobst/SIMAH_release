# SIMAH project Sep 2023 
# analysing the run to run variability of education transitions
library(tidyverse)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/education_calibration")

# load in microsim R package
setwd(paste(WorkingDirectory))

data <- read.csv(paste0(DataDirectory, "/prior_ranges_1000.csv")) %>% 
  mutate(AGECAT = cut(microsim.init.age,
                      breaks=c(0,24,34,44,54,64,79),
                      labels=c("18-24","25-34","35-44","45-54",
                               "54-64","65-79")),
         SEX = ifelse(microsim.init.sex=="m", "Men","Women"),
         RACE = recode(microsim.init.race, "BLA"="Black","WHI"="White","SPA"="Hispanic",
                       "OTH"="Others")) %>% 
  rename(EDUC=microsim.init.education, YEAR=year) %>% 
  group_by(YEAR, samplenum, SEX, AGECAT, RACE,
           EDUC) %>% 
  summarise(n=sum(n)) %>% 
  ungroup() %>% 
  group_by(YEAR, samplenum, SEX, AGECAT, RACE) %>% 
  mutate(prop=n/sum(n), YEAR=as.integer(YEAR)) %>% 
  dplyr::select(-n) %>% drop_na()

# read in target data 
targets <- read.csv(paste0(DataDirectory, "/target_data.csv")) %>% 
  dplyr::select(-n) %>% 
  rename(target=prop)

data <- left_join(data,targets) %>% 
  mutate(EDUC = factor(EDUC, levels=c("LEHS","SomeC","College")))

ggplot(data=subset(data, SEX=="Men" & AGECAT=="18-24"), 
       aes(x=as.numeric(YEAR), y=prop, colour=as.factor(samplenum))) + 
  geom_line() + 
  geom_line(aes(x=YEAR,y=target), colour="darkblue",linewidth=2) + 
  facet_grid(cols=vars(RACE), rows=vars(EDUC)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  ggtitle("Men, 18-24 non-inflated prior") + 
  xlab("Year")

ggsave(paste0(DataDirectory, "/plot_prior_men.png"), dpi=300, width=33, height=19, units="cm")

ggplot(data=subset(data, SEX=="Women" & AGECAT=="18-24"), 
       aes(x=as.numeric(YEAR), y=prop, colour=as.factor(samplenum))) + 
  geom_line() + 
  geom_line(aes(x=YEAR,y=target), colour="darkblue",linewidth=2) + 
  facet_grid(cols=vars(RACE), rows=vars(EDUC)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  ggtitle("Women, 18-24 non-inflated prior") + 
  xlab("Year")

ggsave(paste0(DataDirectory, "/plot_prior_women.png"), dpi=300, width=33, height=19, units="cm")

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
  ungroup() %>% 
  mutate(
            ntilemax = ntile(maximplausibility, 500),
            ntilemean = ntile(meanimplausibility, 500)) 

max <- data %>% 
  filter(samplenum %in% subset(maxmean, ntilemax<=3)$samplenum | 
           samplenum %in% subset(maxmean, ntilemean<=3)$samplenum) %>% 
  mutate(type=ifelse(samplenum %in% subset(maxmean, ntilemax<=3)$samplenum, "max", "mean"),
         typesample = paste0(type, "_", samplenum))

ggplot(data=subset(max, SEX=="Women" & AGECAT=="18-24"), 
       aes(x=as.numeric(YEAR), y=prop, colour=as.factor(typesample))) + 
  geom_line(linewidth=1) + 
  geom_line(aes(x=YEAR,y=target), colour="darkblue",linewidth=1) + 
  facet_grid(cols=vars(RACE), rows=vars(EDUC)) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  ggtitle("Women, 18-24 non-inflated prior") + 
  xlab("Year")

ggplot(data=subset(max, SEX=="Men" & AGECAT=="18-24"), 
       aes(x=as.numeric(YEAR), y=prop, colour=as.factor(typesample))) + 
  geom_line(linewidth=1) + 
  geom_line(aes(x=YEAR,y=target), colour="darkblue",linewidth=1) + 
  facet_grid(cols=vars(RACE), rows=vars(EDUC)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  ggtitle("Women, 18-24 non-inflated prior") + 
  xlab("Year")
