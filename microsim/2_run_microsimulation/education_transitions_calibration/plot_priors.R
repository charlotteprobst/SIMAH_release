# SIMAH project Sep 2023 
# analysing the run to run variability of education transitions
library(tidyverse)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/education_calibration")

# load in microsim R package
setwd(paste(WorkingDirectory))

data <- read.csv(paste0(DataDirectory, "/prior_range_uninflated_neworder.csv")) %>% 
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
  group_by(YEAR,SEX,AGECAT,EDUC,RACE) %>% 
  summarise(n=sum(n)) %>% 
  ungroup() %>% group_by(YEAR,SEX,AGECAT,RACE) %>% 
  mutate(target=n/sum(n))
  
  # 
  # dplyr::select(-n) %>% 
  # rename(target=prop)

data <- left_join(data,targets) %>% 
  mutate(EDUC = factor(EDUC, levels=c("LEHS","SomeC","College")))

ggplot(data=subset(data, SEX=="Men" & AGECAT=="18-24"), 
       aes(x=as.numeric(YEAR), y=prop, colour=as.factor(samplenum))) + 
  geom_line(linewidth=1) + 
  geom_line(aes(x=YEAR,y=target), colour="darkblue",linewidth=1) + 
  facet_grid(cols=vars(RACE), rows=vars(EDUC)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  ggtitle("Men, 18-24 non-inflated prior") + 
  xlab("Year") + ylim(0,1)

ggsave(paste0(DataDirectory, "/plot_prior_men_new.png"), dpi=300, width=33, height=19, units="cm")

ggplot(data=subset(data, SEX=="Women" & AGECAT=="18-24"), 
       aes(x=as.numeric(YEAR), y=prop, colour=as.factor(samplenum))) + 
  geom_line(linewidth=1) + 
  geom_line(aes(x=YEAR,y=target), colour="darkblue",linewidth=1) + 
  facet_grid(cols=vars(RACE), rows=vars(EDUC)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  ggtitle("Women, 18-24 non-inflated prior") + 
  xlab("Year") + ylim(0,1)

ggsave(paste0(DataDirectory, "/plot_prior_women_new.png"), dpi=300, width=33, height=19, units="cm")

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
  ungroup() %>% 
  mutate(
            ntilemax = ntile(maximplausibility, 500),
            ntilemean = ntile(meanimplausibility, 500)) 

max <- data %>% 
  filter(samplenum %in% subset(maxmean, ntilemax<=3)$samplenum | 
           samplenum %in% subset(maxmean, ntilemean<=3)$samplenum) %>% 
  mutate(type=ifelse(samplenum %in% subset(maxmean, ntilemax<=3)$samplenum, "max", "mean"),
         typesample = paste0(type, "_", samplenum))

max <- max %>% 
  filter(samplenum %in% subset(maxmean, ntilemax<=1)$samplenum | 
           samplenum %in% subset(maxmean, ntilemean<=1)$samplenum)

ggplot(data=subset(max, SEX=="Women" & AGECAT=="18-24"), 
       aes(x=as.numeric(YEAR), y=prop, colour=as.factor(EDUC))) + 
  geom_line(linewidth=1) + 
  geom_line(aes(x=YEAR,y=target, colour=as.factor(EDUC)), linetype="dashed",linewidth=1) + 
  facet_grid(cols=vars(typesample), rows=vars(RACE)) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title=element_blank()) + 
  ggtitle("Women, 18-24") + 
  xlab("Year") +
  scale_y_continuous(labels=scales::percent) 

ggsave(paste0(DataDirectory, "/max_mean_women.png"), dpi=300, width=33, height=19, units="cm")


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


