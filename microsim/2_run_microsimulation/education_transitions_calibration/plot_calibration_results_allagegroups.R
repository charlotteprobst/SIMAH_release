# SIMAH project Sep 2023 
# analysing the run to run variability of education transitions
library(tidyverse)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)
# WorkingDirectory <- "/home/cbuckley/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/education_calibration/waves")

targets <- read.csv("SIMAH_workplace/ACS/ACS_popcounts_2000_2021_updated_educsex.csv") %>% 
  rename(YEAR=year, SEX=sex, EDUC=edclass, RACE=race) %>% 
  mutate(SEX= ifelse(SEX==1, "Men","Women")) %>% 
  group_by(YEAR, SEX, EDUC, RACE) %>% 
  summarise(n=sum(TPop)) %>% 
  ungroup() %>% 
  group_by(YEAR, SEX, RACE) %>% 
  mutate(proptarget=n/sum(n),
         SEtarget = sqrt(proptarget*(1-proptarget)/sum(n))) %>% dplyr::select(-n)

# read in output from final wave
output <- read_csv(paste0(DataDirectory, "/output-1.csv"))

# read in implausibility from final wave 
implausibility <- read_csv(paste0(DataDirectory, "/implausibility-9.csv"))

output <- left_join(output, implausibility)

summary_output <- output %>% 
  mutate(SEX = ifelse(microsim.init.sex=="m", "Men","Women"),
        RACE = recode(microsim.init.race, "BLA"="Black","WHI"="White","SPA"="Hispanic",
                      "OTH"="Other")) %>% 
  rename(EDUC=microsim.init.education, YEAR=year) %>% 
  group_by(YEAR, samplenum, seed, SEX,RACE,
           EDUC) %>% 
  summarise(n=sum(n)) %>% 
  ungroup() %>% 
  group_by(YEAR, samplenum, SEX, EDUC,RACE) %>% 
  summarise(n=mean(n)) %>% 
  ungroup() %>% 
  group_by(YEAR, samplenum, SEX,RACE) %>% 
  mutate(propsimulation=n/sum(n), YEAR=as.integer(YEAR)) %>% 
  dplyr::select(-n) %>% drop_na()

summary_output <- left_join(summary_output, targets)

# summary_output <- left_join(summary_output, implausibility)

# recalculate implausibility 
implausibility_new <- summary_output %>% 
  group_by(samplenum, YEAR, SEX, EDUC, RACE) %>% 
  summarise(implausibility = abs(proptarget-propsimulation)/sqrt(SEtarget)) %>% 
  ungroup() %>% 
  group_by(samplenum) %>% 
  summarise(implausibility_new = mean(implausibility)) %>% 
  ungroup() %>% 
  mutate(percentile=ntile(implausibility, 500))

summary_output <- left_join(summary_output, implausibility_new)

best <- summary_output %>% 
  filter(percentile==1) %>% 
  pivot_longer(propsimulation:proptarget)
  
ggplot(data=best, 
       aes(x=as.numeric(YEAR), y=value, colour=as.factor(EDUC), linetype=as.factor(name))) + 
  geom_line(linewidth=1) + 
  # geom_line(aes(x=YEAR,y=target, colour=as.factor(EDUC)), linetype="dashed",linewidth=1) + 
  facet_grid(cols=vars(RACE), rows=vars(SEX)) + 
  scale_linetype_manual(values=c("dotdash","solid"), labels=c("simulation","target")) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title=element_blank()) + 
  xlab("Year") +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) 

# plot range for final wave

ggplot(data=subset(outputsummary, SEX=="Women" & AGECAT=="18-24" & wave==9), 
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

pct_diff <- outputsummary %>% filter(wave==5) %>% 
  filter(percentile==1) %>% 
  mutate(pct_diff = abs(prop-target)/target,
         abs_diff = abs(prop-target))
