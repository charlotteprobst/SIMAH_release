# SIMAH project Sep 2023 
# analysing the run to run variability of education transitions
library(tidyverse)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)
# WorkingDirectory <- "/home/cbuckley/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/education_calibration/newagecat30")

targets <- read.csv("SIMAH_workplace/microsim/2_output_data/education_calibration/education_targets_indage.csv") %>% 
  mutate(AGECAT = cut(AGE,
                      breaks=c(0,18,19,20,21,22,23,24,34,44,54,64,79),
                      labels=c("18","19","20","21","22","23","24","25-34",
                               "35-44","45-54","55-64","65-79")),
         AGECAT = cut(AGE,
                     breaks=c(0,24,34,44,54,64,79),
                     labels=c("18-24","25-34","35-44","45-54",
                              "55-64","65-79"))
         
         ) %>% 
  mutate_at(vars(YEAR, RACE, SEX, EDUC), as.factor) %>% 
  group_by(YEAR, RACE, SEX, EDUC, .drop=FALSE) %>% 
  summarise(TPop=sum(TPop),
            OrigSample = sum(OrigSample)) %>% 
  group_by(YEAR, RACE, SEX) %>% 
  mutate(proptarget=TPop/sum(TPop),
         SE=sqrt(proptarget*(1-proptarget)/sum(OrigSample)),
         YEAR= as.integer(as.character(YEAR))) %>% 
  mutate_at(vars(RACE, SEX, EDUC), as.character)

# read in output from final wave
output <- read_csv(paste0(DataDirectory, "/validation_output.csv"))

summary_output <- output %>% 
  # mutate(AGECAT = cut(microsim.init.age,
  #                     breaks=c(0,18,19,20,21,22,23,24,34,44,54,64,79),
  #                     labels=c("18","19","20","21","22","23","24","25-34",
  #                              "35-44","45-54","55-64","65-79")),
    mutate(AGECAT = cut(microsim.init.age,
                             breaks=c(0,24,34,44,54,64,79),
                             labels=c("18-24","25-34","35-44","45-54",
                                      "55-64","65-79")),
        SEX = ifelse(microsim.init.sex=="m", "Men","Women"),
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
  mutate(proptarget=ifelse(proptarget==0, 0.001, proptarget),
         SE = ifelse(SE==0, 0.001, SE)) %>% 
  summarise(implausibility = abs(proptarget-propsimulation)/sqrt(SE)) %>% 
  ungroup() %>% 
  group_by(samplenum) %>% 
  summarise(implausibility_new = max(implausibility)) %>% 
  ungroup() %>% 
  mutate(percentile=ntile(implausibility_new, 500))

summary_output <- left_join(summary_output, implausibility_new)

# filter on validation
summary_output <- summary_output %>% filter(implausibility_new<3)

best <- summary_output %>% 
  # filter(percentile==1) %>%
  # filter(samplenum==4) %>% 
  pivot_longer(c(propsimulation,proptarget))

best$lower <- best$value - (1.96*best$SE)
best$upper <- best$value + (1.96*best$SE)
best$lower <- ifelse(best$name=="propsimulation",NA, best$lower)
best$upper <- ifelse(best$name=="propsimulation",NA, best$upper)

best$EDUC <- factor(best$EDUC, levels=c("LEHS","SomeC","College"))

ggplot(data=subset(best, SEX=="Men"),
       aes(x=as.numeric(YEAR), y=value, colour=as.factor(samplenum), linetype=as.factor(name))) + 
  geom_line(linewidth=1) + 
  # geom_ribbon(aes(ymin=lower, ymax=upper, colour=as.factor(EDUC), fill=as.factor(EDUC))) + 
  # geom_line(aes(x=YEAR,y=target, colour=as.factor(EDUC)), linetype="dashed",linewidth=1) + 
  facet_grid(cols=vars(RACE), rows=vars(EDUC)) + 
  scale_linetype_manual(values=c("solid","dotdash"), labels=c("simulation","target")) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  xlab("Year") + ylab("Proportion in education category") + 
  ggtitle("all ages") + 
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) + geom_vline(xintercept=2014, linetype="dotted")
ggsave(paste0(DataDirectory, "/range_final_wave.png"), dpi=300, width=33, height=19, units="cm")

# plot range for final wave

ggplot(data=subset(summary_output, SEX=="Women" & AGECAT=="18-24"), 
       aes(x=as.numeric(YEAR), y=propsimulation, colour=as.factor(samplenum))) + 
  geom_line(linewidth=1) + 
  geom_line(aes(x=YEAR,y=proptarget), colour="darkblue",linewidth=1) +
  # geom_line(aes(x=YEAR,y=PSID_new), colour="purple",linewidth=1, linetype="dashed") +
  facet_grid(cols=vars(RACE), rows=vars(EDUC)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  ggtitle("Women, all ages") + 
  xlab("Year") + ylim(0,1)

ggsave(paste0(DataDirectory, "/plot_prior_allages_men.png"), dpi=300, width=33, height=19, units="cm")

pct_diff <- outputsummary %>% filter(wave==5) %>% 
  filter(percentile==1) %>% 
  mutate(pct_diff = abs(prop-target)/target,
         abs_diff = abs(prop-target))
