# SIMAH project Sep 2023 
# analysing the run to run variability of education transitions
library(tidyverse)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)
# WorkingDirectory <- "/home/cbuckley/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/education_calibration/new_implausibility_se")

source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/calculate_implausibility_education.R")

targets <- read.csv("SIMAH_workplace/microsim/2_output_data/education_calibration/education_targets.csv") %>% 
  group_by(YEAR, AGECAT, RACE, SEX) %>% 
  mutate(target=TPop/sum(TPop),
         SE=sqrt(target*(1-target)/sum(OrigSample)),
         variance = (SE^2) * OrigSample) %>% 
  dplyr::select(-c(TPop:OrigSample))

# read in output from final wave
output <- read_csv(paste0(DataDirectory, "/validation_output.csv"))

implausibility <- calculate_implausibility_education(output, targets)
implausibility$split <- ifelse(implausibility$implausibility>=3, "reject","keep")

summary(as.factor(implausibility_new_cal$split))
write.csv(implausibility_new_cal, paste0(DataDirectory, "/implausibility_validation.csv"), row.names=F)

summary_output <- left_join(summary_output, implausibility_new)
summary_output <- left_join(summary_output, targets)

best <- summary_output %>% 
  # filter(percentile==1) %>%
  # filter(samplenum==4) %>% 
  group_by(YEAR, SEX, EDUC, RACE, AGECAT) %>% 
  summarise(min = min(propsimulation),
            max = max(propsimulation),
            proptarget = mean(target))

ggplot(data=subset(best, SEX=="Men"), 
       aes(x=as.numeric(YEAR), colour=as.factor(EDUC))) + 
  # geom_line(linewidth=1) + 
  geom_ribbon(aes(ymin=min, ymax=max, colour=as.factor(EDUC), fill=as.factor(EDUC)),
              alpha=0.5) + 
  geom_line(aes(x=YEAR,y=proptarget,colour=as.factor(EDUC)), linewidth=1,
            linetype="dashed") +
  # geom_line(aes(x=YEAR,y=PSID_new), colour="purple",linewidth=1, linetype="dashed") +
  facet_grid(cols=vars(AGECAT), rows=vars(RACE)) + 
  theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position="bottom") +
  ggtitle("Men - posterior distribution") + 
  geom_vline(xintercept=2014, linetype="dotted") + 
  xlab("Year") + ylim(0,1)
ggsave("SIMAH_workplace/microsim/2_output_data/education_calibration/new_implausibility_se/validation_men.png",
       dpi=300, width=33, height=19, units="cm")

# work out maximum distance between target and simulation

distance <- summary_output %>% 
  dplyr::select(YEAR, samplenum, SEX, EDUC, RACE, AGECAT, propsimulation, proptarget, split) %>% 
  filter(split=="keep") %>% 
  mutate(diff = abs(propsimulation-proptarget)) %>% 
  filter(RACE!="Other")
  


best$lower <- best$value - (1.96*best$SE)
best$upper <- best$value + (1.96*best$SE)
best$lower <- ifelse(best$name=="propsimulation",NA, best$lower)
best$upper <- ifelse(best$name=="propsimulation",NA, best$upper)

ggplot(data=subset(best, AGECAT=="65-79"),
       aes(x=as.numeric(YEAR), y=value, colour=as.factor(EDUC), linetype=as.factor(name))) + 
  geom_line(linewidth=1) + 
  # geom_ribbon(aes(ymin=lower, ymax=upper, colour=as.factor(EDUC), fill=as.factor(EDUC))) + 
  # geom_line(aes(x=YEAR,y=target, colour=as.factor(EDUC)), linetype="dashed",linewidth=1) + 
  facet_grid(cols=vars(RACE), rows=vars(SEX)) + 
  scale_linetype_manual(values=c("dotdash","solid"), labels=c("simulation","target")) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title=element_blank()) + 
  xlab("Year") +
  ggtitle("all ages") + 
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) 
ggsave(paste0(DataDirectory, "/best_allages_inflated.png"), dpi=300, width=33, height=19, units="cm")

# plot range for final wave

ggplot(data=subset(summary_output, SEX=="Women" & AGECAT=="25-34"), 
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
