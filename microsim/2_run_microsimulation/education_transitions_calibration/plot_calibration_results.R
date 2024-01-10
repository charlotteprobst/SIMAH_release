# SIMAH project Sep 2023 
# analysing the run to run variability of education transitions
library(tidyverse)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)
# WorkingDirectory <- "/home/cbuckley/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/education_calibration/waves")

targets <- read.csv("SIMAH_workplace/microsim/2_output_data/education_calibration/target_data.csv") %>% 
  dplyr::select(YEAR, SEX, AGECAT, EDUC, RACE, prop, SE) %>% 
  rename(target=prop)

# load in microsim R package
setwd(paste(WorkingDirectory))
files <- Sys.glob(paste0(DataDirectory, "/implausibility*"))
implausibility <- lapply(files, read_csv)

for(i in 1:length(implausibility)){
  implausibility[[i]] <- data.frame(implausibility[[i]])
  implausibility[[i]]$wave <- i
}

implausibility <- do.call(rbind,implausibility) 

ggplot(implausibility, aes(y=implausibility)) + geom_boxplot() + 
  facet_grid(cols=vars(wave)) + 
  theme_bw() + 
  theme(axis.text.x=element_blank())
getwd()
ggsave("SIMAH_workplace/microsim/2_output_data/education_calibration/implausibility_summary.png",
       dpi=300, width=33, height=19, units="cm")

summary <- implausibility %>% group_by(wave) %>% 
  summarise(mean = mean(implausibility),
            min = min(implausibility),
            max = max(implausibility))

files <- Sys.glob(paste0(DataDirectory, "/output*"))
outputs <- lapply(files, read_csv)

names <- gsub("/Users/charlottebuckley/Google Drive/SIMAH Sheffield/SIMAH_workplace/microsim/2_output_data/education_calibration/waves/output-", "", files)
names <- parse_number(names)

for(i in 1:length(outputs)){
  outputs[[i]] <- data.frame(outputs[[i]])
  outputs[[i]]$wave <- names[i]
  outputs[[i]] <- outputs[[i]] %>% 
    mutate(AGECAT = cut(microsim.init.age,
                        breaks=c(0,24,34,44,54,64,79),
                        labels=c("18-24","25-34","35-44","45-54",
                                 "55-64","65-79")),
           SEX = ifelse(microsim.init.sex=="m", "Men","Women"),
           RACE = recode(microsim.init.race, "BLA"="Black","WHI"="White","SPA"="Hispanic",
                         "OTH"="Others")) %>% 
    rename(EDUC=microsim.init.education, YEAR=year) %>% 
    group_by(YEAR, wave, samplenum, seed, SEX, AGECAT,RACE,
             EDUC) %>% 
    summarise(n=sum(n)) %>% 
    ungroup() %>% 
    group_by(YEAR, wave, samplenum, SEX, AGECAT, EDUC,RACE) %>% 
    summarise(n=mean(n)) %>% 
    ungroup() %>% 
    group_by(YEAR, wave, samplenum, SEX, AGECAT,RACE) %>% 
    mutate(prop=n/sum(n), YEAR=as.integer(YEAR)) %>% 
    dplyr::select(-n) %>% drop_na()
}

outputsummary <- do.call(rbind, outputs)
outputsummary <- left_join(outputsummary, targets)

# join up with implausibility too 
outputsummary <- left_join(outputsummary, implausibility)

# plot the best fitting model from the best wave
best <- outputsummary %>% filter(wave==9) %>% 
  filter(percentile==1) %>% 
  pivot_longer(prop:target) %>% 
  filter(AGECAT=="35-44" | AGECAT=="45-54")

ggplot(data=subset(best, SEX=="Men" & samplenum==4), 
       aes(x=as.numeric(YEAR), y=value, colour=as.factor(EDUC), linetype=as.factor(name))) + 
  geom_line(linewidth=1) + 
  # geom_line(aes(x=YEAR,y=target, colour=as.factor(EDUC)), linetype="dashed",linewidth=1) + 
  facet_grid(cols=vars(RACE), rows=vars(AGECAT)) + 
  scale_linetype_manual(values=c("dotdash","solid"), labels=c("simulation","target")) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title=element_blank()) + 
  # ggtitle("age 18-24") + 
  xlab("Year") +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) 
ggsave("SIMAH_workplace/microsim/2_output_data/education_calibration/best_setting35+.png",
       dpi=300, width=33, height=19, units="cm")
# plot range for final wave

ggplot(data=subset(outputsummary, SEX=="Women" & AGECAT=="18-24" & wave==9), 
       aes(x=as.numeric(YEAR), y=prop, colour=as.factor(samplenum))) + 
  geom_line(linewidth=1) + 
  geom_line(aes(x=YEAR,y=target), colour="darkblue",linewidth=1) +
  # geom_line(aes(x=YEAR,y=PSID_new), colour="purple",linewidth=1, linetype="dashed") +
  facet_grid(cols=vars(RACE), rows=vars(EDUC)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  ggtitle("Women, 18-24") + 
  xlab("Year") + ylim(0,1)
ggsave("SIMAH_workplace/microsim/2_output_data/education_calibration/posterior_women.png",
       dpi=300, width=33, height=19, units="cm")

pct_diff <- outputsummary %>% filter(wave==5) %>% 
  filter(percentile==1) %>% 
  mutate(pct_diff = abs(prop-target)/target,
         abs_diff = abs(prop-target))
