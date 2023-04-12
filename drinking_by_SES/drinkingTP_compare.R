# comparison of drinking patterns by SES group
# SIMAH project 2023 
# C Buckley 

# Read in libraries
library(tidyverse)
library(readxl)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)

Microsimold <- read.csv("SIMAH_workplace/microsim/2_output_data/AlcCats_originalTP.csv") %>% 
  mutate(type="Previous TP")

Microsimnew <- read.csv("SIMAH_workplace/microsim/2_output_data/AlcCats_newTP.csv") %>% 
  mutate(type="New TP (raw)")

Microsimnew_mean <- read.csv("SIMAH_workplace/microsim/2_output_data/AlcCats_newTP-calibratedmean.csv") %>% 
  mutate(type="New TP - calibrated")

Microsimnew_max <- read.csv("SIMAH_workplace/microsim/2_output_data/AlcCats_newTP-calibratedmax.csv") %>% 
  mutate(type="New TP - calibrated (max)")

# combine the data together for GPD 
combined <- rbind(Microsimold, Microsimnew, Microsimnew_mean) %>% 
  mutate(BRFSSdata = ifelse(name=="BRFSS",value,NA)) %>% 
  group_by(year,sex,education, AlcCAT) %>% 
  fill(BRFSSdata, .direction=c("down")) %>% 
  filter(name=="Microsimulation") %>% 
  mutate(education = factor(education,
                            levels=c("LEHS","SomeC","College")),
    AlcCAT = factor(AlcCAT, 
                    levels=c("Non-drinker","<20 / <40 gpd","21-40 / 41-60 gpd", "41+ / 61+ gpd"))) %>% 
  dplyr::select(year, type, sex, education, AlcCAT,lower_ci, upper_ci, BRFSSdata, value) %>% 
  distinct() %>% 
  filter(type!="New TP - calibrated (max)")

# draw a plot - prevalence of category I drinking
ggplot(data=subset(combined, sex=="Women"), aes(x=year, y=value, colour=type)) + 
  geom_line(size=2, aes(x=subset(combined, sex=="Women")$year, y=subset(combined, sex=="Women")$BRFSSdata), colour="black") +
  # geom_ribbon(aes(ymin=subset(combined, sex=="Women")$lower_ci, ymax=subset(combined, sex=="Women")$upper_ci),
  #             colour=NA, fill="grey10", alpha=1) + 
  geom_line(size=1, alpha=0.9) + 
  facet_grid(cols=vars(education), rows=vars(AlcCAT), scales="free") + theme_bw() + 
  theme(legend.position="bottom",legend.title=element_blank(),
        text = element_text(size=18)) + 
  xlim(2000,2019) + ylab("proportion in category") + scale_y_continuous(labels = scales::percent, limits=c(0,NA)) +
  ggtitle("Women") + xlab("")
ggsave("SIMAH_workplace/drinking_by_SES/Women_compareTPs.png", dpi=300, width=33, height=19, units="cm")

# draw a plot - prevalence of category I drinking
ggplot(data=subset(combined, sex=="Men"), aes(x=year, y=value, colour=type)) + 
  geom_line(size=2, aes(x=subset(combined, sex=="Men")$year, y=subset(combined, sex=="Men")$BRFSSdata), colour="black") +
  # geom_ribbon(aes(ymin=subset(combined, sex=="Men")$lower_ci, ymax=subset(combined, sex=="Men")$upper_ci),
  #             colour=NA, fill="grey10", alpha=1) + 
  geom_line(size=1, alpha=0.9) + 
  facet_grid(cols=vars(education), rows=vars(AlcCAT), scales="free") + theme_bw() + 
  theme(legend.position="bottom",legend.title=element_blank(),
        text = element_text(size=18)) +   xlim(2000,2019) + ylab("proportion in category") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,NA)) +
  ggtitle("Men") + xlab("")
ggsave("SIMAH_workplace/drinking_by_SES/Men_compareTPs.png", dpi=300, width=33, height=19, units="cm")



  

