# SIMAH project October 2021 
# plots for drawing opioid and alcohol mortality rates for publication

library(dplyr)
library(tidyr)
library(foreign)
library(haven)
library(ggplot2)
library(sjlabelled)
library(RColorBrewer)

wd <- "~/Google Drive/SIMAH Sheffield/"
setwd(wd)

# read in datafiles 
gendereducation <- read_dta("SIMAH_Workplace/opioid_paper/poisoningdata/poison-gender-education-StandardRates-25plus.dta")

gendereducation <- gendereducation %>% remove_all_labels() %>% 
  zap_formats() %>% 
  dplyr::select(year, edclass, sex, alc_only_fin_StdRate,
                opioid_only_fin_StdRate, alc_opioid_fin_StdRate) %>% 
  pivot_longer(cols=c(alc_only_fin_StdRate:alc_opioid_fin_StdRate),
               names_to="type", values_to="rate") %>% 
  mutate(type=gsub("_fin_StdRate","",type),
         edclass = recode(edclass, "1"="High school degree or less",
                          "2"="Some college",
                          "3"="College degree or more"),
         edclass = factor(edclass, levels=c("High school degree or less",
                                            "Some college",
                                            "College degree or more")),
         sex = ifelse(sex==1, "Men","Women"),
         type = recode(type, "alc_only"="Alcohol",
                       "opioid_only"="Opioid",
                       "alc_opioid"="Alcohol and Opioid"),
         type=factor(type, levels=c("Alcohol","Opioid","Alcohol and Opioid")))
Fig1 <- ggplot(data=gendereducation, aes(x=year, y=rate, colour=edclass)) + 
  geom_line() + facet_grid(cols=vars(sex), rows=vars(type), scales="free") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=18)) + ylim(0,NA) + 
  xlab("Year") + scale_colour_brewer(palette="Set1")
Fig1
ggsave("SIMAH_workplace/opioid_paper/poisoningdata/Figure1_differentscale.png",
       Fig1, width=33, height=19, units="cm", dpi=300)

# read in rates by race data file 
gendereducationrace <- read_dta("SIMAH_Workplace/opioid_paper/poisoningdata/poison-gender-race-education-StandardRates-25plus.dta")

gendereducationrace <- gendereducationrace %>% remove_all_labels() %>% 
  zap_formats() %>% 
  dplyr::select(year, edclass, sex, race, alc_only_fin_StdRate,
                opioid_only_fin_StdRate, alc_opioid_fin_StdRate) %>% 
  pivot_longer(cols=c(alc_only_fin_StdRate:alc_opioid_fin_StdRate),
               names_to="type", values_to="rate") %>% 
  mutate(type=gsub("_fin_StdRate","",type),
         edclass = recode(edclass, "1"="High school degree or less",
                          "2"="Some college",
                          "3"="College degree or more"),
         edclass = factor(edclass, levels=c("High school degree or less",
                                            "Some college",
                                            "College degree or more")),
         sex = ifelse(sex==1, "Men","Women"),
         type = recode(type, "alc_only"="Alcohol",
                       "opioid_only"="Opioid",
                       "alc_opioid"="Alcohol and Opioid"),
         type=factor(type, levels=c("Alcohol","Opioid","Alcohol and Opioid")),
         race = recode(race, "1"="Non-Hispanic White",
                       "2"="Non-Hispanic Black",
                       "3"="Hispanic",
                       "4"="Non-Hispanic Others"),
         race = factor(race, levels=c("Non-Hispanic White",
                                      "Non-Hispanic Black",
                                      "Hispanic",
                                      "Non-Hispanic Others")))

Fig2p1 <- ggplot(data=subset(gendereducationrace, type=="Alcohol"), 
                 aes(x=year, y=rate, colour=edclass)) + 
  geom_line() + facet_grid(cols=vars(race), rows=vars(sex), scales="free") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=18)) + ylim(0,NA) + 
  xlab("Year") + scale_colour_brewer(palette="Set1") + 
  ggtitle("Alcohol")
Fig2p1

Fig2p2 <- ggplot(data=subset(gendereducationrace, type=="Opioid"), 
                 aes(x=year, y=rate, colour=edclass)) + 
  geom_line() + facet_grid(cols=vars(race), rows=vars(sex), scales="free") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=18)) + ylim(0,NA) + 
  xlab("Year") + scale_colour_brewer(palette="Set1") + 
  ggtitle("Opioid")
Fig2p2

Fig2p3 <- ggplot(data=subset(gendereducationrace, type=="Alcohol and Opioid"), 
                 aes(x=year, y=rate, colour=edclass)) + 
  geom_line() + facet_grid(cols=vars(race), rows=vars(sex), scales="free") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=18)) + ylim(0,NA) + 
  xlab("Year") + scale_colour_brewer(palette="Set1") + 
  ggtitle("Alcohol and Opioid")
Fig2p3


ggsave("SIMAH_workplace/opioid_paper/poisoningdata/Figure2_differentscale.png",
       Fig1, width=33, height=19, units="cm", dpi=300)