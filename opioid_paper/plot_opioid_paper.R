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
color.vec <- c("#132268", "#447a9e", "#93AEBF")

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
  geom_line(size=1) + facet_grid(cols=vars(sex), rows=vars(type), scales="free", switch="y") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=18, family="serif")) + ylim(0,NA) + 
  xlab("Year") + scale_colour_manual(values=color.vec)
  
  # scale_colour_brewer(palette="Set1")
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

Fig2p1 <- ggplot(data=subset(gendereducationrace, sex=="Men"), 
                 aes(x=year, y=rate, colour=edclass)) + 
  geom_line(size=1) + facet_grid(cols=vars(race), rows=vars(type), scales="free", switch="y") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=18, family="serif")) + ylim(0,NA) + 
  xlab("Year") +  ggtitle("Men") +
  scale_colour_manual(values=color.vec)

  
  # scale_colour_brewer(palette="Set1") + 
Fig2p1
ggsave("SIMAH_workplace/opioid_paper/poisoningdata/Figure2_Men.png",
       Fig2p1, width=33, height=19, units="cm", dpi=300)

Fig2p2 <- ggplot(data=subset(gendereducationrace, sex=="Women"), 
                 aes(x=year, y=rate, colour=edclass)) + 
  geom_line(size=1) + facet_grid(cols=vars(race), rows=vars(type), scales="free", switch="y") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=18, family="serif")) + ylim(0,NA) + 
  xlab("Year") + scale_colour_manual(values=color.vec) + 
  ggtitle("Women")
Fig2p2
ggsave("SIMAH_workplace/opioid_paper/poisoningdata/Figure2_Women.png",
       Fig2p2, width=33, height=19, units="cm", dpi=300)

# combine fig2p1 and fig2p2 
library(gridExtra)
library(ggpubr)

combined <- ggarrange(Fig2p1, Fig2p2, ncol=1, nrow=2, common.legend = TRUE, legend="bottom")
ggsave("SIMAH_workplace/opioid_paper/poisoningdata/Figure2_combined.png",
       combined, width=33, height=40, units="cm", dpi=300)

# alternative method of combining
gendereducationrace$typesex <- paste(gendereducationrace$sex, gendereducationrace$type, 
                                     sep=": ")

Fig2alternative <- ggplot(data=gendereducationrace, 
                 aes(x=year, y=rate, colour=edclass)) + 
  geom_line(size=1) + facet_grid(cols=vars(race), rows=vars(typesex), switch="y", scales="free") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=18, family="serif")) + ylim(0,NA) + 
  xlab("Year") + scale_colour_manual(values=color.vec)
Fig2alternative
ggsave("SIMAH_workplace/opioid_paper/poisoningdata/Figure2_alternative.png",
       Fig2alternative, width=33, height=40, units="cm", dpi=300)


# Supplementary Figure 1 - poisonings by race/ethnicity and NOT education
genderrace <- read_dta("SIMAH_Workplace/opioid_paper/poisoningdata/poison-gender-race-StandardRates-25plus.dta")

genderrace <- genderrace %>% remove_all_labels() %>% 
  zap_formats() %>% 
  dplyr::select(year, race, sex, alc_only_fin_StdRate,
                opioid_only_fin_StdRate, alc_opioid_fin_StdRate) %>% 
  pivot_longer(cols=c(alc_only_fin_StdRate:alc_opioid_fin_StdRate),
               names_to="type", values_to="rate") %>% 
  mutate(type=gsub("_fin_StdRate","",type),
         race = recode(race, "1"="Non-Hispanic White",
                       "2"="Non-Hispanic Black",
                       "3"="Hispanic",
                       "4"="Non-Hispanic Others"),
         race = factor(race, levels=c("Non-Hispanic White",
                                      "Non-Hispanic Black",
                                      "Hispanic",
                                      "Non-Hispanic Others")),
         sex = ifelse(sex==1, "Men","Women"),
         type = recode(type, "alc_only"="Alcohol",
                       "opioid_only"="Opioid",
                       "alc_opioid"="Alcohol and Opioid"),
         type=factor(type, levels=c("Alcohol","Opioid","Alcohol and Opioid")))

color.vec <- c("#132268", "#447a9e", "#93AEBF", "#000000")

Fig1Supp <- ggplot(data=genderrace, aes(x=year, y=rate, colour=race)) + 
  geom_line(size=1) + facet_grid(cols=vars(sex), rows=vars(type), scales="free", switch="y") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=18, family="serif")) + ylim(0,NA) + 
  xlab("Year") + scale_colour_brewer(palette="Set1")
Fig1Supp
ggsave("SIMAH_workplace/opioid_paper/poisoningdata/Figure1_supplementary.png",
       Fig1Supp, width=33, height=40, units="cm", dpi=300)
# adding some comments about the code
