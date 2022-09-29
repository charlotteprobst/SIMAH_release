# SIMAH project October 2021 
# plots for drawing opioid and alcohol mortality rates for publication

library(dplyr)
library(tidyr)
library(foreign)
library(haven)
library(ggplot2)
library(sjlabelled)
library(RColorBrewer)

# github demonstration

wd <- "~/Google Drive/SIMAH Sheffield/"
setwd(wd)

# read in datafiles 
gendereducation <- read_dta("SIMAH_Workplace/opioid_paper/poisoningdata/poison-gender-eduation-StandardRates-25plus-00-20.dta")
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
         type=factor(type, levels=c("Alcohol","Alcohol and Opioid","Opioid"))) %>% 
  filter(year<=2019)
devtools::install_github("zeehio/facetscales")
library(facetscales)

type <- c("Alcohol","Opioid","Alcohol and Opioid")
sex <- c("Men","Women")
edclass <- c("High school degree or less", "Some college",
             "College degree or more")
facet_bounds <- expand.grid(type,sex,edclass)
facet_bounds$ymin <- 0
facet_bounds$ymax <- ifelse(facet_bounds$Var1=="Opioid", 40, 9)
names(facet_bounds) <- c("type","sex","edclass","ymin","ymax")

ff <- with(facet_bounds,
           data.frame(rate=c(ymin,ymax),
                      type=c(type,type),
                      sex =c(sex,sex),
                      edclass=c(edclass,edclass)))

Fig1 <- ggplot(data=gendereducation, aes(x=year, y=rate, colour=edclass)) + 
  geom_line(size=1.5) + facet_grid(cols=vars(sex), rows=vars(type), scales="free", switch="y") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=22, family="Times")) + 
  xlab("Year") + scale_colour_manual(values=color.vec) + 
  geom_point(data=ff,x=NA, colour=NA) + 
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2019))
  # scale_colour_brewer(palette="Set1")
Fig1
ggsave("SIMAH_workplace/opioid_paper/poisoningdata/Figure1_2019.eps",
       Fig1, width=14.4, height=7.89, dpi=1200, device="eps", family="Times")

# read in rates by race data file 
gendereducationrace <- read_dta("SIMAH_Workplace/opioid_paper/poisoningdata/poison-gender-race-education-StandardRates-25plus-00-20.dta")

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
         type=factor(type, levels=c("Alcohol","Alcohol and Opioid","Opioid")),
         race = recode(race, "1"="Non-Hispanic White",
                       "2"="Non-Hispanic Black",
                       "3"="Hispanic",
                       "4"="Non-Hispanic Others"),
         race = factor(race, levels=c("Non-Hispanic White",
                                      "Non-Hispanic Black",
                                      "Hispanic",
                                      "Non-Hispanic Others"))) %>% 
  filter(year<=2019)

type <- c("Alcohol","Opioid","Alcohol and Opioid")
sex <- c("Men","Women")
edclass <- c("High school degree or less", "Some college",
             "College degree or more")
race <- c("Non-Hispanic White","Non-Hispanic Black","Hispanic",
          "Non-Hispanic Others")
facet_bounds <- expand.grid(type,sex,edclass,race)
facet_bounds$ymin <- 0
facet_bounds$ymax <- ifelse(facet_bounds$Var1=="Opioid" & facet_bounds$Var2=="Men", 60,
                            ifelse(facet_bounds$Var1!="Opioid" & facet_bounds$Var2=="Men",13,
                                   ifelse(facet_bounds$Var1=="Opioid" & facet_bounds$Var2=="Women",40,
                                          ifelse(facet_bounds$Var1!="Opioid" & facet_bounds$Var2=="Women",4,NA))))
names(facet_bounds) <- c("type","sex","edclass","race","ymin","ymax")

ff <- with(facet_bounds,
           data.frame(rate=c(ymin,ymax),
                      type=c(type,type),
                      sex =c(sex,sex),
                      edclass=c(edclass,edclass),
                      race=c(race,race)))

Fig2p1 <- ggplot(data=subset(gendereducationrace, sex=="Men"), 
                 aes(x=year, y=rate, colour=edclass)) + 
  geom_line(size=1.5) + facet_grid(cols=vars(race), rows=vars(type), scales="free", switch="y") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=23, family="Times"),
                     plot.margin = margin(0.2,1.1,0.2,0.2, "cm")) + ylim(0,NA) + 
  xlab("Year") +  ggtitle("Men") +
  scale_colour_manual(values=color.vec) + geom_point(data=ff,x=NA, colour=NA) +
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2019)) +
  theme(panel.spacing=unit(1.6,"lines"))


Fig2p1
ggsave("SIMAH_workplace/opioid_paper/poisoningdata/Figure2_Men.png",
       Fig2p1, width=33, height=19, units="cm", dpi=300)

ff <- ff %>% filter(sex=="Women")

Fig2p2 <- ggplot(data=subset(gendereducationrace, sex=="Women"), 
                 aes(x=year, y=rate, colour=edclass)) + 
  geom_line(size=1.5) + facet_grid(cols=vars(race), rows=vars(type), scales="free", switch="y") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=23, family="Times"),
                     plot.margin = margin(0.2,1.1,0.2,0.2, "cm")) + ylim(0,NA) + 
  xlab("Year") + scale_colour_manual(values=color.vec) +
  ggtitle("Women") + geom_point(data=ff,x=NA, colour=NA) +
  scale_x_continuous(breaks=c(2000,2005, 2010,2015,2019)) + 
  theme(panel.spacing=unit(1.6,"lines"))
Fig2p2
ggsave("SIMAH_workplace/opioid_paper/poisoningdata/Figure2_Women.png",
       Fig2p2, width=33, height=19, units="cm", dpi=300)

# combine fig2p1 and fig2p2 

library(gridExtra)
library(ggpubr)

combined <- ggarrange(Fig2p1, Fig2p2, ncol=1, nrow=2, common.legend = TRUE, legend="bottom")
combined
ggsave("SIMAH_workplace/opioid_paper/poisoningdata/Figure2_2019.eps",
       combined, width=35, height=45, units="cm", dpi=1200, device="eps", family="Times")

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
genderrace <- read_dta("SIMAH_Workplace/opioid_paper/poisoningdata/poison-gender-race-StandardRates-25plus-00-20.dta")

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
         type=factor(type, levels=c("Alcohol","Opioid","Alcohol and Opioid"))) %>% 
  filter(year<=2019)

color.vec <- c("#132268", "#447a9e", "#93AEBF", "#000000")

Fig1Supp <- ggplot(data=genderrace, aes(x=year, y=rate, colour=race)) + 
  geom_line(size=1) + facet_grid(cols=vars(sex), rows=vars(type), scales="free", switch="y") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=18, family="serif")) + ylim(0,NA) + 
  xlab("Year") + scale_colour_brewer(palette="Set1") + 
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2019))

Fig1Supp
ggsave("SIMAH_workplace/opioid_paper/poisoningdata/Figure1_supplementary2019.png",
       Fig1Supp, width=33, height=40, units="cm", dpi=300)


# supplementary Figure 2. 
# education ratios by race and ethnicity 
# read in rates by race data file 
gendereducationrace <- read_dta("SIMAH_Workplace/opioid_paper/poisoningdata/poison-gender-race-education-StandardRates-25plus-00-20.dta")

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
         type=factor(type, levels=c("Alcohol","Alcohol and Opioid","Opioid")),
         race = recode(race, "1"="Non-Hispanic White",
                       "2"="Non-Hispanic Black",
                       "3"="Hispanic",
                       "4"="Non-Hispanic Others"),
         race = factor(race, levels=c("Non-Hispanic White",
                                      "Non-Hispanic Black",
                                      "Hispanic",
                                      "Non-Hispanic Others"))) %>% 
  pivot_wider(names_from=edclass, values_from=rate) %>% 
  mutate(HighLow = `High school degree or less`/`College degree or more`,
         HighMed = `Some college`/`College degree or more`) %>% 
  pivot_longer(cols=c(HighLow:HighMed)) %>% 
  mutate(name = recode(name, "HighLow"="Low Ed to High",
                       "HighMed"="Med Ed to High")) %>% filter(year<=2019)

color.vec <- c("#132268", "#447a9e", "#93AEBF", "#000000")

SuppFig2P1 <- ggplot(data=subset(gendereducationrace, sex=="Men" & race!="Non-Hispanic Others"), 
                 aes(x=year, y=value, colour=race)) + 
  geom_line(size=1) + facet_grid(cols=vars(type), rows=vars(name), scales="free", switch="y") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=18, family="serif")) + ylim(0,NA) + 
  xlab("Year") +  ggtitle("Men") +
  scale_colour_manual(values=color.vec) +
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2019))
SuppFig2P1

SuppFig2P2 <- ggplot(data=subset(gendereducationrace, sex=="Women" & race!="Non-Hispanic Others"), 
                     aes(x=year, y=value, colour=race)) + 
  geom_line(size=1) + facet_grid(cols=vars(type), rows=vars(name), scales="free", switch="y") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=18, family="serif")) + ylim(0,NA) + 
  xlab("Year") +  ggtitle("Women") +
  scale_colour_manual(values=color.vec) +
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2019))

SuppFig2P2

combined <- ggarrange(SuppFig2P1, SuppFig2P2, ncol=1, nrow=2, common.legend = TRUE, legend="bottom")
combined
ggsave("SIMAH_workplace/opioid_paper/poisoningdata/SuppFigure2_combinedV2.png",
       combined, width=33, height=40, units="cm", dpi=300)


