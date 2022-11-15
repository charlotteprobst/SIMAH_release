# SIMAH project October 2021 
# plots for drawing opioid and alcohol mortality rates for publication

library(dplyr)
library(tidyr)
library(foreign)
library(haven)
library(ggplot2)
library(sjlabelled)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)

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
         type=factor(type, levels=c("Alcohol","Alcohol and Opioid","Opioid"))) 
color.vec <- c("#132268", "#447a9e", "#93AEBF")

devtools::install_github("zeehio/facetscales")
library(facetscales)

type <- c("Alcohol","Opioid","Alcohol and Opioid")
sex <- c("Men","Women")
edclass <- c("High school degree or less", "Some college",
             "College degree or more")
facet_bounds <- expand.grid(type,sex,edclass)
facet_bounds$ymin <- 0
names(facet_bounds) <- c("type","sex","edclass","ymin")
facet_bounds$ymax <- ifelse(facet_bounds$type=="Opioid" & facet_bounds$sex=="Men", 60,
                            ifelse(facet_bounds$type=="Alcohol" & facet_bounds$sex=="Men", 15,
                                   ifelse(facet_bounds$type=="Alcohol and Opioid" & facet_bounds$sex=="Men", 15,
                                          ifelse(facet_bounds$type=="Opioid" & facet_bounds$sex=="Women", 35,
                                                 ifelse(facet_bounds$type=="Alcohol" & facet_bounds$sex=="Women", 5, 5)))))

ff <- with(facet_bounds,
           data.frame(rate=c(ymin,ymax),
                      type=c(type,type),
                      sex =c(sex,sex),
                      edclass=c(edclass,edclass)))

Fig1p1 <- ggplot(data=subset(gendereducation, sex=="Men"), aes(x=year, y=rate, colour=edclass)) + 
  geom_line(size=1.5) + facet_grid(cols=vars(sex), rows=vars(type), scales="free", switch="y") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=22, family="Times")) + 
  xlab("Year") + scale_colour_manual(values=color.vec) + 
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2020), limits=c(2000,2020)) + 
  geom_point(data=subset(ff, sex=="Men"),x=NA, colour=NA)
  # scale_colour_brewer(palette="Set1")
Fig1p1

Fig1p2 <- ggplot(data=subset(gendereducation, sex=="Women"), aes(x=year, y=rate, colour=edclass)) + 
  geom_line(size=1.5) + facet_grid(cols=vars(sex), rows=vars(type), scales="free", switch="y") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=22, family="Times")) + 
  xlab("Year") + scale_colour_manual(values=color.vec) + 
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2020), limits=c(2000,2020)) +
  geom_point(data=subset(ff, sex=="Women"),x=NA, colour=NA)
# scale_colour_brewer(palette="Set1")
Fig1p2



combined <- ggarrange(Fig1p1, Fig1p2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
combined
ggsave("SIMAH_workplace/opioid_paper/poisoningdata/Figure1_LisbonAddictions_p2.png",
       combined, width=55, height=30, units="cm", dpi=500)


# read in rates by race data file 
gendereducationrace <- read_dta("SIMAH_Workplace/opioid_paper/poisoningdata/poison-gender-race-education-StandardRates-25plus-00-20.dta")

type <- c("Alcohol","Opioid","Alcohol and Opioid")
sex <- c("Men","Women")
race <- c("Black","White")
edclass <- c("High school degree or less", "Some college",
             "College degree or more")
facet_bounds <- expand.grid(type,sex,race,edclass)
facet_bounds$ymin <- 0
names(facet_bounds) <- c("type","sex","race","edclass","ymin")
facet_bounds$ymax <- ifelse(facet_bounds$type=="Opioid" & facet_bounds$sex=="Men", 80,
                            ifelse(facet_bounds$type=="Alcohol" & facet_bounds$sex =="Men", 15,
                                   ifelse(facet_bounds$type=="Alcohol and Opioid" & facet_bounds$sex == "Men", 25,
                                          ifelse(facet_bounds$type=="Opioid" & facet_bounds$sex == "Women", 50,
                                                 ifelse(facet_bounds$type=="Alcohol" & facet_bounds$sex == "Women", 7, 7)))))

ff <- with(facet_bounds,
           data.frame(rate=c(ymin,ymax),
                      type=c(type,type),
                      sex =c(sex,sex),
                      race = c(race,race),
                      edclass=c(edclass,edclass))) %>%
  mutate(sex2 = ifelse(sex=="Men","men","women"),
         sexrace = paste(race, sex2, sep=" "),
         sexrace = factor(sexrace, levels=c("White men","Black men","White women","Black women")))
  


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
                                      "Non-Hispanic Others")))

genderedrace <- gendereducationrace %>% filter(race != "Non-Hispanic Others" & race!="Hispanic") %>% 
  mutate(race2 = ifelse(race=="Non-Hispanic Black", "Black","White"),
         sex2 = ifelse(sex=="Men","men","women"),
    sexrace = paste(race2, sex2, sep=" "),
         sexrace = factor(sexrace, levels=c("White men","Black men","White women","Black women")))

Fig2p1 <- ggplot(data=subset(genderedrace, sex=="Men"), 
                 aes(x=year, y=rate, colour=edclass)) + 
  geom_line(size=1.5) + facet_grid(cols=vars(sexrace), rows=vars(type), scales="free", switch="y") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=23, family="Times"),
                     plot.margin = margin(0.2,1.1,0.2,0.2, "cm")) + ylim(0,NA) + 
  xlab("Year") +
  scale_colour_manual(values=color.vec)  +
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2020), limits=c(2000,2020)) +
  theme(panel.spacing=unit(1.6,"lines")) + 
  geom_point(data=subset(ff, sex=="Men"),x=NA, colour=NA)

Fig2p1
Fig2p2 <- ggplot(data=subset(genderedrace, sex=="Women"), 
                 aes(x=year, y=rate, colour=edclass)) + 
  geom_line(size=1.5) + facet_grid(cols=vars(sexrace), rows=vars(type), scales="free", switch="y") +
  ylab("Mortality rate per 100,000 population") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=23, family="Times"),
                     plot.margin = margin(0.2,1.1,0.2,0.2, "cm")) + ylim(0,NA) + 
  xlab("Year") +
  scale_colour_manual(values=color.vec) +
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2020), limits=c(2000,2020)) +
  theme(panel.spacing=unit(1.6,"lines")) + 
  geom_point(data=subset(ff, sex=="Women"),x=NA, colour=NA)

Fig2p2
combined <- ggarrange(Fig2p1, Fig2p2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
combined
ggsave("SIMAH_workplace/opioid_paper/poisoningdata/Figure2_LisbonAddictions_p2.png",
       combined, width=55, height=30, units="cm", dpi=500)

# age, period and cohort plots
IRR <- read_dta("SIMAH_workplace/opioid_paper/apcdata/APC-results-by-gender-race.dta") %>% 
  mutate(sex = ifelse(sex==1, "Men","Women"),
         race = as.factor(race),
         race = recode(race, "0"="all","1"="White","2"="Black","3"="Hispanic","4"="Others"),
         apc = recode(apc, "A"="Age","P"="Period","C"="Cohort")) %>% 
  zap_labels() %>% zap_formats() %>% 
  pivot_longer(cols=c(alc_irr:both_hi), values_to="IRR") %>% 
  mutate(substance = ifelse(grepl("opioid", name), "Opioid only",
                            ifelse(grepl("alc",name), "Alcohol only",
                                   "Alcohol and Opioid")),
         type = ifelse(grepl("irr",name),"IRR",
                       ifelse(grepl("hi", name), "upper_CI",
                              ifelse(grepl("lo", name), "lower_CI", "p value")))) %>%
  dplyr::select(category, sex, race, apc, IRR, substance,type) %>% 
  pivot_wider(names_from=type, values_from=IRR) %>% 
  mutate(upper = ifelse(apc=="Age", substr(category, 4,5),
                        ifelse(apc=="Cohort", substr(category,6,9), NA)),
         lower = ifelse(apc=="Age",substr(category,1,2),
                        ifelse(apc=="Cohort", substr(category, 1,4), NA)),
         mid = ifelse(apc=="Age" & lower!=80, (as.numeric(lower)+as.numeric(upper))/2,
                      ifelse(apc=="Age" & category=="80+", 80,
                             ifelse(apc=="Cohort" & category!="<=1930" | category!=">=1996",
                                    (as.numeric(lower)+as.numeric(upper))/2,NA))),
         mid = ifelse(category =="<=1930", 1930,
                      ifelse(category==">=1996", 1996, mid)),
    category = ifelse(apc=="Period", category,
                           ifelse(apc=="Age",mid,
                                  ifelse(apc=="Cohort", mid, NA))),
         # category = ifelse(category=="<=19","1930",
         #                   ifelse(category==">=19","1996",category)),
         category = as.numeric(category),
         substance = factor(substance, levels=c("Alcohol only","Alcohol and Opioid","Opioid only")))
unique(IRR$category)
summary(as.factor(IRR$category))

race <- IRR %>% filter(race!="all") %>% 
  mutate(upper_CI = ifelse(upper_CI>30, NA, upper_CI),
         race = factor(race, levels=c("White","Black","Hispanic")))

ageeffects <- ggplot(subset(race,apc=="Age"), aes(x=category, y=IRR, colour=sex, fill=sex)) + 
  facet_grid(cols=vars(race),
             rows=vars(substance), scales="free") + geom_line(size=1) + 
  theme_bw() + ylim(0,NA) +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(fill="white"),
        text = element_text(size=18)) + 
  ylab("IRR") + xlab("") + 
  # geom_ribbon(aes(ymin=lower_CI, ymax=upper_CI, fill=sex), colour=NA, alpha=0.3) + 
  scale_colour_brewer(palette="Set1") + 
  scale_fill_brewer(palette="Set1") + 
  geom_hline(yintercept=1, linetype="dashed")
ageeffects

ggsave(paste0("SIMAH_workplace/opioid_paper/poisoningdata/Fig3_LisbonAddictions_age.png"), ageeffects, dpi=300, width=33, height=19, units="cm")

periodeffects <- ggplot(subset(race,apc=="Period"), aes(x=category, y=IRR, colour=sex, fill=sex)) + 
  facet_grid(cols=vars(race),
             rows=vars(substance), scales="free") + geom_line(size=1) + 
  theme_bw() + ylim(0,NA) +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(fill="white"),
        text = element_text(size=18)) + 
  ylab("IRR") + xlab("") + 
  # geom_ribbon(aes(ymin=lower_CI, ymax=upper_CI, fill=sex), colour=NA, alpha=0.3) + 
  scale_colour_brewer(palette="Set1") + 
  scale_fill_brewer(palette="Set1") + 
  geom_hline(yintercept=1, linetype="dashed")
periodeffects

ggsave(paste0("SIMAH_workplace/opioid_paper/poisoningdata/Fig3_LisbonAddictions_period.png"), periodeffects, dpi=300, width=33, height=19, units="cm")


cohorteffects <- ggplot(subset(race,apc=="Cohort"), aes(x=category, y=IRR, colour=sex, fill=sex)) + 
  facet_grid(cols=vars(race),
             rows=vars(substance), scales="free") + geom_line(size=1) + 
  theme_bw() + ylim(0,NA) +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(fill="white"),
        text = element_text(size=18)) + 
  ylab("IRR") + xlab("") + 
  # geom_ribbon(aes(ymin=lower_CI, ymax=upper_CI, fill=sex), colour=NA, alpha=0.3) + 
  scale_colour_brewer(palette="Set1") + 
  scale_fill_brewer(palette="Set1") + 
  geom_hline(yintercept=1, linetype="dashed")
cohorteffects

ggsave(paste0("SIMAH_workplace/opioid_paper/poisoningdata/Fig3_LisbonAddictions_cohort.png"), cohorteffects, dpi=300, width=33, height=19, units="cm")




