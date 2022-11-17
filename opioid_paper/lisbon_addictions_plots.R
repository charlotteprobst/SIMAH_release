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
poisonings <- read_dta("SIMAH_Workplace/opioid_paper/poisoningdata/temp2000-20poi-age-aggregate-population.dta")
color.vec <- c("#132268", "#447a9e", "#93AEBF")

agedist <- poisonings %>% filter(year==2020) %>% 
  mutate(age_gp = cut(age,
                      breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,79,100))) %>% 
  
  group_by(age_gp) %>% 
  summarise(tpop=sum(pop)) %>% ungroup() %>% 
  mutate(prop = tpop/sum(tpop))

agesprate <- poisonings %>% 
  mutate(age_gp = cut(age,
                      breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,79,100))) %>% 
  group_by(year, age_gp) %>% 
  summarise(alc_only = sum(alc_only)/sum(pop)*100000,
            op_only = sum(opioid_only)/sum(pop)*100000,
            alc_opioid = sum(alc_opioid)/sum(pop)*100000)

agesprate <- left_join(agesprate, agedist) %>% 
  group_by(year, age_gp) %>% 
  summarise(alc_only = alc_only*prop,
            op_only = op_only*prop,
            alc_opioid = alc_opioid*prop) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  summarise(alc_only = sum(alc_only),
            op_only = sum(op_only),
            alc_opioid=sum(alc_opioid)) %>% pivot_longer(alc_only:alc_opioid) %>% 
  mutate(name = ifelse(name=="alc_only","Alcohol only",
                       ifelse(name=="op_only","Opioid only", "Alcohol and Opioid")),
         name = factor(name, levels=c("Alcohol only", "Opioid only", "Alcohol and Opioid")))

Fig1p1 <- ggplot(data=subset(agesprate), aes(x=year, y=value)) + 
  geom_line(size=1.5) + facet_grid(cols=vars(name), scales="free", switch="y") +
  ylab("Age-standardised mortality per 100,000") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=28, family="Times")) + 
  xlab("Year") + scale_colour_manual(values=color.vec) + 
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2020), limits=c(2000,2020))
  # scale_colour_brewer(palette="Set1")
Fig1p1
  
ggsave("SIMAH_workplace/opioid_paper/poisoningdata/Figure1_LisbonAddictions_V2.png",
       Fig1p1, width=55, height=30, units="cm", dpi=500)  

# plot by educational attainment
agedist <- poisonings %>% filter(year==2020) %>% 
  mutate(age_gp = cut(age,
                      breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,79,100))) %>% 
  
  group_by(age_gp, edclass) %>% 
  summarise(tpop=sum(pop)) %>% ungroup() %>% group_by(edclass) %>% 
  mutate(prop = tpop/sum(tpop))

agesprate <- poisonings %>% 
  mutate(age_gp = cut(age,
                      breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,79,100))) %>% 
  group_by(year, age_gp, edclass) %>% 
  summarise(alc_only = sum(alc_only)/sum(pop)*100000,
            op_only = sum(opioid_only)/sum(pop)*100000,
            alc_opioid = sum(alc_opioid)/sum(pop)*100000)

agesprate <- left_join(agesprate, agedist) %>% 
  group_by(year, age_gp, edclass) %>% 
  summarise(alc_only = alc_only*prop,
            op_only = op_only*prop,
            alc_opioid = alc_opioid*prop) %>% 
  ungroup() %>% 
  group_by(year, edclass) %>% 
  summarise(alc_only = sum(alc_only),
            op_only = sum(op_only),
            alc_opioid=sum(alc_opioid)) %>% pivot_longer(alc_only:alc_opioid) %>% 
  mutate(name = ifelse(name=="alc_only","Alcohol only",
                       ifelse(name=="op_only","Opioid only", "Alcohol and Opioid")),
         name = factor(name, levels=c("Alcohol only", "Opioid only", "Alcohol and Opioid")),
         edclass = ifelse(edclass==1,"HS or less",
                          ifelse(edclass == 3,"College degree +", "SomeC")),
         edclass = factor(edclass, levels=c("HS or less", "College degree +", "SomeC"))) %>% 
  filter(edclass !="SomeC")

Fig1p1 <- ggplot(data=subset(agesprate), aes(x=year, y=value, colour=edclass)) + 
  geom_line(size=1.5) + facet_grid(cols=vars(name), scales="free", switch="y") +
  ylab("Age-standardised mortality per 100,000") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=28, family="Times")) + 
  xlab("Year") + scale_colour_manual(values=color.vec) + 
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2020), limits=c(2000,2020))
# scale_colour_brewer(palette="Set1")
Fig1p1

ggsave("SIMAH_workplace/opioid_paper/poisoningdata/Figure2_LisbonAddictions_V2.png",
       Fig1p1, width=55, height=30, units="cm", dpi=500)  

agedist <- poisonings %>% filter(year==2020) %>% 
  mutate(age_gp = cut(age,
                      breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,79,100))) %>% 
  
  group_by(age_gp, race) %>% 
  summarise(tpop=sum(pop)) %>% ungroup() %>% group_by(race) %>% 
  mutate(prop = tpop/sum(tpop))

agesprate <- poisonings %>% 
  mutate(age_gp = cut(age,
                      breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,79,100))) %>% 
  group_by(year, age_gp, race) %>% 
  summarise(alc_only = sum(alc_only)/sum(pop)*100000,
            op_only = sum(opioid_only)/sum(pop)*100000,
            alc_opioid = sum(alc_opioid)/sum(pop)*100000)

agesprate <- left_join(agesprate, agedist) %>% 
  group_by(year, age_gp, race) %>% 
  summarise(alc_only = alc_only*prop,
            op_only = op_only*prop,
            alc_opioid = alc_opioid*prop) %>% 
  ungroup() %>% 
  group_by(year, race) %>% 
  summarise(alc_only = sum(alc_only),
            op_only = sum(op_only),
            alc_opioid=sum(alc_opioid)) %>% pivot_longer(alc_only:alc_opioid) %>% 
  mutate(name = ifelse(name=="alc_only","Alcohol only",
                       ifelse(name=="op_only","Opioid only", "Alcohol and Opioid")),
         name = factor(name, levels=c("Alcohol only", "Opioid only", "Alcohol and Opioid")),
         race = ifelse(race==1,"NH-White",
                       ifelse(race==2, "NH-Black", "Others"))) %>%  filter(race !="Others")

Fig1p1 <- ggplot(data=subset(agesprate), aes(x=year, y=value, colour=race)) + 
  geom_line(size=1.5) + facet_grid(cols=vars(name), scales="free", switch="y") +
  ylab("Age-standardised mortality per 100,000") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=28, family="Times")) + 
  xlab("Year") + scale_colour_manual(values=color.vec) + 
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2020), limits=c(2000,2020))
# scale_colour_brewer(palette="Set1")
Fig1p1

ggsave("SIMAH_workplace/opioid_paper/poisoningdata/Figure3_LisbonAddictions_V2.png",
       Fig1p1, width=55, height=30, units="cm", dpi=500)  

agedist <- poisonings %>% filter(year==2020) %>% 
  mutate(age_gp = cut(age,
                      breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,79,100))) %>% 
  
  group_by(age_gp, race, edclass) %>% 
  summarise(tpop=sum(pop)) %>% ungroup() %>% group_by(race, edclass) %>% 
  mutate(prop = tpop/sum(tpop))

agesprate <- poisonings %>% 
  mutate(age_gp = cut(age,
                      breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,79,100))) %>% 
  group_by(year, age_gp, race, edclass) %>% 
  summarise(alc_only = sum(alc_only)/sum(pop)*100000,
            op_only = sum(opioid_only)/sum(pop)*100000,
            alc_opioid = sum(alc_opioid)/sum(pop)*100000)

agesprate <- left_join(agesprate, agedist) %>% 
  group_by(year, age_gp, race, edclass) %>% 
  summarise(alc_only = alc_only*prop,
            op_only = op_only*prop,
            alc_opioid = alc_opioid*prop) %>% 
  ungroup() %>% 
  group_by(year, race, edclass) %>% 
  summarise(alc_only = sum(alc_only),
            op_only = sum(op_only),
            alc_opioid=sum(alc_opioid)) %>% pivot_longer(alc_only:alc_opioid) %>% 
  mutate(name = ifelse(name=="alc_only","Alcohol only",
                       ifelse(name=="op_only","Opioid only", "Alcohol and Opioid")),
         name = factor(name, levels=c("Alcohol only", "Opioid only", "Alcohol and Opioid")),
         race = ifelse(race==1,"NH-White",
                       ifelse(race==2, "NH-Black", "Others")),
         edclass = ifelse(edclass==1,"HS or less",
                          ifelse(edclass == 3,"College degree +", "SomeC"))) %>% 
  filter(race !="Others") %>% 
  filter(edclass!="SomeC") %>% mutate(cat = paste(race, edclass))

color.vec <- c("#132268", "#447a9e", "#93AEBF", "black")


Fig1p1 <- ggplot(data=subset(agesprate), aes(x=year, y=value, colour=cat)) + 
  geom_line(size=1.5) + facet_grid(cols=vars(name), scales="free", switch="y") +
  ylab("Age-standardised mortality per 100,000") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=28, family="Times")) + 
  xlab("Year") + scale_colour_manual(values=color.vec) + 
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2020), limits=c(2000,2020))
# scale_colour_brewer(palette="Set1")
Fig1p1

ggsave("SIMAH_workplace/opioid_paper/poisoningdata/Figure4_LisbonAddictions_V2.png",
       Fig1p1, width=55, height=30, units="cm", dpi=500)  

# age, period and cohort plots
IRR <- read_dta("SIMAH_workplace/opioid_paper/apcdata/APC-results-by-gender-race.dta") %>% 
  mutate(sex = ifelse(sex==1, "men","women"),
         race = as.factor(race),
         race = recode(race, "0"="all","1"="NH-White","2"="NH-Black","3"="Hispanic","4"="Others"),
         apc = recode(apc, "A"="Age","P"="Period","C"="Cohort"),
         cat = paste(race, sex)) %>% 
  filter(race!="all") %>% filter(race!="Hispanic") %>% filter(race!="Others") %>% 
  zap_labels() %>% zap_formats() %>% 
  pivot_longer(cols=c(alc_irr:both_hi), values_to="IRR") %>% 
  mutate(substance = ifelse(grepl("opioid", name), "Opioid only",
                            ifelse(grepl("alc",name), "Alcohol only",
                                   "Alcohol and Opioid")),
         type = ifelse(grepl("irr",name),"IRR",
                       ifelse(grepl("hi", name), "upper_CI",
                              ifelse(grepl("lo", name), "lower_CI", "p value")))) %>%
  dplyr::select(category, sex, race, cat, apc, IRR, substance,type) %>% 
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
         substance = factor(substance, levels=c("Alcohol only","Opioid only","Alcohol and Opioid")))
unique(IRR$category)
summary(as.factor(IRR$category))

ageeffects <- ggplot(subset(IRR,apc=="Age"), aes(x=category, y=IRR, colour=cat, fill=cat)) + 
  facet_grid(cols=vars(substance),
             scales="free") + geom_line(size=1) + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=28, family="Times")) + 
  ylab("IRR") + xlab("") + 
  # geom_ribbon(aes(ymin=lower_CI, ymax=upper_CI, fill=sex), colour=NA, alpha=0.3) + 
  scale_colour_brewer(palette="Set1") + 
  scale_fill_brewer(palette="Set1") + 
  geom_hline(yintercept=1, linetype="dashed")
ageeffects

ggsave(paste0("SIMAH_workplace/opioid_paper/poisoningdata/Fig5_LisbonAddictions_age_V2.png"), ageeffects, dpi=300, width=33, height=19, units="cm")

periodeffects <- ggplot(subset(IRR,apc=="Period"), aes(x=category, y=IRR, colour=cat, fill=cat)) + 
  facet_grid(cols=vars(substance), scales="free") + geom_line(size=1) + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=28, family="Times")) + 
  ylab("IRR") + xlab("") + 
  # geom_ribbon(aes(ymin=lower_CI, ymax=upper_CI, fill=sex), colour=NA, alpha=0.3) + 
  scale_colour_brewer(palette="Set1") + 
  scale_fill_brewer(palette="Set1") + 
  geom_hline(yintercept=1, linetype="dashed") + 
  xlim(2000,2020)
periodeffects

ggsave(paste0("SIMAH_workplace/opioid_paper/poisoningdata/Fig6_LisbonAddictions_period_V2.png"), periodeffects, dpi=300, width=33, height=19, units="cm")


cohorteffects <- ggplot(subset(IRR,apc=="Cohort"), aes(x=category, y=IRR, colour=cat, fill=cat)) + 
  facet_grid(cols=vars(substance), scales="free") + geom_line(size=1) + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white"),
                     text = element_text(size=28, family="Times")) + 
  # geom_rect(data=subset(race,apc=="Cohort"), aes(xmin=min_x, xmax=max_x, ymin = min_y, 
  #                                                ymax =max_y, colour=sex),
  #           fill=NA,
  #           alpha = .05)  + 
  ylab("IRR") + xlab("") + 
  # geom_ribbon(aes(ymin=lower_CI, ymax=upper_CI, fill=sex), colour=NA, alpha=0.3) + 
  scale_colour_brewer(palette="Set1") + 
  scale_fill_brewer(palette="Set1") + 
  geom_hline(yintercept=1, linetype="dashed")
cohorteffects

ggsave(paste0("SIMAH_workplace/opioid_paper/poisoningdata/Fig7_LisbonAddictions_cohort.png"), cohorteffects, dpi=300, width=33, height=19, units="cm")




