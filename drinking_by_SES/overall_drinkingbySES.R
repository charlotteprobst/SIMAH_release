# comparison of drinking patterns by SES group
# SIMAH project 2023 
# C Buckley 

# Read in libraries
library(tidyverse)
library(readxl)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)

# first do GPD over time by SES group and dataset 
NAS <- read.csv("SIMAH_workplace/drinking_by_SES/NAS_mean_GPD.csv") %>% 
  dplyr::select(-X) %>% 
  pivot_longer(men_LEHS:women_College) %>% 
  separate(name, into=c("sex","education")) %>% mutate(data="NAS")
NSDUH <- read.csv("SIMAH_workplace/drinking_by_SES/NSDUH_mean_GPD.csv") %>% 
  dplyr::select(-X) %>% 
  pivot_longer(Men_LEHS:Women_College) %>% 
  separate(name, into=c("sex","education")) %>% 
  mutate(sex = ifelse(sex=="Men","men","women")) %>% rename(year=Year) %>% mutate(data="NSDUH")

BRFSS <- read_excel("SIMAH_workplace/drinking_by_SES/BRFSS_trend_GPD_current drinkers.xlsx") %>% 
  rename(sex = sex_recode, education=education_summary, year=YEAR, value=GPD.mean) %>% 
  mutate(sex = ifelse(sex=="Female","women","men")) %>% 
  dplyr::select(year, sex, education, value) %>% mutate(data="BRFSS")
NHIS <- read.csv("SIMAH_workplace/drinking_by_SES/NHIS_mean_GPD.csv") %>% 
  pivot_longer(Low.SES:High.SES, names_to="education") %>% 
  mutate(education = ifelse(education=="Low.SES","LEHS",
                            ifelse(education=="Middle.SES","SomeC",
                                   ifelse(education=="High.SES","College",NA)))) %>% 
  rename(sex=Sex, year=Year) %>% mutate(data="NHIS")
Microsim <- read.csv("SIMAH_workplace/drinking_by_SES/Microsim_mean_GPD.csv") %>% 
  rename(sex=microsim.init.sex, education=microsim.init.education, value=meangpd) %>% 
  filter(type=="Microsimulation") %>% 
  dplyr::select(year, sex, education, value) %>% mutate(data="Microsim",
                                                        sex = ifelse(sex=="Men","men","women")) %>% 
  group_by(year, sex, education, data) %>% summarise(value=mean(value))

# combine the data together for GPD 
combined <- rbind(BRFSS,NAS,NHIS,NSDUH,Microsim) %>% 
  mutate(education = factor(education,
                            levels=c("LEHS","SomeC","College")),
         data = factor(data, levels=c("BRFSS","NSDUH","NHIS","NAS","Microsim")))

# draw a plot 
ggplot(data=combined, aes(x=year, y=value, colour=education)) + geom_line(size=1) + 
  facet_grid(cols=vars(data), rows=vars(sex)) + theme_bw() + 
  theme(legend.position="bottom",legend.title=element_blank()) + 
  ylim(0,NA) + xlim(2000,2020) + ylab("mean grams per day")
ggsave("SIMAH_workplace/drinking_by_SES/gpd_by_data.png", dpi=300, width=33, height=19, units="cm")

# now do prevalence categories for each data source
NAS <- read.csv("SIMAH_workplace/drinking_by_SES/NAS_mean_alc_cats.csv") %>% dplyr::select(-X) %>% 
  pivot_longer(men_LEHS:women_College) %>% 
  separate(name, into=c("sex","education")) %>% mutate(data="NAS")

NSDUH <- read.csv("SIMAH_workplace/drinking_by_SES/NSDUH_mean_alc_cats.csv") %>% 
  dplyr::select(-X) %>% 
  pivot_longer(Men_LEHS:Women_College) %>% 
  separate(name, into=c("sex","education")) %>% 
  mutate(sex = ifelse(sex=="Men","men","women")) %>% rename(year=Year) %>% mutate(data="NSDUH")

BRFSS <- read_excel("SIMAH_workplace/drinking_by_SES/BRFSS_trend_AlcCAT.xlsx") %>% 
  pivot_longer(LA:HIGH.ub) %>% 
  mutate(alc_cat = ifelse(grepl("LA", name), "Lifetime abstainer",
                          ifelse(grepl("FD", name), "Former drinker",
                                 ifelse(grepl("LOW",name),"Category I",
                          ifelse(grepl("MEDIUM", name), "Category II",
                                 ifelse(grepl("HIGH", name), "Category III", NA))))),
         type = ifelse(grepl("lb",name), "lower",
                       ifelse(grepl("ub",name),"higher","mean"))) %>% 
  drop_na() %>% filter(type=="mean") %>% 
  rename(sex = sex_recode, education=education_summary, year=YEAR) %>% 
  mutate(sex = ifelse(sex=="Female","women","men")) %>% 
  dplyr::select(year, alc_cat, sex, education, value) %>% mutate(data="BRFSS")

NHIS <- read.csv("SIMAH_workplace/drinking_by_SES/NHIS_mean_alc_cats.csv") %>% 
  dplyr::select(-c(X, X.1)) %>% pivot_longer(Low.SES:High.SES, names_to="education") %>% 
  mutate(education = ifelse(education=="Low.SES","LEHS",
                            ifelse(education=="Mid..SES","SomeC",
                                   ifelse(education=="High.SES","College",NA)))) %>% 
  rename(year=Year, alc_cat=Category) %>% mutate(data="NHIS",
                                                 value = value/100)

NESARC <- read.csv("SIMAH_workplace/drinking_by_SES/NESARC_mean_alc_cats.csv") %>% 
  pivot_longer(Low.SES:High.SES, names_to="education") %>% 
  mutate(education = ifelse(education=="Low.SES","LEHS",
                            ifelse(education=="Middle.SES","SomeC",
                                   ifelse(education=="High.SES","College",NA)))) %>% 
  rename(year=Year, alc_cat=Category.) %>% mutate(data="NESARC",
                                                 value = value/100)

Microsim <- read.csv("SIMAH_workplace/drinking_by_SES/Microsim_mean_alc_cats.csv") %>% 
  rename(alc_cat = AlcCAT, value=simulatedpercent) %>% 
  dplyr::select(year, alc_cat, sex, education, value) %>% 
  mutate(alc_cat = ifelse(alc_cat=="Low risk", "Category I",
                          ifelse(alc_cat=="Medium risk","Category II",
                                 ifelse(alc_cat=="High risk","Category III", alc_cat))),
         sex = ifelse(sex=="f","women","men"),
         data="Microsim")

# combine the data together for GPD 
combined <- rbind(BRFSS,NAS,NHIS,NSDUH, NESARC, Microsim) %>% 
  mutate(education = factor(education,
                            levels=c("LEHS","SomeC","College")),
         data = factor(data, levels=c("BRFSS","NSDUH","NHIS","NAS","NESARC","Microsim"))) %>% drop_na()

# draw a plot - prevalence of category I drinking
ggplot(data=subset(combined, alc_cat=="Category I"), aes(x=year, y=value, colour=education)) + geom_line(size=1) + 
  facet_grid(cols=vars(data), rows=vars(sex)) + theme_bw() + 
  theme(legend.position="bottom",legend.title=element_blank()) + 
  xlim(2000,2020) + ylab("prevalence") + scale_y_continuous(labels = scales::percent, limits=c(0,NA))
ggsave("SIMAH_workplace/drinking_by_SES/categoryI_prevalence.png", dpi=300, width=33, height=19, units="cm")

ggplot(data=subset(combined, alc_cat=="Category II"), aes(x=year, y=value, colour=education)) + geom_line(size=1) + 
  facet_grid(cols=vars(data), rows=vars(sex)) + theme_bw() + 
  theme(legend.position="bottom",legend.title=element_blank()) + 
  xlim(2000,2020) + ylab("prevalence") + scale_y_continuous(labels = scales::percent, limits=c(0,NA))
ggsave("SIMAH_workplace/drinking_by_SES/categoryII_prevalence.png", dpi=300, width=33, height=19, units="cm")

ggplot(data=subset(combined, alc_cat=="Category III"), aes(x=year, y=value, colour=education)) + geom_line(size=1) + 
  facet_grid(cols=vars(data), rows=vars(sex)) + theme_bw() + 
  theme(legend.position="bottom",legend.title=element_blank()) + 
  xlim(2000,2020) + ylab("prevalence") + scale_y_continuous(labels = scales::percent, limits=c(0,NA))
ggsave("SIMAH_workplace/drinking_by_SES/categoryIII_prevalence.png", dpi=300, width=33, height=19, units="cm")

ggplot(data=subset(combined, alc_cat=="Lifetime abstainer"), aes(x=year, y=value, colour=education)) + geom_line(size=1) + 
  facet_grid(cols=vars(data), rows=vars(sex)) + theme_bw() + 
  theme(legend.position="bottom",legend.title=element_blank()) + 
  ylim(0,NA) + xlim(2000,2020) + ylab("prevalence")
ggsave("SIMAH_workplace/drinking_by_SES/abstainer_prevalence.png", dpi=300, width=33, height=19, units="cm")

ggplot(data=subset(combined, alc_cat=="Former drinker"), aes(x=year, y=value, colour=education)) + geom_line(size=1) + 
  facet_grid(cols=vars(data), rows=vars(sex)) + theme_bw() + 
  theme(legend.position="bottom",legend.title=element_blank()) + 
  ylim(0,NA) + xlim(2000,2020) + ylab("prevalence")
ggsave("SIMAH_workplace/drinking_by_SES/formerdrinker_prevalence.png", dpi=300, width=33, height=19, units="cm")



  

