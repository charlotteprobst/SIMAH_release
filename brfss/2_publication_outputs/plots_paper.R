# SIMAH October 2021 - code to generate plots for publication
library(foreign)
library(SASxport)
library(readr)
library(dplyr)
library(tidyr)
library(labelled)
library(sjlabelled)
library(tidyverse)

# CB laptop directory
wd <- "~/Google Drive/SIMAH Sheffield/"
# CB desktop directory
# wd <- "G:/My Drive/SIMAH Sheffield/"
setwd(wd)

####read in the joined up data files 
data <- readRDS("SIMAH_workplace/brfss/processed_data/BRFSS_for_paper_analysis_reweighted.RDS")

summary <- data %>% 
  # filter(State=="USA") %>% 
  group_by(State, YEAR, drinkingstatus_detailed) %>%
  tally() %>% ungroup() %>%
  group_by(State, YEAR) %>%
  mutate(percent = n/sum(n)) %>% 
  mutate(drinkingstatus_detailed=recode(drinkingstatus_detailed,
                                        "Former drinker"="Former drinker",
                                        "Lifetime abstainer"="Lifetime abstainer",
                                        "Yearly drinker"="Non-30 day drinker (annual)",
                                        "Monthly drinker"="30 day drinker"),
    drinkingstatus_detailed = factor(drinkingstatus_detailed,
                                          levels=c("Lifetime abstainer",
                                                   "Former drinker",
                                                   "Non-30 day drinker (annual)",
                                                   "30 day drinker"))) %>% 
  filter(YEAR<=2019)

Figure1 <- ggplot(data=subset(summary, State=="USA"), aes(x=YEAR, y=percent, fill=drinkingstatus_detailed)) +
  geom_bar(stat="identity",position=position_fill(reverse=T)) +
  xlab("Year") + ylab("Proportion") +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text = element_text(family="serif",size=18)) +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=scales::percent, expand=c(0,0),limits=c(0,1.01)) + 
  scale_x_continuous(breaks=c(1984, 1986, 1988, 1990, 1992, 1994, 1996, 
                              1998, 2000, 2002, 2004, 2006, 2008, 2010, 
                              2012, 2014, 2016, 2018),
                     expand=c(0,0))
Figure1
ggsave("SIMAH_workplace/brfss/paper/Figure1_reweighted.png", dpi=500, width=33, height=19, units="cm")

summary2 <- summary %>% filter(State=="Colorado" 
                              | State=="New York" |
                                State=="Minnesota" |
                                State=="Tennessee" |
                                State=="Texas")
Figure2 <- ggplot(data=summary2, aes(x=YEAR, y=percent, fill=drinkingstatus_detailed)) +
  geom_bar(stat="identity",position=position_fill(reverse=T)) +
  xlab("Year") + ylab("Proportion") +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text = element_text(family="serif",size=18),
        strip.background=element_rect(fill="white")) +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=scales::percent, expand=c(0,0),limits=c(0,1.01)) + 
  scale_x_continuous(breaks=c(1984, 1986, 1988, 1990, 1992, 1994, 1996, 
                              1998, 2000, 2002, 2004, 2006, 2008, 2010, 
                              2012, 2014, 2016, 2018),
                     expand=c(0,0)) +
  facet_grid(rows=vars(State))
Figure2
ggsave("SIMAH_workplace/brfss/paper/FigureA1_STATES.png", dpi=500, width=33, height=19, units="cm")

TableA1 <- data %>% group_by(State,YEAR, sex_recode) %>% 
  filter(YEAR<=2019) %>% 
  mutate(sex_recode=ifelse(sex_recode=="Female","women","men")) %>% 
  summarise(raw_drinking_prevalence = round(mean(drinkingstatus),digits=3),
            adjusted_drinking_prevalence=round(mean(drinkingstatus_updated),digits=3)) %>% 
  pivot_wider(names_from=sex_recode, values_from=c(raw_drinking_prevalence:adjusted_drinking_prevalence)) %>% 
  rename(Year = YEAR) %>% 
  dplyr::select(State, Year, raw_drinking_prevalence_women, adjusted_drinking_prevalence_women,
                raw_drinking_prevalence_men, adjusted_drinking_prevalence_men)
write.csv(TableA1, "SIMAH_workplace/brfss/paper/TableA2.csv", row.names=F)

TableA1 <- TableA1 %>% filter(State=="USA") %>% ungroup() %>% dplyr::select(-State)
write.csv(TableA1, "SIMAH_workplace/brfss/paper/TableA1.csv", row.names=F)

trendovertime <- data %>% filter(State=="USA") %>% group_by(YEAR) %>% 
  filter(gramsperday!=0) %>% 
  summarise(raw_gpd = mean(gramsperday),
            raw_apc = mean(gramspercapita),
            new_apc = mean(gramspercapita_90),
            new_gpd = mean(gramsperday_upshifted_crquotient)) %>% 
  filter(YEAR<=2019) %>% pivot_longer(cols=raw_gpd:new_gpd) %>% 
  mutate(name = recode(name, "raw_gpd"="BRFSS",
                       "raw_apc"="APC", "new_apc"="adjusted APC",
                       "new_gpd"="adjusted BRFSS"),
         name = factor(name, levels=c("APC","adjusted APC", "BRFSS","adjusted BRFSS")))

ggplot(data=trendovertime, aes(x=YEAR, y=value, colour=name)) + geom_line(size=1.5) +
  ylim(0,NA) +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text = element_text(family="serif",size=18)) +
  scale_fill_brewer(palette="Set1") +
  scale_x_continuous(breaks=c(1984, 1986, 1988, 1990, 1992, 1994, 1996, 
                              1998, 2000, 2002, 2004, 2006, 2008, 2010, 
                              2012, 2014, 2016, 2018),
                     expand=c(0,0)) +
  xlab("") + ylab("Grams alcohol per day")
  
ggsave("SIMAH_workplace/brfss/paper/Figure2.png", dpi=500, width=33, height=19, units="cm")

Table2 <- trendovertime %>% 
  mutate(value=round(value,digits=1)) %>% 
  pivot_wider(names_from=name, values_from=value) %>% 
  rename(raw_brfss = `BRFSS`, 
         adjusted_brfss = `adjusted BRFSS`,
         raw_APC = APC, 
         adjusted_APC = `adjusted APC`,
         Year = YEAR) %>% 
  dplyr::select(Year, raw_brfss, adjusted_brfss, raw_APC, adjusted_APC) %>% 
  mutate(coverage_baseline = round(raw_brfss/raw_APC, digits=3),
         coverage_adjusted = round(adjusted_brfss / adjusted_APC,digits=3))

write.csv(Table2, "SIMAH_workplace/brfss/paper/TableA4.csv", row.names=F)

mean(Table2$coverage_baseline)
sd(Table2$coverage_baseline)
mean(Table2$coverage_adjusted)
sd(Table2$coverage_adjusted)

trendovertime <- data %>% filter(State=="Colorado" | State=="New York" |
                                   State=="Texas" | State=="Minnesota" |
                                   State=="Tennessee") %>% group_by(State,YEAR) %>% 
  filter(gramsperday!=0) %>% 
  summarise(raw_gpd = mean(gramsperday),
            raw_apc = mean(gramspercapita),
            new_apc = mean(gramspercapita_90),
            new_gpd = mean(gramsperday_upshifted_crquotient)) %>% 
  filter(YEAR<=2019) %>% pivot_longer(cols=raw_gpd:new_gpd) %>% 
  mutate(name = recode(name, "raw_gpd"="BRFSS",
                       "raw_apc"="APC", "new_apc"="adjusted APC",
                       "new_gpd"="adjusted BRFSS"),
         name = factor(name, levels=c("APC","adjusted APC", "BRFSS","adjusted BRFSS")))

ggplot(data=trendovertime, aes(x=YEAR, y=value, colour=name)) + geom_line(size=1.5) +
  ylim(0,NA) +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text = element_text(family="serif",size=18),
        strip.background=element_rect(fill="white")) +
  scale_fill_brewer(palette="Set1") +
  scale_x_continuous(breaks=c(1984, 1986, 1988, 1990, 1992, 1994, 1996, 
                              1998, 2000, 2002, 2004, 2006, 2008, 2010, 
                              2012, 2014, 2016, 2018),
                     expand=c(0,0)) +
  xlab("") + ylab("Grams alcohol per day") +
  facet_grid(cols=vars(State))

ggsave("SIMAH_workplace/brfss/paper/FigureA2_states.png", dpi=500, width=33, height=19, units="cm")

trendovertime <- data %>% group_by(State,YEAR) %>% 
  filter(gramsperday!=0) %>% 
  summarise(raw_gpd = mean(gramsperday),
            raw_apc = mean(gramspercapita),
            new_apc = mean(gramspercapita_90),
            new_gpd = mean(gramsperday_upshifted_crquotient)) %>% 
  filter(YEAR<=2019) %>% pivot_longer(cols=raw_gpd:new_gpd) %>% 
  mutate(name = recode(name, "raw_gpd"="BRFSS",
                       "raw_apc"="APC", "new_apc"="adjusted APC",
                       "new_gpd"="adjusted BRFSS"),
         name = factor(name, levels=c("APC","adjusted APC", "BRFSS","adjusted BRFSS")))

Table3 <- trendovertime %>% 
  mutate(value=round(value,digits=1)) %>% 
  pivot_wider(names_from=name, values_from=value) %>% 
  rename(raw_brfss = `BRFSS`, 
         adjusted_brfss = `adjusted BRFSS`,
         raw_APC = APC, 
         adjusted_APC = `adjusted APC`,
         Year = YEAR) %>% 
  dplyr::select(State, Year, raw_brfss, adjusted_brfss, raw_APC, adjusted_APC) %>% 
  mutate(coverage_baseline = round(raw_brfss/raw_APC, digits=3),
         coverage_adjusted = round(adjusted_brfss / adjusted_APC,digits=3))

means <- Table3 %>% group_by(State) %>% 
  summarise(coverage_baseline_mean = round(mean(coverage_baseline),digits=3),
            coverage_baseline_sd = round(sd(coverage_baseline),digits=3),
            coverage_adjusted_mean = round(mean(coverage_adjusted),digits=3),
            coverage_adjusted_sd = round(sd(coverage_adjusted),digits=3))
write.csv(means, "SIMAH_workplace/brfss/paper/TableA5.csv", row.names=F)

example <- data %>% filter(YEAR==1984) %>% filter(State=="USA") %>% 
  dplyr::select(sex_recode, race_eth, age_var, drinkingstatus, drinkingstatus_detailed, alc_frequency, gramsperday, frequency_upshifted, 
                gramsperday_upshifted_crquotient, quantity_per_occasion, quantity_per_occasion_upshifted) %>% sample_n(10)

USquintiles <- data %>% filter(State=="USA")  %>% filter(alc_frequency!=0) %>% 
  mutate(agecat = cut(age_var,
                      breaks=c(0,34,64,100),
                      labels=c("18-34","35-64","65+"))) %>% 
  group_by(YEAR, sex_recode, agecat) %>% 
    mutate(quintile = ntile(alc_frequency,5)) %>% ungroup() %>% 
  group_by(YEAR, sex_recode, quintile, agecat) %>% 
  summarise(meanfreqraw = mean(alc_frequency),
            meanfreqshifted = mean(frequency_upshifted)) %>% mutate(quintile=as.factor(quintile)) %>%
  # mutate(meanfreqraw = round(meanfreqraw), meanfreqshifted=round(meanfreqshifted)) %>% 
  pivot_longer(cols=meanfreqraw:meanfreqshifted) %>% pivot_wider(names_from=quintile, values_from=value) %>% 
  mutate(Q2Q1 = `2`/`1`, Q3Q1=`3`/`1`, Q4Q1 = `4`/`1`, Q5Q1 = `5`/`1`) %>% 
  group_by(YEAR, sex_recode, agecat, name) %>% 
  summarise(Q2Q1 = mean(Q2Q1),
            Q3Q1 = mean(Q3Q1),
            Q4Q1 = mean(Q4Q1),
            Q5Q1 = mean(Q5Q1)) %>% pivot_longer(cols=Q2Q1:Q5Q1, names_to="quintile") %>% filter(name=="meanfreqraw")

ggplot(data=USquintiles, aes(x=YEAR, y=value, colour=quintile)) + geom_line() + 
  facet_grid(cols=vars(agecat), rows=vars(sex_recode)) +
  xlab("") + ylab("Quintile ratio") + 
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text = element_text(family="serif",size=18)) +
  scale_fill_brewer(palette="Set1") +
  scale_x_continuous(breaks=c(1984, 1988, 1992, 1996, 
                               2000, 2004, 2008, 
                              2012, 2016),
                     expand=c(0,0))

ggsave("SIMAH_workplace/brfss/paper/frequency_quiintiles.png", dpi=500, width=33, height=19, units="cm")

USquintiles <- data %>% filter(State=="USA")  %>% filter(gramsperday!=0) %>% 
  mutate(agecat = cut(age_var,
                      breaks=c(0,34,64,100),
                      labels=c("18-34","35-64","65+"))) %>% 
  group_by(YEAR, sex_recode, agecat) %>% 
  mutate(quintile = ntile(gramsperday,5)) %>% ungroup() %>% 
  group_by(YEAR, sex_recode, quintile, agecat) %>% 
  summarise(meanquantraw = mean(gramsperday),
            meanquantshifted = mean(gramsperday_upshifted_crquotient)) %>% mutate(quintile=as.factor(quintile)) %>%
  # mutate(meanfreqraw = round(meanfreqraw), meanfreqshifted=round(meanfreqshifted)) %>% 
  pivot_longer(cols=meanquantraw:meanquantshifted) %>% pivot_wider(names_from=quintile, values_from=value) %>% 
  mutate(Q2Q1 = `2`/`1`, Q3Q1=`3`/`1`, Q4Q1 = `4`/`1`, Q5Q1 = `5`/`1`) %>% 
  group_by(YEAR, sex_recode, agecat, name) %>% 
  summarise(Q2Q1 = mean(Q2Q1),
            Q3Q1 = mean(Q3Q1),
            Q4Q1 = mean(Q4Q1),
            Q5Q1 = mean(Q5Q1)) %>% pivot_longer(cols=Q2Q1:Q5Q1, names_to="quintile") %>% filter(name=="meanquantraw")

ggplot(data=USquintiles, aes(x=YEAR, y=value, colour=quintile)) + geom_line() + 
  facet_grid(cols=vars(agecat), rows=vars(sex_recode)) +
  xlab("") + ylab("Quintile ratio") + 
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text = element_text(family="serif",size=18)) +
  scale_fill_brewer(palette="Set1") +
  scale_x_continuous(breaks=c(1984, 1988, 1992, 1996, 
                              2000, 2004, 2008, 
                              2012, 2016),
                     expand=c(0,0))

ggsave("SIMAH_workplace/brfss/paper/frequency_quiintiles.png", dpi=500, width=33, height=19, units="cm")