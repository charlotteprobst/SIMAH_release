# SIMAH October 2021 - code to generate plots for publication
library(foreign)
library(SASxport)
library(readr)
library(dplyr)
library(tidyr)
library(labelled)
library(sjlabelled)
library(tidyverse)
library(maps)
library(viridis)

# CB laptop directory
wd <- "~/Google Drive/SIMAH Sheffield/"
# CB desktop directory
# wd <- "G:/My Drive/SIMAH Sheffield/"
setwd(wd)

####read in the joined up data files 
data <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS__upshifted_1984_2020_paper.RDS")

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
                                State=="Texas") %>% 
  filter(drinkingstatus_detailed!="30 day drinker")

Figure2 <- ggplot(data=summary2, aes(x=YEAR, y=percent, fill=drinkingstatus_detailed)) +
  geom_bar(stat="identity",position="stack",colour="black") +
  xlab("Year") + ylab("Proportion") +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text = element_text(family="serif",size=18),
        strip.background=element_rect(fill="white")) +
  # scale_fill_manual(values=c("black","grey80","grey2")) + 
  scale_fill_brewer(palette="Greys") +
  scale_y_continuous(labels=scales::percent, expand=c(0,0),limits=c(0,1.01)) +
  scale_x_continuous(breaks=c(1984, 1986, 1988, 1990, 1992, 1994, 1996, 
                              1998, 2000, 2002, 2004, 2006, 2008, 2010, 
                              2012, 2014, 2016, 2018),
                     expand=c(0,0)) +
  facet_grid(rows=vars(State))
Figure2
ggsave("SIMAH_workplace/brfss/paper/Figure1_STATES.png", dpi=500, width=33, height=19, units="cm")

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

coverage <- data %>% group_by(State, YEAR, sex_recode) %>% 
  filter(YEAR<=2019) %>% 
  mutate(sex_recode=ifelse(sex_recode=="Female","women","men")) %>% 
  summarise(raw_brfss_gramsperday = mean())

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
         adjusted_APC = `adjusted APC`,
         Year = YEAR) %>% 
  dplyr::select(Year, raw_brfss, adjusted_brfss, adjusted_APC) %>% 
  mutate(coverage_baseline = round(raw_brfss/adjusted_APC, digits=3),
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
  facet_grid(rows=vars(State))

ggsave("SIMAH_workplace/brfss/paper/FigureA2_states.png", dpi=500, width=21, height=29, units="cm")

trendovertime <- data %>% group_by(State,YEAR) %>% 
  filter(gramsperday!=0) %>% 
  summarise(raw_gpd = mean(gramsperday),
            raw_apc = mean(gramspercapita),
            new_apc = mean(gramspercapita_90),
            new_gpd = mean(gramsperday_upshifted)) %>% 
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
         adjusted_APC = `adjusted APC`,
         Year = YEAR) %>% 
  dplyr::select(State, Year, raw_brfss, adjusted_brfss, adjusted_APC) %>% 
  mutate(coverage_baseline = round(raw_brfss/adjusted_APC, digits=3),
         coverage_adjusted = round(adjusted_brfss / adjusted_APC,digits=3))

means <- Table3 %>% group_by(State) %>% 
  summarise(coverage_baseline_mean = round(mean(coverage_baseline),digits=3),
            coverage_baseline_sd = round(sd(coverage_baseline),digits=3),
            coverage_adjusted_mean = round(mean(coverage_adjusted),digits=3),
            coverage_adjusted_sd = round(sd(coverage_adjusted),digits=3))
write.csv(means, "SIMAH_workplace/brfss/paper/TableA5.csv", row.names=F)

# draw a map 
means$region <- tolower(means$State)
means$region <- ifelse(means$region=="dc","district of columbia",means$region)
states_map <- map_data("state")

coverage_map <- left_join(states_map, means, by=c("region"))
coverage_map$coverage_baseline_mean <- round(coverage_map$coverage_baseline_mean, digits=2)

coverage_map <- coverage_map %>% pivot_longer(cols=c(coverage_baseline_mean, coverage_adjusted_mean)) %>% 
  mutate(name = ifelse(name=="coverage_baseline_mean","Raw","Adjusted"),
         name = factor(name, levels=c("Raw","Adjusted")))

ggplot(coverage_map, aes(long, lat, group=group)) + 
  geom_polygon(aes(fill=value), color="white") +
  # scale_fill_viridis(option="C", labels=scales::percent, name="coverage") +
  scale_fill_brewer(palette="YlGnBu", labels=scales::percent, name="coverage") +
  theme(legend.background=element_rect(fill="white", colour=NA)) +
  facet_grid(rows=vars(name), switch="y") + 
  theme_minimal() + 
  xlab("") + ylab("") + theme(axis.line=element_blank(),
        strip.text.y.left = element_text(angle=0),
        axis.text = element_blank(),
        axis.ticks=element_blank(),
        plot.background=element_rect(fill="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="right",
        strip.text = element_text(size=14),
        legend.text = element_text(size=10))

ggsave("SIMAH_workplace/brfss/plots/coverage_map.png", dpi=500, width=33, height=33, units='cm')


# proportion of people in each WHO drinking category at baseline and adjusted 
categories_baseline <- data %>% 
  group_by(YEAR, State, sex_recode) %>% 
  mutate(drinkingcat = ifelse(gramsperday=="0", "abstainer",
                                       ifelse(gramsperday>0 & gramsperday<=20 & sex_recode=="Female","Category I",
                                              ifelse(gramsperday>20 & gramsperday<=40 & sex_recode=="Female","Category II",
                                                     ifelse(gramsperday>40 & gramsperday<=60 & sex_recode=="Female", "Category III",
                                                            ifelse(gramsperday>60 & sex_recode=="Female","Category III",
                                                                   ifelse(gramsperday>0 & gramsperday<=40 & sex_recode=="Male","Category I",
                                                                          ifelse(gramsperday>40 & gramsperday<=60 & sex_recode=="Male","Category II",
                                                                                 ifelse(gramsperday>60 & gramsperday<=100 & sex_recode=="Male","Category III",
                                                                                        ifelse(gramsperday>100 & sex_recode=="Male", "Category III", NA)))))))))) %>% 
  group_by(YEAR, State, drinkingcat, .drop=FALSE) %>% 
  tally() %>% mutate(type="baseline")

categories_adjusted <- data %>% 
  group_by(YEAR, State, sex_recode) %>% 
  mutate(drinkingcat = ifelse(gramsperday_upshifted=="0", "abstainer",
                                       ifelse(gramsperday_upshifted>0 & gramsperday_upshifted<=20 & sex_recode=="Female","Category I",
                                              ifelse(gramsperday_upshifted>20 & gramsperday_upshifted<=40 & sex_recode=="Female","Category II",
                                                     ifelse(gramsperday_upshifted>40 & gramsperday_upshifted<=60 & sex_recode=="Female", "Category III",
                                                            ifelse(gramsperday_upshifted>60 & sex_recode=="Female","Category III",
                                                                   ifelse(gramsperday_upshifted>0 & gramsperday_upshifted<=40 & sex_recode=="Male","Category I",
                                                                          ifelse(gramsperday_upshifted>40 & gramsperday_upshifted<=60 & sex_recode=="Male","Category II",
                                                                                 ifelse(gramsperday_upshifted>60 & gramsperday_upshifted<=100 & sex_recode=="Male","Category III",
                                                                                        ifelse(gramsperday_upshifted>100 & sex_recode=="Male", "Category III", NA)))))))))) %>% 
  group_by(YEAR, State, drinkingcat, .drop=FALSE) %>% 
  tally() %>% 
  mutate(type = "adjusted")


categories <- rbind(categories_adjusted, categories_baseline) %>% 
  group_by(YEAR, State, type) %>% 
  mutate(percent = n/sum(n)) %>% 
  dplyr::select(YEAR, State, drinkingcat, type, percent) %>% 
  pivot_wider(names_from=type, values_from=percent)
categories$type = factor(categories$type, levels=c("baseline","adjusted"))

USA <- categories %>% filter(State=="USA") %>% 
  pivot_longer(cols=adjusted:baseline) %>% 
  mutate(name = factor(name, levels=c("baseline","adjusted"))) %>% 
  group_by(State, sex_recode, drinkingcat, name) %>% 
  summarise(mean = round(mean(value, na.rm=T),digits=4)*100,
            sd = round(sd(value, na.rm=T),digits=4)*100) %>% 
  pivot_wider(names_from=name, values_from=c(mean,sd)) %>% 
  mutate(ratio = round(mean_adjusted / mean_baseline,digits=2))

write.csv(USA, "SIMAH_workplace/brfss/paper/TableDrinkingCatsUSA.csv", row.names=F)

categories <- categories %>% pivot_longer(cols=adjusted:baseline) %>% 
  mutate(name = ifelse(name=="baseline","Baseline","Adjusted"),
         name = factor(name, levels=c("Baseline","Adjusted")),
         drinkingcat= ifelse(drinkingcat=="abstainer","Abstainer", drinkingcat))

Figure5 <- ggplot(data=subset(categories, State=="USA"), aes(x=YEAR, y=value, fill=drinkingcat)) +
  geom_bar(stat="identity",position=position_fill(reverse=T)) +
  xlab("Year") + ylab("Proportion") + facet_grid(rows=vars(name), switch='y') + 
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
Figure5
ggsave("SIMAH_workplace/brfss/paper/Figure5_drinkingcategories.png", dpi=500, width=22, height=20, units="cm")

frequency_categories_baseline <- data %>% 
  group_by(YEAR, State, sex_recode) %>% 
  mutate(drinkingcat = ifelse(gramsperday=="0", "abstainer",
                              ifelse(gramsperday>0 & gramsperday<=20 & sex_recode=="Female","Category I",
                                     ifelse(gramsperday>20 & gramsperday<=40 & sex_recode=="Female","Category II",
                                            ifelse(gramsperday>40 & gramsperday<=60 & sex_recode=="Female", "Category III",
                                                   ifelse(gramsperday>60 & sex_recode=="Female","Category III",
                                                          ifelse(gramsperday>0 & gramsperday<=40 & sex_recode=="Male","Category I",
                                                                 ifelse(gramsperday>40 & gramsperday<=60 & sex_recode=="Male","Category II",
                                                                        ifelse(gramsperday>60 & gramsperday<=100 & sex_recode=="Male","Category III",
                                                                               ifelse(gramsperday>100 & sex_recode=="Male", "Category III", NA)))))))))) %>% 
  group_by(YEAR, State, drinkingcat, .drop=FALSE) %>% 
  tally() %>% mutate(type="baseline")

categories_baseline <- data %>% 
  group_by(YEAR, State, sex_recode) %>% 
  mutate(drinkingcat = ifelse(alc_frequency==0, "abstainer",
                              ifelse(alc_frequency>0 & alc_frequency<=5,"1-5 days per month",
                                     ifelse(alc_frequency>5 & alc_frequency<=10,"6-10 days per month",
                                            ifelse(alc_frequency>10 & alc_frequency<=15, "11-15 days per month",
                                                   ifelse(alc_frequency>15 & alc_frequency<=20, "16-20 days per month",
                                                          ifelse(alc_frequency>20 & alc_frequency<=25, "21-25 days per month",
                                                                 ifelse(alc_frequency>25, "25-30 days per month",NA)))))))) %>% 
  group_by(YEAR, State, drinkingcat, sex_recode,.drop=FALSE) %>% 
  tally() %>% 
  mutate(type = "Baseline")

categories_adjusted <- data %>% 
  group_by(YEAR, State, sex_recode) %>% 
  mutate(drinkingcat = ifelse(frequency_upshifted==0, "abstainer",
                              ifelse(frequency_upshifted>0 & frequency_upshifted<=5,"1-5 days per month",
                                     ifelse(frequency_upshifted>5 & frequency_upshifted<=10,"6-10 days per month",
                                            ifelse(frequency_upshifted>10 & frequency_upshifted<=15, "11-15 days per month",
                                                   ifelse(frequency_upshifted>15 & frequency_upshifted<=20, "16-20 days per month",
                                                          ifelse(frequency_upshifted>20 & frequency_upshifted<=25, "21-25 days per month",
                                                                 ifelse(frequency_upshifted>25, "25-30 days per month",NA)))))))) %>% 
  group_by(YEAR, State, drinkingcat,sex_recode, .drop=FALSE) %>% 
  tally() %>% 
  mutate(type = "Adjusted")

categories <- rbind(categories_adjusted, categories_baseline) %>% 
  group_by(YEAR, State, sex_recode, type) %>% 
  mutate(percent = n/sum(n)) %>% 
  dplyr::select(YEAR, State, sex_recode, drinkingcat, type, percent) %>% 
  pivot_wider(names_from=type, values_from=percent)

USfreqcategories <- categories %>% filter(State=="USA") %>% 
  group_by(sex_recode, drinkingcat) %>% 
  summarise(meanBaseline=mean(Baseline),
            sdBaseline = sd(Baseline),
            meanAdjusted = mean(Adjusted),
            sdAdjusted = sd(Adjusted),
            ratio = meanAdjusted/meanBaseline)
write.csv(USfreqcategories, "SIMAH_workplace/brfss/paper/TableFreqCatsUSA.csv", row.names=F)
