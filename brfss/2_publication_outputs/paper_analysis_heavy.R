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
data <- readRDS("SIMAH_workplace/brfss/processed_data/BRFSS_reweighted_upshifted_1984_2020.RDS")

# NY State targets 
NYstate <- data %>% filter(State=="New York") %>% 
  group_by(YEAR, sex_recode) %>% 
  summarise(prevalence = mean(drinkingstatus_updated))
Years <- expand.grid(sex_recode=c("Male","Female"), YEAR=1984:2019)
NYstate <- left_join(Years, NYstate)

ggplot(data=NYstate, aes(x=YEAR, y=prevalence)) + geom_point() +
  facet_grid(cols=vars(sex_recode)) + geom_line() + ylim(0,NA)


NYstate <- data %>% filter(State=="New York") %>% 
  group_by(YEAR, sex_recode) %>% 
  filter(drinkingstatus_updated==1) %>% 
  summarise(quantity = mean(gramsperday_upshifted_crquotient),
            sequantity = std.error(gramsperday_upshifted_crquotient),
            frequency = mean(frequency_upshifted)) %>% 
  pivot_wider(names_from=sex_recode, values_from=c(quantity,sequantity, frequency))

ggplot(data=NYstate, ae)

# mean GPD over time 
summary <- data %>% group_by(State, YEAR, sex_recode) %>% 
  filter(gramsperday_upshifted_crquotient>0) %>% 
  summarise(meanGPD = mean(gramsperday_upshifted_crquotient)) %>% 
  filter(State=="USA") %>% 
  mutate(sex_recode=ifelse(sex_recode=="Male","Men","Women"))

ggplot(data=summary, aes(x=YEAR, y=meanGPD)) + geom_line() + 
  facet_grid(cols=vars(sex_recode)) + ylim(0,NA) +
  theme_bw() + 
  theme(strip.background = element_rect(fill="white"),
        text = element_text(size=18)) +
  ylab("Mean grams per day (in drinkers)") + 
  xlab("")
ggsave("SIMAH_workplace/brfss/plots/BRFSS_gpd_1984_2020.png", dpi=300,
       width=33, height=19, units="cm")

summary <- data %>% 
  mutate(heavydrinking_baseline = ifelse(sex_recode=="Female" & gramsperday>=14,1,
                                       ifelse(sex_recode=="Male" & gramsperday>=28,1,0)),
         heavydrinking_upshifted = ifelse(sex_recode=="Female" & gramsperday_upshifted_crquotient>=14,1,
                                          ifelse(sex_recode=="Male" & gramsperday_upshifted_crquotient>=28,1,0))) %>% 
  group_by(State, YEAR,sex_recode) %>% 
  summarise(percentheavy_baseline = mean(heavydrinking_baseline),
            percentheavy_upshifted = mean(heavydrinking_upshifted)) %>% 
  pivot_longer(cols=c(percentheavy_baseline:percentheavy_upshifted))

ggplot(data=subset(summary, State=="USA"), aes(x=YEAR, y=value, colour=name)) + 
  geom_line() + facet_grid(cols=vars(sex_recode))

summary_table <- summary %>% pivot_wider(names_from=name, values_from=value)

percent_consumed_raw <- data %>% group_by(YEAR, sex_recode, State, region) %>% 
  mutate(total_alcohol_baseline = sum(gramsperday),
         topdrinkers = ntile(gramsperday, 100),
         topdrinkersbin = ifelse(topdrinkers>=95, 1,0)) %>% 
  group_by(YEAR, sex_recode, State, region, topdrinkersbin) %>% 
  summarise(sumgrams_raw = sum(gramsperday)) %>% ungroup() %>% 
  group_by(YEAR, sex_recode, State) %>% 
  mutate(percent_top_raw = sumgrams_raw/sum(sumgrams_raw))

percent <- percent_consumed_raw %>% filter(topdrinkersbin==1)

ggplot(data=percent, aes(x=YEAR, y=percent_top_raw, colour=State)) + 
  geom_line() + 
  facet_grid(cols=vars(region), rows=vars(sex_recode))

# compare with alcohol related deaths from IHME
IHME <- read.csv("SIMAH_workplace/brfss/paper/IHME-GBD_2019_DATA-df69af32-1.csv") %>% 
  mutate(cause = recode(cause, "524"="Liver cirrhosis due to alc use",
                        "420"="Liver cancer due to alc use",
                        "938"="Alcoholic cardiomyopathy",
                        "560"="Alcohol use disorders"),
         sex = recode(sex, "1"="Men","2"="Women")) %>% filter(metric!=2) %>% 
  mutate(metric = recode(metric, "1"="Number","3"="Rate"))

locations <- readxl::read_excel("SIMAH_workplace/brfss/paper/locations.xlsx")

IHME <- left_join(IHME, locations) %>% dplyr::select(STATE, year, sex, cause, metric, val, upper, lower) %>% 
  pivot_wider(names_from=metric, values_from=c(val, upper, lower)) %>% rename(State=STATE) %>% 
  mutate(State=ifelse(State=="United States of America", "USA", 
                      ifelse(State=="District of Columbia", "DC", State)))

summary_table <- data %>% group_by(YEAR, State, sex_recode) %>% 
  mutate(heavydrinking_baseline = ifelse(sex_recode=="Female" & gramsperday>=14,1,
                                         ifelse(sex_recode=="Male" & gramsperday>=28,1,0)),
         heavydrinking_upshifted = ifelse(sex_recode=="Female" & gramsperday_upshifted_crquotient>=14,1,
                                          ifelse(sex_recode=="Male" & gramsperday_upshifted_crquotient>=28,1,0))) %>% 
  summarise(meanGPD_raw = mean(gramsperday),
            meanGPD_upshifted = mean(gramsperday_upshifted_crquotient),
            meanheavy_raw = mean(heavydrinking_baseline),
            meanheavy_upshifted = mean(heavydrinking_upshifted)) %>% 
  rename(year=YEAR, sex=sex_recode) %>% 
  mutate(sex = ifelse(sex=="Female","Women","Men"))

# summary_table <- summary %>% pivot_wider(names_from=name, values_from=value) %>% rename(year=YEAR, sex=sex_recode) %>% 
#   mutate(sex = ifelse(sex=="Female","Women","Men"))

IHME <- left_join(IHME, summary_table) %>% 
  dplyr::select(-c(upper_Number:lower_Rate)) %>% 
  pivot_longer(cols=meanGPD_raw:meanheavy_upshifted) %>% 
  mutate(type = ifelse(grepl("raw",name),"raw","upshifted"),
         measure = ifelse(grepl("GPD",name),"gpd","heavy")) %>% dplyr::select(-name)

correlations <- IHME %>% 
  group_by(State, sex, cause, type, measure) %>% 
    summarise(cor_number = cor(val_Number, value, use="complete.obs"),
              cor_rate = cor(val_Rate, value, use="complete.obs")) %>% 
  pivot_wider(names_from=type, values_from=c(cor_number:cor_rate)) %>% 
  mutate(rate_difference = cor_rate_upshifted - cor_rate_raw,
         number_difference = cor_number_upshifted - cor_number_raw) %>% filter(measure=="gpd") %>% 
  filter(State=="USA")
IHME$period <- IHME$year-1990
library(lme4)
model_mixed <- lmer(log(val_Rate) ~ value + period + (1|State), data=subset(IHME, measure=="gpd" & type=="raw" & 
                                                                       cause=="Alcohol use disorders" & 
                                                                sex=="Men"))
summary(model_mixed)
model_mixed <- lmer(log(val_Rate) ~ value + period + (1|State), data=subset(IHME, measure=="gpd" & type=="upshifted" &
                                                                     cause=="Alcohol use disorders" & 
                                                                sex=="Men"))
summary(model_mixed)
subset <- IHME %>% filter(State=="USA" & cause=="Alcohol use disorders" & measure=="heavy")

ggplot(data=subset, aes(x=log(val_Rate), y=value)) + geom_point() + 
  facet_grid(cols=vars(sex), rows=vars(type))


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


quintiles <- data %>% group_by(YEAR, State, sex_recode) %>% filter(gramsperday!=0) %>% 
  mutate(quintile = ntile(gramsperday,5)) %>% 
  ungroup() %>% 
  group_by(YEAR, State, sex_recode, quintile) %>% 
  summarise(mean_GPD_baseline = mean(gramsperday),
            mean_GPD_adjusted = mean(gramsperday_upshifted_crquotient),
            mean_freq_baseline = mean(alc_frequency),
            mean_freq_adjusted = mean(frequency_upshifted))

ggplot(data=subset(quintiles, State=="USA"), aes(x=YEAR, y=value, colour=name)) + 
  facet_grid(cols=vars(quintile), rows=vars(sex_recode)) +
  geom_line()
