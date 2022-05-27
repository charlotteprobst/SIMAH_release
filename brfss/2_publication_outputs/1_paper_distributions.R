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
data <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_1984_2020_paper.RDS")

# subset <- data %>% filter(State=="USA") %>% 
#   dplyr::select(YEAR, gramsperday, gramsperday_upshifted_crquotient) %>% 
#   pivot_longer(gramsperday:gramsperday_upshifted_crquotient) %>% 
#   mutate(name = ifelse(name=="gramsperday","Unadjusted","Adjusted"),
#          name = factor(name, levels=c("Unadjusted","Adjusted"))) %>% 
#   filter(YEAR==2020) %>%
#   mutate(value=ifelse(value>200, 200, value)) %>% 
#   group_by(name) %>% 
#   mutate(mean = mean(value))
# 
# ggplot(data=subset, aes(x=value)) + geom_histogram(bins=50, colour="black",
#                                                    fill="white") + 
#   facet_grid(rows=vars(name)) + theme_bw() + 
#   geom_vline(aes(xintercept=mean), linetype="dashed", size=1) +
#   xlab("Grams per day") + ylab("Total")
# ggsave("SIMAH_workplace/brfss/paper/SuppFigureDistributions.png", dpi=500, width=33, height=19, units="cm")

summary <- data %>% 
  # filter(State=="USA") %>% 
  group_by(State, YEAR, drinkingstatus_detailed) %>%
  tally() %>% ungroup() %>%
  group_by(State, YEAR) %>%
  mutate(percent = n/sum(n)) %>% 
  mutate(drinkingstatus_detailed=recode(drinkingstatus_detailed,
                                        "Former drinker"="Former drinkers",
                                        "Lifetime abstainer"="Lifetime abstainers",
                                        "Yearly drinker"="30-day abstainers (annual drinkers)",
                                        "Monthly drinker"="30 day drinker"),
    drinkingstatus_detailed = factor(drinkingstatus_detailed,
                                          levels=c("Lifetime abstainers",
                                                   "Former drinkers",
                                                   "30-day abstainers (annual drinkers)",
                                                   "30 day drinker")))

# Figure1 <- ggplot(data=subset(summary, State=="USA"), aes(x=YEAR, y=percent, fill=drinkingstatus_detailed)) +
#   geom_bar(stat="identity",position=position_fill(reverse=T)) +
#   xlab("Year") + ylab("Proportion") +
#   theme_bw() + 
#   theme(legend.position="bottom",
#         legend.title=element_blank(),
#         text = element_text(family="serif",size=18)) +
#   scale_fill_brewer(palette="Set1") +
#   scale_y_continuous(labels=scales::percent, expand=c(0,0),limits=c(0,1.01)) + 
#   scale_x_continuous(breaks=c(1984, 1986, 1988, 1990, 1992, 1994, 1996, 
#                               1998, 2000, 2002, 2004, 2006, 2008, 2010, 
#                               2012, 2014, 2016, 2018),
#                      expand=c(0,0))
# Figure1
# ggsave("SIMAH_workplace/brfss/paper/Figure1_reweighted.png", dpi=500, width=33, height=19, units="cm")

summary2 <- summary %>% filter(State=="Colorado" 
                              | State=="New York" |
                                State=="Minnesota" |
                                State=="Tennessee" |
                                State=="Texas") %>% 
  filter(drinkingstatus_detailed!="30 day drinker")


Figure2 <- ggplot(data=summary2, aes(x=YEAR, y=percent, fill=drinkingstatus_detailed)) +
  geom_bar(stat="identity", colour="black") +
  xlab("Year") + ylab("Proportion") +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text = element_text(family="serif",size=18),
        strip.background=element_rect(fill="white")) +
  scale_fill_brewer(palette="Greys") +
  scale_y_continuous(labels=scales::percent, expand=c(0,0),limits=c(0,1.01)) + 
  scale_x_continuous(breaks=c(1984, 1986, 1988, 1990, 1992, 1994, 1996, 
                              1998, 2000, 2002, 2004, 2006, 2008, 2010, 
                              2012, 2014, 2016, 2018,2020),
                     expand=c(0,0)) +
  facet_grid(rows=vars(State))
Figure2
ggsave("SIMAH_workplace/brfss/paper/Figure1_STATES.png", dpi=500, width=33, height=19, units="cm")

data <- data %>% 
  mutate(drinkingstatus_updated = ifelse(drinkingstatus_detailed=="Monthly drinker" |
                                           drinkingstatus_detailed=="Yearly drinker", 1,0))

TableA1 <- data %>% group_by(State,YEAR, sex_recode) %>% 
  filter(YEAR<=2020) %>% 
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

