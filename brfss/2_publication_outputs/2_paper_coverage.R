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

# first calculate coverage for the US and by state
trendovertime <- data %>% filter(State=="USA") %>% group_by(YEAR) %>% 
  filter(drinkingstatus_updated==1) %>% 
  summarise(raw_gpd = mean(gramsperday),
            raw_apc = mean(gramspercapita),
            new_apc = mean(gramspercapita_90),
            new_gpd = mean(gramsperday_upshifted_crquotient)) %>% 
  filter(YEAR<=2020) %>% pivot_longer(cols=raw_gpd:new_gpd) %>% 
  mutate(name = recode(name, "raw_gpd"="Initial BRFSS",
                       "raw_apc"="Initial APC", "new_apc"="Adjusted APC",
                       "new_gpd"="Adjusted BRFSS"),
         name = factor(name, levels=c("Initial APC","Adjusted APC", "Adjusted BRFSS","Initial BRFSS")))

ggplot(data=trendovertime, aes(x=YEAR, y=value, colour=name, linetype=name)) + geom_line(size=1.5) +
  ylim(0,NA) +
  theme_bw() + 
  scale_linetype_manual(values=c("solid","dashed","dotted","dotdash")) + 
  scale_colour_grey(start=0.8, end=0.2) + 
  # scale_colour_brewer(palette="Greys") + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text = element_text(family="serif",size=18),
        legend.key.width=unit(2,"cm")) +
  # scale_fill_brewer(palette="Set1") +
  scale_x_continuous(breaks=c(1984, 1986, 1988, 1990, 1992, 1994, 1996, 
                              1998, 2000, 2002, 2004, 2006, 2008, 2010, 
                              2012, 2014, 2016, 2018, 2020),
                     expand=c(0.01,0.01)) +
  xlab("") + ylab("Grams alcohol per day")

ggsave("SIMAH_workplace/brfss/paper/Figure2.png", dpi=500, width=33, height=19, units="cm")

Table2 <- trendovertime %>% 
  mutate(value=round(value,digits=1)) %>% 
  pivot_wider(names_from=name, values_from=value) %>% 
  rename(raw_brfss = `Raw BRFSS`, 
         adjusted_brfss = `Adjusted BRFSS`,
         adjusted_APC = `Adjusted APC`,
         raw_APC = `Raw APC`,
         Year = YEAR) %>% 
  dplyr::select(Year, raw_brfss, adjusted_brfss, raw_APC, adjusted_APC) %>% 
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
  filter(drinkingstatus_updated==1) %>% 
  summarise(raw_gpd = mean(gramsperday),
            raw_apc = mean(gramspercapita),
            new_apc = mean(gramspercapita_90),
            new_gpd = mean(gramsperday_upshifted_crquotient)) %>% 
  filter(YEAR<=2020) %>% pivot_longer(cols=raw_gpd:new_gpd) %>% 
  mutate(name = recode(name, "raw_gpd"="Initial BRFSS",
                       "raw_apc"="Initial APC", "new_apc"="Adjusted APC",
                       "new_gpd"="Adjusted BRFSS"),
         name = factor(name, levels=c("Initial APC","Adjusted APC", "Initial BRFSS","Adjusted BRFSS")))

ggplot(data=trendovertime, aes(x=YEAR, y=value, colour=name, linetype=name)) + geom_line(size=1.5) +
  ylim(0,NA) +
  theme_bw() + 
  scale_linetype_manual(values=c("solid","dashed","dotted","dotdash")) + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text = element_text(family="serif",size=18),
        strip.background=element_rect(fill="white"),
        legend.key.width=unit(2,"cm")) +
  scale_fill_brewer(palette="Set1") +
  scale_colour_grey(start=0.8, end=0.2) + 
  scale_x_continuous(breaks=c(1984, 1986, 1988, 1990, 1992, 1994, 1996, 
                              1998, 2000, 2002, 2004, 2006, 2008, 2010, 
                              2012, 2014, 2016, 2018),
                     expand=c(0,0)) +
  xlab("") + ylab("Grams alcohol per day") +
  facet_grid(rows=vars(State))

ggsave("SIMAH_workplace/brfss/paper/FigureA2_states.png", dpi=500, width=21, height=29, units="cm")

trendovertime <- data %>% group_by(State,YEAR) %>% 
  filter(drinkingstatus_updated==1) %>% 
  summarise(raw_gpd = mean(gramsperday),
            raw_apc = mean(gramspercapita),
            new_apc = mean(gramspercapita_90),
            new_gpd = mean(gramsperday_upshifted_crquotient)) %>% 
  filter(YEAR<=2020) %>% pivot_longer(cols=raw_gpd:new_gpd) %>% 
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
         raw_APC = `APC`,
         Year = YEAR) %>% 
  dplyr::select(State, Year, raw_brfss, adjusted_brfss, raw_APC, adjusted_APC) %>% 
  mutate(coverage_baseline = round(raw_brfss/adjusted_APC, digits=3),
         coverage_adjusted = round(adjusted_brfss / adjusted_APC,digits=3))

means <- Table3 %>% group_by(State) %>% 
  summarise(coverage_baseline_mean = round(mean(coverage_baseline),digits=3),
            coverage_baseline_sd = round(sd(coverage_baseline),digits=3),
            coverage_adjusted_mean = round(mean(coverage_adjusted),digits=3),
            coverage_adjusted_sd = round(sd(coverage_adjusted),digits=3))
write.csv(means, "SIMAH_workplace/brfss/paper/TableA5.csv", row.names=F)

# draw map plot 

library(usmap)
means$state <- tolower(means$State)
means <- means %>% pivot_longer(cols=c(coverage_baseline_mean, coverage_adjusted_mean)) %>% 
  rename(Coverage=value) %>% 
  mutate(name = ifelse(name=="coverage_baseline_mean","Initial","Adjusted"),
         name = factor(name, levels=c("Initial","Adjusted")))


plot1 <- plot_usmap(data=means, values="Coverage") +
  scale_fill_gradient(low="yellow", high="blue", labels=scales::percent_format(accuracy=5L)) +
  facet_grid(rows=vars(name)) +
  theme(strip.background=element_rect(fill="white"),
        strip.text = element_text(size=18),
        legend.text = element_text(size=16),
        legend.title = element_text(size=18))
plot1

ggsave("SIMAH_workplace/brfss/paper/Figure2_map.png", dpi=700, width=23, height=30, units="cm")



