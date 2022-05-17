# SIMAH project April 2022 - analysing state-level covid-19 policies for supplement

library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(viridis)

# CB laptop directory
wd <- "~/Google Drive/SIMAH Sheffield/"
# CB desktop directory
# wd <- "G:/My Drive/SIMAH Sheffield/"
setwd(wd)

data <- read.csv("SIMAH_workplace/covid-19_policy/OxCGRT_US_latest.csv") %>% filter(Jurisdiction=="STATE_WIDE") %>% 
  dplyr::select(CountryCode, RegionName, Jurisdiction, Date, StringencyIndex,
                GovernmentResponseIndex, ContainmentHealthIndex, EconomicSupportIndex) %>% 
  mutate(Date = as.character(Date),
         Date = as.Date(Date,"%Y%m%d")) %>% 
  pivot_longer(cols=StringencyIndex:EconomicSupportIndex, values_to="Index")

data$region <- tolower(data$RegionName)
data$region <- ifelse(data$region=="Washington DC","district of columbia",data$region)
states_map <- map_data("state")

covid_map <- left_join(states_map, data, by=c("region"))

ggplot(data=data, aes(x=Date, y=Index, colour=RegionName)) + geom_line() +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  scale_x_date(date_labels="%b-%Y") +
  facet_grid(cols=vars(name))
ggsave("SIMAH_workplace/covid-19_policy/plot_all_states.png",dpi=300, width=33, height=19, units="cm")


SIMAH_states <- c("California","Colorado","Florida","Indiana","Kentucky","Louisiana",
                  "Massachusetts","Michigan","Minnesota","Missouri","New York","Oregon",
                  "Pennsylvania","Tennessee","Texas")

data_sub <- data %>% filter(RegionName %in% SIMAH_states)

mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(15)


ggplot(data=data_sub, aes(x=Date, y=Index, colour=RegionName)) + geom_line(size=1) +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  scale_x_date(date_labels="%b-%Y") +
  facet_grid(cols=vars(name)) +
  scale_colour_manual(values=mycolors)

ggsave("SIMAH_workplace/covid-19_policy/plot_SIMAH_states.png",dpi=300, width=33, height=19, units="cm")

