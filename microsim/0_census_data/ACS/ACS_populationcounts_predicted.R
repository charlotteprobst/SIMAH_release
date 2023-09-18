#  SIMAH project 2022 

# projecting the population for 2020 and comparing with ACS projections based on weights 

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
suppressPackageStartupMessages(library("dplyr"))
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)
options(scipen=999)

####EDIT ONLY BELOW HERE ### 
###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(paste(WorkingDirectory))

# first read in the data 2000 to 2020
ACS_20002020 <- read.csv("SIMAH_workplace/ACS/ACS_popcounts_2000_2020.csv") %>% filter(state=="USA")

# now read in the replication weights for ACS 
ACS_2020weights <- readRDS("SIMAH_workplace/ACS/rep_weights_2020.RDS") %>% 
  do.call(rbind,.) %>% group_by(year,sex,race,age_gp, edclass) %>% 
  summarise(min = min(TPop),
            max = max(TPop)) %>% mutate(age_gp=as.numeric(age_gp))

# join the original data with the min and max for the 2020 weights 
ACS_20002020 <- left_join(ACS_20002020, ACS_2020weights)

# now plot to show the jump in 2020
ggplot(data=subset(ACS_20002020,sex==1), aes(x=year, y=TPop, colour=edclass)) +
  geom_line() + geom_errorbar(aes(ymin=min, ymax=max)) + 
  facet_grid(cols=vars(age_gp), rows=vars(race), scales="free") +
  geom_vline(aes(xintercept=2019)) + xlim(2010,2020)


# calculate percent change year on year 
pct_change <- ACS_20002020 %>% 
  group_by(sex, age_gp, race, edclass) %>% 
  arrange(year, .by_group=TRUE) %>% 
  mutate(pct_change= (TPop/lag(TPop)-1) * 100)

ggplot(data=subset(pct_change,sex==1), aes(x=year, y=pct_change, colour=edclass)) +
  geom_line() + 
  facet_grid(cols=vars(age_gp), rows=vars(race), scales="free") +
  geom_vline(aes(xintercept=2019)) + xlim(2010,2020)

pct_change_overall = pct_change %>% filter(year<2020) %>%  
  group_by(sex, age_gp, race, edclass) %>% 
  summarise(mean_pct_change = mean(pct_change, na.rm=T)) %>% 
  mutate(type="pre2020")

pct_change_2020 <- pct_change %>% filter(year==2020) %>% 
  summarise(mean_pct_change = mean(pct_change)) %>% 
  mutate(type="2020")

pct_change_overall <- rbind(pct_change_overall, pct_change_2020) %>% 
  pivot_wider(names_from=type, values_from=mean_pct_change)


# now fit a linear model to predict - data without 2020
ACS_20002020$time <- ACS_20002020$year-2000

data <- ACS_20002020 %>% 
  filter(sex==2 & age_gp==18 & race=="Black" & edclass=="College")

arima.model <- auto.arima(data$TPop)
forecast(arima.model)

fore_arima = forecast::forecast(arima.model, h=12)
df_arima = as.data.frame(fore_arima)
dat_test$arima = df_arima$`Point Forecast`

pred_function <- function(data){
  model <- lm(TPop ~ year, data=subset(data, year!=2020 & year>=2015))
  data$predicted <- predict(model, data)
  return(data)
}

modelled <- ACS_20002020 %>% 
  group_by(sex, age_gp, race, edclass) %>% 
  do(pred_function(.)) %>% 
  pivot_longer(cols=c(TPop, predicted)) %>% 
  mutate(name=factor(name, levels=c("TPop","predicted")))


# plot the modelled versus observed population 
ggplot(data=subset(modelled, sex==1), aes(x=year, y=value, colour=as.factor(edclass), linetype=name)) + 
  geom_line(size=1) + facet_grid(cols=vars(age_gp), rows=vars(race), scales="free") +
  xlim(2015,2020) + theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank()) + ggtitle("Men") + 
  scale_x_continuous(breaks=c(2015,2020), limits=c(2015,2020))
ggsave("SIMAH_workplace/demography/plots/modelled_ACS_popcounts_men.png",
       dpi=300, width=33, height=19, units="cm")

ggplot(data=subset(modelled, sex==2), aes(x=year, y=value, colour=as.factor(edclass), linetype=name)) + 
  geom_line(size=1) + facet_grid(cols=vars(age_gp), rows=vars(race), scales="free") +
  xlim(2015,2020) + theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank()) + ggtitle("Women") + 
  scale_x_continuous(breaks=c(2015,2020), limits=c(2015,2020))

ggsave("SIMAH_workplace/demography/plots/modelled_ACS_popcounts_women.png",
       dpi=300, width=33, height=19, units="cm")

# now create an overall plot 
overall <- modelled %>% group_by(year, sex, race, edclass, name) %>% 
  summarise(value=sum(value)) %>% 
  mutate(sex = ifelse(sex==1, "Men","Women"))

ggplot(data=subset(overall), aes(x=year, y=value, colour=as.factor(edclass), linetype=name)) + 
  geom_line(size=1) + facet_grid(cols=vars(sex), rows=vars(race), scales="free") +
  xlim(2015,2020) + theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank()) + 
  scale_x_continuous(breaks=c(2015,2020), limits=c(2015,2020))

ggsave("SIMAH_workplace/demography/plots/modelled_ACS_popcounts_overall.png",
       dpi=300, width=33, height=19, units="cm")

finaldata_2020 <- modelled  %>% 
  pivot_wider(names_from=name, values_from=value) %>% 
  dplyr::select(state,year,sex, age_gp, race, edclass,predicted) %>% filter(year==2020) %>% 
  rename(TPop=predicted)

# join with the original data 
ACS_20002020 <- read.csv("SIMAH_workplace/ACS/ACS_popcounts_2000_2020.csv") %>% filter(state=="USA") %>% 
  filter(year<=2019)
ACS_20002020 <- rbind(ACS_20002020, finaldata_2020)

write.csv(ACS_20002020, "SIMAH_workplace/demography/ACS_popcounts_2000_2020_predicted2020.csv")

