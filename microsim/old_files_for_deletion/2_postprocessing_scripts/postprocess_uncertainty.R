# postprocessing script for uncertainty estimations 

# first read in data 
library(dplyr)
library(tidyr)

# calculate proportion of individuals in each sex/race/education category over time 
PopSummary <- readRDS("output_data/PopSummary100.RDS")
for(i in names(PopSummary)){
  PopSummary[[paste(i)]] <- PopSummary[[paste(i)]] %>% 
    group_by(microsim.init.sex, microsim.init.education, year, Sample) %>% 
    summarise(sum=sum(n)) %>% ungroup() %>% group_by(microsim.init.sex, year, Sample) %>% 
    mutate(percent=sum/sum(sum)) %>% mutate(year=as.integer(year),
                                            Sample=as.factor(Sample),
                                            datatype="Microsimulation",
                                            microsim.init.sex=ifelse(microsim.init.sex=="f","Female","Male")) %>% data.frame(.)
}

PopSummary <- do.call(rbind,PopSummary)

library(ggplot2)
ggplot(data=PopSummary, aes(x=year, y=percent, colour=Sample)) + geom_line() +
  facet_grid(rows=vars(microsim.init.education), cols=vars(microsim.init.sex)) +
  theme(legend.position = "none") + ylim(0,1) + theme_bw() + theme(legend.position="none")

# add lines for target data from PSID, ACS and Census
source("postprocessing_scripts/process_education_compare.R")

summary <- summary %>% group_by(datatype, year, microsim.init.sex, microsim.init.education) %>%
  summarise(n=sum(n)) %>% ungroup() %>% group_by(datatype, year, microsim.init.sex) %>%
  mutate(percent=n/sum(n)) %>% data.frame(.) %>% select(-n) %>% mutate(min=NA, max=NA)

uncertainty <- PopSummary %>% group_by(year, microsim.init.sex, microsim.init.education) %>% 
  summarise(min=min(percent),
            max=max(percent),
            percent=mean(percent)) %>% data.frame(.) %>% mutate(datatype="Microsimulation")

setdiff(names(summary), names(uncertainty))
setdiff(names(uncertainty), names(summary))

uncertainty <- rbind(summary, uncertainty)

uncertainty <- uncertainty %>% mutate(microsim.init.education = ifelse(microsim.init.education=="LEHS","High school diploma or less",
                                                                       ifelse(microsim.init.education=="SomeC", "Some college",
                                                                              "College degree or more")))
uncertainty <- uncertainty %>% filter(year<=2018)

write.csv(uncertainty, "output_data/uncertainty_estimates100sex.csv", row.names=FALSE)

# for RACE breakdown 
PopSummary <- readRDS("output_data/PopSummary100.RDS")
for(i in names(PopSummary)){
  PopSummary[[paste(i)]] <- PopSummary[[paste(i)]] %>% 
    group_by(microsim.init.race, microsim.init.education, year, Sample) %>% 
    summarise(sum=sum(n)) %>% ungroup() %>% group_by(microsim.init.race, year, Sample) %>% 
    mutate(percent=sum/sum(sum)) %>% mutate(year=as.integer(year),
                                            Sample=as.factor(Sample),
                                            datatype="Microsimulation",
                                            microsim.init.sex=ifelse(microsim.init.race=="BLA",
                                                                     "Non-Hispanic Black",
                                                                     ifelse(microsim.init.race=="WHI",
                                                                            "Non-Hispanic White",
                                                                            ifelse(microsim.init.race=="OTH",
                                                                                   "Non-Hispanic Other",
                                                                                   ifelse(microsim.init.race=="SPA",
                                                                                          "Hispanic",NA))))) %>% data.frame(.)
}

PopSummary <- do.call(rbind,PopSummary)

# source("postprocessing_scripts/process_education_compare.R")

summary <- read.csv("output_data/summary_education_compare.csv")

summary <- summary %>% group_by(datatype, year, microsim.init.race, microsim.init.education) %>%
  summarise(n=sum(n)) %>% ungroup() %>% group_by(datatype, year, microsim.init.race) %>%
  mutate(percent=n/sum(n)) %>% data.frame(.) %>% select(-n) %>% mutate(min=NA, max=NA) %>% 
  mutate(microsim.init.race = recode(microsim.init.race,
         "BLA"="Non-Hispanic Black",
         "WHI"="Non-Hispanic White",
         "OTH"="Non-Hispanic Other",
         "SPA"="Hispanic"))

uncertainty <- PopSummary %>% group_by(year, microsim.init.race, microsim.init.education) %>% 
  summarise(
    min=min(percent),
            max=max(percent),
            percent=mean(percent)) %>% data.frame(.) %>% mutate(datatype="Microsimulation")

setdiff(names(summary), names(uncertainty))
setdiff(names(uncertainty), names(summary))

uncertainty <- rbind(summary, uncertainty)

uncertainty <- uncertainty %>% mutate(microsim.init.education = ifelse(microsim.init.education=="LEHS","High school diploma or less",
                                                                       ifelse(microsim.init.education=="SomeC", "Some college",
                                                                              "College degree or more")))
uncertainty <- uncertainty %>% filter(year<=2018)

write.csv(uncertainty, "output_data/uncertainty_estimates100race.csv", row.names=FALSE)
