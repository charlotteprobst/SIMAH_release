# script to extract summary tables for education for education transitions paper
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(readxl)
library(doParallel)
library(foreach)
library(parallel)
library(ggplot2)

# setwd("/home/cbuckley")
setwd("~/Google Drive/SIMAH Sheffield")

# setwd Sophie
setwd("C:/Users/cmp21seb/Documents/SIMAH/")

TPs <- read.csv("SIMAH_workplace/education_transitions/final_models/income_model_TP_6cat_16_new.csv") %>% 
  mutate(racefinal=ifelse(racefinal=="white","White",
                          ifelse(racefinal=="black","Black",
                                 ifelse(racefinal=="hispanic","Hispanic",
                                        ifelse(racefinal=="other","Others",racefinal)))),
         racefinal = factor(racefinal,
                            levels=c("Black","Hispanic","White","Others")),
         sex = ifelse(sex=="male","Men","Women"),
         incomequintile = paste0("Income quintile ", incomequintile)) %>% filter(incomequintile=="Income quintile 1" | incomequintile=="Income quintile 5") %>% 
  mutate(incomecat = ifelse(incomequintile=="Income quintile 1", "Lowest income quintile","Highest income quintile"))

# Transition LHS to HS at age 18
ggplot(data=subset(TPs, age==18 & Transition=="HS->SomeC1"), aes(x=time, y=prob, fill=as.factor(incomecat))) + 
  geom_bar(stat="identity", position="dodge", colour="black") + facet_grid(cols=vars(racefinal), rows=vars(sex)) + theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        strip.background=element_rect(fill="white",colour="black")) + xlab("Race and ethnicity") + ylab("Probability") + 
  ggtitle("High school or less -> 1 year college at age 18")

# Transition HS to some college
ggplot(data=subset(TPs, age==18 & Transition=="HS->SomeC1"), aes(x=time, y=prob, fill=as.factor(incomecat))) + 
  geom_bar(stat="identity", position="dodge", colour="black") + facet_grid(cols=vars(racefinal), rows=vars(sex)) + theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        strip.background=element_rect(fill="white",colour="black")) + xlab("Race and ethnicity") + ylab("Probability") + 
  ggtitle("High school or less -> 1 year college at age 18")

# Transition some college to 4+ years college
ggplot(data=subset(TPs, age==22 & Transition=="SomeC3->College"), aes(x=time, y=prob, fill=as.factor(incomecat))) + 
  geom_bar(stat="identity", position="dodge", colour="black") + facet_grid(cols=vars(racefinal), rows=vars(sex)) + theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        strip.background=element_rect(fill="white",colour="black")) + xlab("Race and ethnicity") + ylab("Probability") + 
  ggtitle("3 years college -> College graduate at age 22")

# most common pathway for each group
commonpathways <- TPs %>% 
  group_by(age, sex, racefinal, incomecat, time, StateFrom) %>% 
  mutate(filterout= ifelse(Transition1==Transition2,1,0)) %>% 
  filter(filterout==0) %>% 
  mutate(max = max(prob),
            selected = ifelse(max == prob, 1,0)) %>% filter(selected==1)

