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

# read in ACS data 
ACS_raw <- read.csv("SIMAH_workplace/ACS/ACS_popcounts_2000_2021_updated.csv") %>% 
  group_by(year, sex, edclass, race) %>% 
  summarise(total = sum(TPop),
            type="ACS")

ACS_predicted <- read.csv("SIMAH_workplace/demography/ACS_popcounts_predicted2021.csv") %>% 
  group_by(year,sex,edclass,race) %>% 
  summarise(total=sum(TPop),
            type="ACS predicted")

CPS <- read.csv("SIMAH_workplace/CPS/CPS_2000_2021_agegp.csv") %>% 
  group_by(year,sex,edclass,race) %>% 
  summarise(total=sum(TPop),
            type="CPS")

overall <- rbind(ACS_raw, ACS_predicted, CPS) %>% 
  mutate(sex = ifelse(sex==1, "Men","Women"),
         edclass = case_when(edclass=="LEHS" ~ "Low",
                             edclass=="SomeC" ~ "Middle",
                             edclass=="College" ~ "High"),
         edclass = factor(edclass, levels=c("Low","Middle","High")),
         total = total/100000,
         cat = paste0(sex, " - ", type),
         cat = factor(cat, 
                      levels=c("Men - ACS", "Women - ACS",
                               "Men - ACS predicted", "Women - ACS predicted",
                               "Men - CPS" , "Women - CPS")))

ggplot(data=subset(overall), aes(x=year, y=total, colour=as.factor(cat), linetype=as.factor(cat))) + 
  geom_line(size=1) + facet_grid(cols=vars(edclass), rows=vars(race), scales="free") +
  xlim(2015,2020) + theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        strip.background = element_rect(fill="white")) + 
  scale_x_continuous(breaks=c(2015:2021), limits=c(2015,2021)) + 
  scale_linetype_manual(values=c("solid","solid","dotted","dotted","solid","solid")) +
  scale_colour_manual(values=c("#E41A1C","#377EB8","#E41A1C","#377EB8","#4DAF4A","#984EA3")) +
  ylab("Total population (per 100,000)") + 
  xlab("Year")

ggsave("SIMAH_workplace/demography/plots/popcounts_2021_ACSCPS.png",
       dpi=300, width=33, height=19, units="cm")

# create an alternative version
ggplot(data=subset(overall), aes(x=year, y=total, colour=as.factor(type), linetype=as.factor(type))) + 
  geom_line(size=1) + facet_grid(cols=vars(sex,edclass), rows=vars(race), scales="free") +
  xlim(2015,2020) + theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        strip.background = element_rect(fill="white"),
        text = element_text(size=12),
        panel.spacing = unit(1,"lines")) + 
  scale_x_continuous(breaks=c(2015,2017,2019,2021), limits=c(2015,2021)) + 
  scale_linetype_manual(values=c("solid","dotted","solid")) +
  scale_colour_manual(values=c("#E41A1C","#E41A1C","#377EB8")) +
  ylab("Total population (per 100,000)") + 
  xlab("Year")

ggsave("SIMAH_workplace/demography/plots/popcounts_2021_ACSCPS_alt.png",
       dpi=300, width=33, height=19, units="cm")
