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

data <- read_csv("SIMAH_workplace/education_transitions/new_PSID_weighted_IDs.csv")

source("SIMAH_code/psid/2_education_model/0_generate_population.R")

TPs <- read.csv("SIMAH_workplace/education_transitions/final_models/income_model_TP_6cat_16.csv") %>% 
  mutate(racefinal=ifelse(racefinal=="white","White",
                          ifelse(racefinal=="black","Black",
                                 ifelse(racefinal=="hispanic","Hispanic",
                                        ifelse(racefinal=="other","Others",
                                               ifelse(racefinal=="Asian/PI","Others",
                                                      racefinal))))),
         racefinal = factor(racefinal,
                            levels=c("Black","Hispanic","White","AsianPI","Others")),
         sex = ifelse(sex=="male","Men","Women")) %>% filter(incomequintile==1 | incomequintile==5) %>% 
  mutate(incomecat = ifelse(incomequintile==1, "Lowest income quintile","Highest income quintile"))
  
population <- generate_population(TPs, 1000)

simulatedpop1999 <- simulate_population(population, TPs, "1999-2009")
simulatedpop2009<- simulate_population(population, TPs, "2009-2019")

output <- rbind(simulatedpop1999, simulatedpop2009)

# by age 27 where have people ended up
# @ Sophie the code should work up to here but this code needs updating to the new version 
# 6 categories instead of 5 
# Including income as a breakdown
totals <- output %>% filter(age==18 | age==21 | age==24 | age==27) %>% 
  filter(age==27)  %>% 
  group_by(sex, racefinal, education, period) %>% 
  tally(name="Npergroup") %>% ungroup() %>% group_by(sex, period, .drop=FALSE) %>% 
  mutate(TotalN = sum(Npergroup)) %>% ungroup() %>% group_by(sex, racefinal, period, .drop=FALSE) %>% 
  pivot_wider(names_from=education, values_from=Npergroup) %>% group_by(sex, period, racefinal) %>% 
  mutate(sumHS = sum(c_across(`High school diploma or less`:`College degree or more`), na.rm=T),
         sum1YR = sum(c_across(`1 year college`:`College degree or more`), na.rm=T),
         sum2YR = sum(c_across(`2 year college`:`College degree or more`), na.rm=T),
         sum3YR = sum(c_across(`3 year college`:`College degree or more`), na.rm=T),
         sumCollege = `College degree or more`,
         percentHS = sumHS/TotalN,
         percent1yr = sum1YR/TotalN,
         percent2yr = sum2YR/TotalN,
         percent3yr = sum3YR/TotalN,
         percentcollege = sumCollege/TotalN)

totals <- totals %>% select(sex, race, period, sumHS, sum1YR, sum2YR, sum3YR, sumCollege) %>% 
  pivot_longer(cols=c(sumHS:sumCollege), names_to="education") %>% ungroup() %>% 
  mutate(education = ifelse(education=="sumHS","High school diploma or less",
                            ifelse(education=="sum1YR", "One year of college",
                                   ifelse(education=="sum2YR", "Two years of college",
                                          ifelse(education=="sum3YR", "Three years of college",
                                                 ifelse(education=="sumCollege", "College degree or more", NA))))),
         education= factor(education, levels=c("High school diploma or less",
                                               "One year of college",
                                               "Two years of college",
                                               "Three years of college",
                                               "College degree or more")),
         period = recode(period, "1"="Period 1 (1999-2009)", "2"="Period 2 (2011-2019)"))
summary(totals$race)
totals$race <- factor(totals$race, levels=c("Native American","Others","Black","Hispanic","Asian/PI","White"))
# totals$race <- factor(totals$race, levels=c("Hispanic","others", "Black","White"))

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

col.vec <-  c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")
display.brewer.pal(5, "Set2")
scale_fill_brewer(type="qual", palette="Set2")

plot1 <- ggplot(totals, aes(x=education, stratum=race, alluvium=race, y=value,
                            fill=race, label=race)) + 
  scale_fill_manual(values=col.vec) + 
  geom_flow(stat="alluvium", lode.guidance="frontback", colour="darkgray") + theme_bw() +
  geom_stratum() + theme(legend.position="bottom",
                         legend.title=element_blank(),
                         strip.background = element_rect(colour="black", fill="white"),
                         text = element_text(size=22)) + 
  facet_grid(cols=vars(sex),rows=vars(period), scales="free") + ylab("Total population") + 
  xlab("Educational Attainment by age 27") +
  scale_x_discrete(breaks=unique(totals$education),
                   labels=(c("High school \ndiploma or less",
                             "One year \nof college",
                             "Two years \nof college",
                             "Three years \nof college",
                             "College degree \nor more"))) +
  scale_y_continuous(label=scales::comma)


plot1

ggsave("SIMAH_workplace/education_transitions/Figure2_main.pdf", dpi = 300, width = 45, height = 30, units = "cm")

