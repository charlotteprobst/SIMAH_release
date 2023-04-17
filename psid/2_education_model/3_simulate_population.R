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
library(RColorBrewer)
library(ggalluvial)

# setwd("/home/cbuckley")
setwd("~/Google Drive/SIMAH Sheffield/")
# setwd("C:/Users/cmp21seb/Documents/SIMAH/")

# data <- read_csv("SIMAH_workplace/education_transitions/new_PSID_weighted_IDs.csv") # this file doesn't seem to be used anywhere?

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
  mutate(incomecat = ifelse(incomequintile==1, "Lowest","Highest"))
  
population <- generate_population(TPs, 10000) # population starts all aged 16.

# need to add incomecat to simulate_population function
simulatedpop1999 <- simulate_population(population, TPs, "1999-2009") # only up to age 26 as only simulating 20 years
simulatedpop2009<- simulate_population(population, TPs, "2009-2019") # only up to age 26 as only simulating 20 years

output <- rbind(simulatedpop1999, simulatedpop2009)

# by age 27 where have people ended up
# @ Sophie the code should work up to here but this code needs updating to the new version 
# 6 categories instead of 5 
# Including income as a breakdown
totals <- output %>% filter(age==18 | age==21 | age==24 | age==26) %>% 
  filter(age==26)  %>% # As nobody aged 27 has been generated, working with age 26 for now to test
  group_by(sex, racefinal, education, period, incomecat) %>% 
  tally(name="Npergroup") %>% ungroup() %>% group_by(sex, period, .drop=FALSE) %>% 
  mutate(TotalN = sum(Npergroup)) %>% ungroup() %>% group_by(sex, racefinal, period, incomecat, .drop=FALSE) %>% 
  pivot_wider(names_from=education, values_from=Npergroup) %>% group_by(sex, period, racefinal, incomecat) %>%
  dplyr::select(TotalN, sex, period, racefinal, incomecat, LHS, HS, SomeC1, SomeC2, SomeC3,College) %>% 
  mutate(sumLHS = sum(c_across(`LHS`:`College`), na.rm=T), # added additional LHS category
         sumHS = sum(c_across(`HS`:`College`), na.rm=T),
         sum1YR = sum(c_across(`SomeC1`:`College`), na.rm=T),
         sum2YR = sum(c_across(`SomeC2`:`College`), na.rm=T),
         sum3YR = sum(c_across(`SomeC3`:`College`), na.rm=T),
         sumCollege = `College`,
         percentLHS = sumLHS/TotalN,
         percentHS = sumHS/TotalN,
         percent1yr = sum1YR/TotalN,
         percent2yr = sum2YR/TotalN,
         percent3yr = sum3YR/TotalN,
         percentcollege = sumCollege/TotalN)

totals <- totals %>% select(sex, racefinal, period, incomecat, sumLHS, sumHS, sum1YR, sum2YR, sum3YR, sumCollege) %>% 
  pivot_longer(cols=c(sumLHS:sumCollege), names_to="education") %>% ungroup() %>% 
  mutate(education = ifelse(education=="sumLHS", "Less than High school diploma",
                        ifelse(education=="sumHS","High school diploma",
                            ifelse(education=="sum1YR", "One year of college",
                                   ifelse(education=="sum2YR", "Two years of college",
                                          ifelse(education=="sum3YR", "Three years of college",
                                                 ifelse(education=="sumCollege", "College degree or more", NA)))))),
         education= factor(education, levels=c("Less than High school diploma",
                                               "High school diploma",
                                               "One year of college",
                                               "Two years of college",
                                               "Three years of college",
                                               "College degree or more")),
         period = recode(period, "1"="Period 1 (1999-2009)", "2"="Period 2 (2011-2019)")) 
summary(as.factor(totals$racefinal))
totals$racefinal <- factor(totals$racefinal, levels=c("Native American","Others","Black","Hispanic","Asian/PI","White"))
# totals$race <- factor(totals$race, levels=c("Hispanic","others", "Black","White"))
totals$incomecat <- factor(totals$incomecat, levels=c("Lowest", "Highest"))

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

col.vec <-  c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")
display.brewer.pal(5, "Set2")
scale_fill_brewer(type="qual", palette="Set2")
totals$period <- as.factor(totals$period)
totals$sex <- as.factor(totals$sex)

# check alluvia form - correct 
is_alluvia_form(as.data.frame(totals), axes = 1:6, silent = TRUE)

# plot separately for men and women and then join together afterwards 
# code was breaking because it didn't know what to do with the "incomecat" column
plot1 <- ggplot(subset(totals,sex=="Men"), aes(x=education, stratum=racefinal, alluvium=racefinal, y=value,
                            fill=racefinal, label=racefinal)) + 
  scale_fill_manual(values=col.vec) + 
  geom_flow(stat="alluvium", lode.guidance="frontback", colour="darkgray") + theme_bw() +
  geom_stratum() + theme(legend.position="bottom",
                         legend.title=element_blank(),
                         strip.background = element_rect(colour="black", fill="white"),
                         text = element_text(size=22)) + 
  facet_grid(cols=vars(incomecat), rows=vars(period), scales="free") + ylab("Total population") + 
  xlab("Educational Attainment by age 26") +
  scale_x_discrete(breaks=unique(totals$education),
                   labels=(c("Less than \nHigh school diploma",
                             "High school \ndiploma",
                             "One year \nof college",
                             "Two years \nof college",
                             "Three years \nof college",
                             "College degree \nor more"))) +
  scale_y_continuous(label=scales::comma)

# Data is not in a recognized alluvial form
plot1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
ggsave("SIMAH_workplace/education_transitions/Figure2_main.pdf", dpi = 300, width = 45, height = 30, units = "cm")

