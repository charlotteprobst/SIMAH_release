# script for SIMAH project converting between categorical and continuous GPD 
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(tidyverse)
library(MASS)
library(magrittr)
library(caTools)
library(party)
library(fitdistrplus)

# CB laptop directory
wd <- "~/Google Drive/SIMAH Sheffield/"
setwd(wd)
# read data
dat <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_2000_2020_final.RDS") %>% filter(YEAR>=2000) %>% 
  filter(State=="USA")

assign_alc_cat <- function(data){
  data <- data %>% mutate(AlcCAT = ifelse(sex_recode=="Male" & gramsperday_upshifted>0 &
                                            gramsperday_upshifted<=40, "Low risk",
                                          ifelse(sex_recode=="Female" & gramsperday_upshifted>0 &
                                                   gramsperday_upshifted<=20, "Low risk",
                                                 ifelse(sex_recode=="Male" & gramsperday_upshifted>40 &
                                                          gramsperday_upshifted<=60, "Medium risk",
                                                        ifelse(sex_recode=="Female" & gramsperday_upshifted>20 &
                                                                 gramsperday_upshifted<=40, "Medium risk",
                                                               ifelse(sex_recode=="Male" & gramsperday_upshifted>60,
                                                                      "High risk",
                                                                      ifelse(sex_recode=="Female" & gramsperday_upshifted>40,
                                                                             "High risk", NA)))))))
  return(data)
}

dat <- dat %>% assign_alc_cat() 

dat <- dat %>% filter(gramsperday_upshifted>0)

dat <- dat %>% mutate(agecat = cut(age_var,
                                   breaks=c(0,24,64,100),
                                   labels=c("18-24","25-64","65+")))

dat <- dat %>% mutate(education_summary = ifelse(agecat=="18-24" & education_summary=="College","SomeC",
                                                 education_summary))

dat <- dat %>% mutate(yearcat = cut(YEAR,
                                    breaks=c(0,2010,2022),
                                    labels=c("2000-2010",
                                             "2011+")))
unique(dat$yearcat)
dat <- dat %>% mutate(group = paste(AlcCAT, education_summary, agecat, race_eth, sex_recode, sep="_"))
dat <- dat %>% filter(gramsperday_upshifted<200)

dat <- dat %>% 
  group_by(group) %>% 
  mutate(min = min(gramsperday_upshifted),
         max = max(gramsperday_upshifted))

fitdistribution_beta <- function(data, group){
  groupdata <- dat %>% filter(group==i) %>% 
    mutate(scaled = ((gramsperday_upshifted - min) + 10e-10) / ((max - min) + 10e-9))
  alccat <- unique(groupdata$AlcCAT)
  agecat <- unique(groupdata$agecat)
  race_eth <- unique(groupdata$race_eth)
  education_summary <- unique(groupdata$education_summary)
  sex_recode <- unique(groupdata$sex_recode)
  year_cat <- unique(groupdata$yearcat)
  distribution <- fitdist(groupdata$scaled, distr="beta")
  shape1 <- distribution$estimate[[1]]
  shape2 <- distribution$estimate[[2]]
  dist <- data.frame(group=i, AlcCAT=alccat, agecat=agecat, 
                     # race_eth=race_eth, 
                     education_summary = education_summary,
                     sex_recode = sex_recode, 
                     # year_cat = year_cat,
                     shape1=shape1, shape2=shape2, min=unique(groupdata$min), 
                     max=unique(groupdata$max))
  return(dist)
}

distributions <- list()

for(i in unique(dat$group)){
  distributions[[paste(i)]] <- fitdistribution_beta(dat, i)
}

distribution <- do.call(rbind, distributions)

write.csv(distribution, "SIMAH_workplace/microsim/1_input_data/CatContDistr_beta.csv", row.names=F)
