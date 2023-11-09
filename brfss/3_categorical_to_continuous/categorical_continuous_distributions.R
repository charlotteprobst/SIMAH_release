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

# saveRDS(dat, "SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_2000_2020_USA.RDS")

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

dat <- dat %>% mutate(group = paste(AlcCAT, race_eth, education_summary, sex_recode, sep="_"))

fitdistribution_gamma <- function(data, group){
  groupdata <- dat %>% filter(group==i)
  distribution <- fitdist(groupdata$gramsperday_upshifted, distr="gamma")
  shape <- distribution$estimate[[1]]
  rate <- distribution$estimate[[2]]
  dist <- data.frame(group=i, shape=shape, rate=rate)
  return(dist)
}

distributions <- list()

for(i in unique(dat$group)){
  distributions[[paste(i)]] <- fitdistribution_gamma(dat, i)
}

distribution <- do.call(rbind, distributions)

# explore beta distribution - first normalise between 0 and 1 (store the method so it is reversible)
dat <- dat %>% 
  group_by(group) %>% 
  mutate(min = min(gramsperday_upshifted),
         max = max(gramsperday_upshifted))

fitdistribution_beta <- function(data, group){
  groupdata <- dat %>% filter(group==i) %>% 
    mutate(scaled = ((gramsperday_upshifted - min) + 10e-10) / ((max - min) + 10e-9))
  distribution <- fitdist(groupdata$scaled, distr="beta")
  shape1 <- distribution$estimate[[1]]
  shape2 <- distribution$estimate[[2]]
  dist <- data.frame(group=i, shape1=shape1, shape2=shape2, min=unique(groupdata$min), 
                     max=unique(groupdata$max))
  return(dist)
}

distributions <- list()

for(i in unique(dat$group)){
  distributions[[paste(i)]] <- fitdistribution_beta(dat, i)
}

distribution <- do.call(rbind, distributions)
  
# 
# dat <- left_join(dat, distribution)
# 
# dat <- dat %>% group_by(group) %>% mutate(min=min(gramsperday_upshifted), max=max(gramsperday_upshifted))
# 
# samplegpd <- function(data){
#   shape <- unique(data$shape)
#   rate <- unique(data$rate)
#   # min <- unique(data$min)
#   # max <- unique(data$max)
#   # data$newgpd <- rtrunc(nrow(data), "gamma", min, max, shape=shape, scale=rate)
#   data$newgpd <- rgamma(nrow(data), shape, rate)
#   return(data)
# }
# 
# dat <- dat %>% group_by(group) %>% do(samplegpd(.))

write.csv(distribution, "SIMAH_workplace/microsim/1_input_data/CatContDistr_beta.csv", row.names=F)


