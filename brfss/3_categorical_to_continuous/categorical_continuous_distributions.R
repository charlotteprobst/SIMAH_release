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
dat <- readRDS("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_1984_2020_final.RDS") %>% filter(YEAR>=2000) %>% 
  filter(drinkingstatus==1) %>% filter(State=="USA")

assign_alc_cat <- function(data){
  data <- data %>% mutate(AlcCAT = ifelse(sex_recode=="Male" & gramsperday>0 &
                                            gramsperday<=40, "Low risk",
                                          ifelse(sex_recode=="Female" & gramsperday>0 &
                                                   gramsperday<=20, "Low risk",
                                                 ifelse(sex_recode=="Male" & gramsperday>40 &
                                                          gramsperday<=60, "Medium risk",
                                                        ifelse(sex_recode=="Female" & gramsperday>20 &
                                                                 gramsperday<=40, "Medium risk",
                                                               ifelse(sex_recode=="Male" & gramsperday>60,
                                                                      "High risk",
                                                                      ifelse(sex_recode=="Female" & gramsperday>40,
                                                                             "High risk", NA)))))))
  return(data)
}

dat <- dat %>% assign_alc_cat() 

dat <- dat %>% mutate(group = paste(AlcCAT, race_eth, education_summary, sex_recode, sep="_"))

# highrisk <- ggplot(data=subset(dat, AlcCAT=="High risk"), aes(x=gramsperday)) +
#   geom_density() +
#   geom_vline(aes(xintercept=median), linetype="dashed") +  
#   facet_grid(rows=vars(sex_recode), scales="free_x") +
#   theme(legend.position="bottom")
# highrisk
# 
# mediumrisk <- ggplot(data=subset(dat, AlcCAT=="Medium risk"), aes(x=gramsperday)) +
#   geom_density() +
#   geom_vline(aes(xintercept=median), linetype="dashed") +  
#   facet_grid(rows=vars(sex_recode), scales="free_x") +
#   theme(legend.position="bottom")
# mediumrisk
# 
# lowrisk <- ggplot(data=subset(dat, AlcCAT=="Low risk"), aes(x=gramsperday)) +
#   geom_density() +
#   geom_vline(aes(xintercept=median), linetype="dashed") +  
#   facet_grid(rows=vars(sex_recode), scales="free_x") +
#   theme(legend.position="bottom")
# lowrisk

fitdistribution <- function(data, group){
  groupdata <- dat %>% filter(group==i)
  distribution <- fitdist(groupdata$gramsperday, distr="gamma")
  shape <- distribution$estimate[[1]]
  rate <- distribution$estimate[[2]]
  dist <- data.frame(group=i, shape=shape, rate=rate)
  return(dist)
}

distributions <- list()

for(i in unique(dat$group)){
  distributions[[paste(i)]] <- fitdistribution(dat, i)
}

distribution <- do.call(rbind, distributions)

dat <- left_join(dat, distribution)

dat <- dat %>% group_by(group) %>% mutate(min=min(gramsperday), max=max(gramsperday))

samplegpd <- function(data){
  shape <- unique(data$shape)
  rate <- unique(data$rate)
  # min <- unique(data$min)
  # max <- unique(data$max)
  # data$newgpd <- rtrunc(nrow(data), "gamma", min, max, shape=shape, scale=rate)
  data$newgpd <- rgamma(nrow(data), shape, rate)
  return(data)
}

dat <- dat %>% group_by(group) %>% do(samplegpd(.))

write.csv(distribution, "SIMAH_workplace/microsim/1_input_data/CatContDistr.csv", row.names=F)


