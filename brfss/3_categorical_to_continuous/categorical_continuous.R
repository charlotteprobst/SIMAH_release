# script for SIMAH project converting between categorical and continuous GPD 
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(tidyverse)
library(MASS)
library(magrittr)
library(caTools)
library(party)

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

# lambda <- 0.129 #lambda for transforming consumption taken from M.Strong / D.Moyo script

# optimise lambda for transforming alcohol consumption
# first group by sex and alcohol category - should be 6 groups
dat <- dat %>% mutate(group = paste0(AlcCAT, "_", sex_recode))

lambdas <- list()
for(i in unique(dat$sex_recode)){
  data <- dat %>% filter(sex_recode==i)
  b <- boxcox(lm(data$gramsperday ~ 1))
  lambda <- b$x[which.max(b$y)]
  newdata <- data.frame(sex_recode=unique(data$sex_recode), lambda = lambda)
  lambdas[[i]] <- newdata
}
lambdas <- do.call(rbind,lambdas)


# now join group lambdas to the individual level data for transformation
dat <- left_join(dat, lambdas)

dat <- dat %>% 
  filter(gramsperday!=0) %>% #remove non drinkers - don't want to model them 
  mutate(transcons = (gramsperday ^ lambda - 1) / lambda) %>% #transform consumption - following M Strong approach
  mutate(sex_recode = factor(sex_recode), education_summary = factor(education_summary),
         employment = factor(employment), race_eth = factor(race_eth), 
         AlcCAT = factor(AlcCAT))

# plot the transformed consumption by group 
ggplot(data=dat, aes(x=transcons)) + facet_grid(cols=vars(sex_recode), scales="free") + 
  geom_histogram()
  
# specify model - following M Strong approach with variables available in microsimulation and including race and ethnicity 
models <- list()

for(i in unique(dat$sex_recode)){
  data <- dat %>% filter(sex_recode==i)
  models[[i]] <- lm(transcons ~ age_var + I(age_var^2) + I(age_var^3)  + YEAR + race_eth + 
                      AlcCAT + 
            education_summary  + age_var + age_var*education_summary + 
            race_eth*age_var*education_summary*AlcCAT, data=data)
}

# attach the coefficients back into the dataframe - for prediction
back.tran <- function(x){(lambda * x + 1) ^ (1/lambda)}

# see how the model performs on a given years data

test_model <- function(dat,year,selectedgroup){
year <- dat %>% filter(YEAR==year) %>% assign_alc_cat() %>% 
  filter(gramsperday!=0) %>% #remove non drinkers - don't want to model them 
  mutate(transcons = (gramsperday ^ lambda - 1) / lambda) %>% #transform consumption - following M Strong approach
  mutate(sex_recode = factor(sex_recode), education_summary = factor(education_summary),
         employment = factor(employment), race_eth = factor(race_eth), 
         AlcCAT = factor(AlcCAT)) %>% 
  mutate(oldgpd = gramsperday, oldAlcCAT = AlcCAT) %>% 
  filter(sex_recode==selectedgroup)

# predict grams per day using model
year$gramsperday <- back.tran(predict(models[[selectedgroup]], year))

# assign alc category based on new grams per day 
year <- assign_alc_cat(year)

# look at the proportion of these correctly assigned - and whether there are systematic differences in
# model performance based on socio demographic groups
proportion <- year %>% mutate(correct = ifelse(oldAlcCAT == AlcCAT, 1,0)) %>% 
  group_by(sex_recode, education_summary, race_eth, correct) %>% tally() %>% ungroup() %>% 
  group_by(sex_recode,education_summary, race_eth) %>% mutate(prop = n/sum(n)) %>% 
  filter(correct==1)
# accuracy with classifying the alcohol categories

cor <- cor.test(year$gramsperday, year$oldgpd)
list <- list(proportion, cor)
# correlation with old and new gpd on training data set 
return(list)
}

proportion <- test_model(dat, 2010, "Male")[[1]]
test_model(dat, 2010, "Male")[[2]]

# model seems ok for now - save a copy for implementation in the microsimulation 
saveRDS(models, "SIMAH_workplace/microsim/1_input_data/SSCatContModel.RDS")

