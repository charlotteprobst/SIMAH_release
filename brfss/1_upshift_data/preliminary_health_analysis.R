library(foreign)
library(SASxport)
library(readr)
library(dplyr)
library(tidyr)
library(labelled)
library(sjlabelled)
library(tidyverse)

# CB laptop directory
wd <- "~/Google Drive/SIMAH Sheffield/"
# CB desktop directory
# wd <- "G:/My Drive/SIMAH Sheffield/"
setwd(wd)

####read in the joined up data files 
data <- readRDS("SIMAH_workplace/brfss/processed_data/BRFSS_reweighted_upshifted_1984_2020.RDS")

USA <- data %>% filter(State=="USA")

summarystats <- USA %>% mutate(gpd_cat = cut(gramsperday_upshifted_crquotient,
                                             breaks=c(-1,0,20,40,60,200),
                                             labels=c("non drinker","1-20grams","21-40grams","41-60grams","61+grams"))) %>% 
group_by(YEAR, sex_recode, education_summary,gpd_cat) %>% 
  summarise(meanmentalhealthdays = mean(mentalhealth,na.rm=T),
            meanphysicalhealthdays = mean(physicalhealth, na.rm=T),
            meanHED = mean(hed)) %>% drop_na()
  # mutate(drinkingstatus_updated = recode(drinkingstatus_updated, "0"="non-drinker","1"="drinker"))

ggplot(data=summarystats, aes(x=YEAR, y=meanmentalhealthdays, colour=as.factor(education_summary))) + 
  geom_line() + 
  facet_grid(cols=vars(hed_cat), rows=vars(sex_recode)) +
  theme(legend.title=element_blank())


USAdropmissing <- USA %>% drop_na(mentalhealth)
USAdropmissing$period <- USAdropmissing$YEAR-min(USAdropmissing$YEAR)
USAdropmissing$anyhed <- ifelse(USAdropmissing$hed==0, 0, 1)

ggplot(data=USAdropmissing, aes(x=gramsperday_upshifted_crquotient, 
                                y=mentalhealth, colour=as.factor(education_summary))) + 
  geom_point() + 
  facet_grid(rows=vars(sex_recode)) +
  theme(legend.title=element_blank())

correlations <- USAdropmissing %>% group_by(YEAR, sex_recode, education_summary) %>% 
  summarise(correlation = cor(mentalhealth, hed))

ggplot(data=correlations, aes(x=YEAR, y=correlation, colour=education_summary)) + geom_line() + 
  facet_grid(cols=vars(sex_recode))

USAdropmissing <- USAdropmissing %>% filter(drinkingstatus_updated==1)

modelmen <- lm(gramsperday_upshifted_crquotient ~period + education_summary + 
                 education_summary*mentalhealth*period, 
               data=subset(USAdropmissing,sex_recode=="Male"))
summary(modelmen)
plot_model(modelmen, type=c("pred"), terms=c("mentalhealth","education_summary","period"))


modelmen <- lm(hed ~period + education_summary + 
                 education_summary*mentalhealth*period, 
               data=subset(USAdropmissing,sex_recode=="Male"))
summary(modelmen)
plot_model(modelmen, type=c("pred"), terms=c("mentalhealth","education_summary","period"))

modelmen <- glm(anyhed ~period + education_summary + 
                 education_summary*mentalhealth*period, 
               data=subset(USAdropmissing,sex_recode=="Male"))
summary(modelmen)
plot_model(modelmen, type=c("pred"), terms=c("mentalhealth","education_summary","period"))

modelwomen <- lm(gramsperday_upshifted_crquotient~period + education_summary + mentalhealth + education_summary*mentalhealth*period, 
               data=subset(USAdropmissing,sex_recode=="Female"))
summary(modelwomen)

modelwomen <- lm(hed~period + education_summary + mentalhealth + education_summary*mentalhealth*period, 
                 data=subset(USAdropmissing,sex_recode=="Female"))
summary(modelwomen)

plot_model(modelwomen, type=c("pred"), terms=c("mentalhealth","education_summary","period"))
