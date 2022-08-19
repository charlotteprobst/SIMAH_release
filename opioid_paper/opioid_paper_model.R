# SIMAH project October 2021 
# plots for drawing opioid and alcohol mortality rates for publication

library(dplyr)
library(tidyr)
library(foreign)
library(haven)
library(ggplot2)
library(sjlabelled)
library(RColorBrewer)

# github demonstration

wd <- "~/Google Drive/SIMAH Sheffield/"
setwd(wd)

# read in datafiles 
data <- read_dta("SIMAH_workplace/opioid_paper/poisoningdata/poison-gender-race-education-StandardRates-25plus-00-20.dta")
summary(data$alc_only_fin_StdRate)
summary(data$opioid_only_fin_StdRate)
summary(data$alc_opioid_fin_StdRate)

data$log_alc_only <- log(1+data$alc_only_fin_StdRate)
data$log_op_only <- log(1+data$opioid_only_fin_StdRate)
data$log_alc_op <- log(1+data$alc_opioid_fin_StdRate)

data$yearsq <- data$year^2

data <- data %>% filter(year<=2019)

data$yearcen <- scale(data$year, center=T, scale=T)

test <- unique(data.frame(data$year, data$yearcen))


data$yearcen <- data$year-2010
data$edclass <- as.factor(data$edclass)
data$edclass <- relevel(data$edclass, ref="3")

men <- data %>% filter(sex==1)


model <- gls(opioid_only_fin_StdRate ~ yearcen+yearcen*as.factor(race)+
               yearcen*as.factor(edclass), data=data)
summary(model)

model <- gls(log_op_only ~ yearcen+yearcen*as.factor(race)+
               yearcen*as.factor(edclass), data=data,
             method="REML")
summary(model)
AIC(model)


summary(model)
confint(model)
names(data)
