#Alcohol use disorders dose-response meta analysis draft 
#(alcohol use -> AUD incidence)

# Load libraries 
library(tidyverse)  # data management
library(readxl)     # import excel data
library(janitor)    # Edit data formatting
library(skimr)      # descriptive statistics
library(tableone)   # create table one
library(data.table) # enable data table

library(dosresmeta)
library(mvtnorm)
library(ellipse)
library(mvmeta)
library(rms)
library(meta)
library(metafor)
library(rmeta)

# Date
DATE <- 20221125

#Name file location and output accordingly
file_location <- "/Users/tessacarr/Downloads/Data Extraction/"
output <- "/Users/tessacarr/Downloads/Data Extraction/"

# Load data
data <- read_xlsx(paste0(file_location, "Analysis_data_AUD_incidence.xlsx"), na="")

data1 <- data %>%
  mutate(
    logRR = log(RR),
    loglowerRR = log(lowerRR),
    logupperRR = log(upperRR),
    se = ifelse(upperRR!=1 & lowerRR!=1, (logupperRR - loglowerRR)/3.92, NA),
    inver_se = 1/se) %>% as.data.table

data_all <- data1 %>%
  filter(group == "All participants")

#Create scatter plot
ggplot(data_all, aes(alc_daily_g, RR, size=inver_se)) + 
  scale_y_continuous(trans = log2_trans()) +
  geom_point(shape=1, colour="black") + scale_size_area(max_size=20)

#Create linear model
lin_mod <- dosresmeta(formula=logRR ~ alc_daily_g, proc="1stage",
                      id=id_study, type="ir", se=se, cases=outcome_n, n=total_n, data=data_all)

summary(lin_mod)
predict(lin_mod, delta=10, exp=TRUE)

#Plot
predict(lin_mod, data.frame(alc_daily_g=seq(0, 100, 1)), order=TRUE, exp=TRUE) %>% 
  ggplot(aes(x=alc_daily_g, y=pred)) + 
  #scale_y_continuous(trans = log2_trans()) +
  geom_line() + 
  geom_ribbon(aes(ymin= ci.lb, ymax=ci.ub), alpha=0.1) + 
  coord_cartesian(ylim=c(-5, 100))+
  theme_bw()

#Create quadratic model(model being used)
quad_mod <- dosresmeta(formula=logRR ~ alc_daily_g + I(alc_daily_g^2), proc= "1stage", 
                       id=id_study, type="ir", se=se, cases=outcome_n, n=total_n, data=data_all)

summary(quad_mod)
predict(quad_mod, data.frame(alc_daily_g=seq(0, 100, 10)), exp=TRUE)

#Plot
predict(quad_mod, data.frame(alc_daily_g=seq(0, 100, 1)), order=TRUE, exp=TRUE) %>% 
  ggplot(aes(x=alc_daily_g, y=pred)) + 
  labs(y= "Relative risk of AUD incidence", x = "Alcohol consumption (g/day)") +
  #scale_y_continuous(trans = log2_trans()) + #to log the y axis
  geom_line(colour = "black", linetype=1) + 
  geom_ribbon(aes(ymin= ci.lb, ymax=ci.ub), alpha=0.1, colour = "black", linetype = "dotted") + #add , fill = "" to change the colour of the CI
  coord_cartesian(ylim=c(-5, 100))+
  theme_classic() +
  geom_point(data_all, mapping=aes(alc_daily_g, RR, size=inver_se), shape=1, 
           colour="black", alpha = 0.5) +  #alpha changes the opacity of the circle
           scale_size(range = c (5, 10), name = "Studies (inverse SE)")

ggsave("/Users/tessacarr/Downloads/AUD Analysis/Figure1_dose_response_nolog_AUDincidence.jpeg", device = "jpeg", dpi = 600,
       width = 15, height = 10, units = "cm")
