#Alcohol use disorders dose-response meta analysis draft

# Load libraries 
library(tidyverse)  # data management
library(readxl)     # import excel data
library(janitor)    # Edit data formatting
library(skimr)      # descriptive statistics
library(tableone)   # create table one
library(dyplyr)
library(dosresmeta)
library(scales)
library(meta)
library(metafor)

#load libraries for DRMA, multivariate MA, and RCS
install.packages("dosresmeta")
install.packages("mvtnorm")
install.packages("ellipse")
install.packages("mvmeta")
install.packages("rms")


#load additional libraries for general intervention meta-analysis
install.packages("meta")
install.packages("metafor")
install.packages("rmeta")

#Load data, change file location and name of file accordingly
file_location <- "/Users/tessacarr/Downloads/Data Extraction/"
data <- read_xlsx(paste0(file_location, "Analysis_data_AUD_mortality.xlsx"), na="")
output <- "/Users/tessacarr/Downloads/Data Extraction/"

#Add logRR, loglowerRR, logupperRR, se, inverse_se columns
data1 <- data %>%
  mutate(
    logRR = log(RR),
    loglowerRR = log(lowerRR),
    logupperRR = log(upperRR),
    se = ifelse(upperRR!=1 & lowerRR!=1, (logupperRR - loglowerRR)/3.92, NA),
    inver_se = 1/se) %>%
  filter(!str_detect(group, "age"))

dose5 <- rma(yi = logRR, sei = se, data = data1[c(10,15),], method = "FE", )
predict(dose5, transf = exp)

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
predict(lin_mod, data.frame(alc_daily_g=seq(0, 150, 1)), order=TRUE, exp=TRUE) %>% 
ggplot(aes(x=alc_daily_g, y=pred)) +  scale_y_continuous(trans = log2_trans()) + 
  geom_line() + 
  geom_ribbon(aes(ymin= ci.lb, ymax=ci.ub), alpha=0.1) + 
  coord_cartesian(ylim=c(-5, 200))+
  theme_bw()

#Create quadratic model 
quad_mod <- dosresmeta(formula=logRR ~ alc_daily_g + I(alc_daily_g^2), proc= "1stage", id=id_study, type="cc", se=se, cases=outcome_n, n=total_n, data=data_all)

summary(quad_mod)

predict(quad_mod, data.frame(alc_daily_g=seq(0, 150, 1)), order=TRUE, exp=TRUE) %>% 
  ggplot(aes(x=alc_daily_g, y=pred)) +  geom_line() + 
  geom_ribbon(aes(ymin= ci.lb, ymax=ci.ub), alpha=0.1) + 
  coord_cartesian(ylim=c(-5, 200))+
  theme_bw()


#to remove any data file rm()
  