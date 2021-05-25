# libraries required:
library("dplyr") # select

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")

dAAF <- read.csv("LE/AAF_estimates_US 2018.csv")
dPOP <- read.csv("LE/US exposure estimates for AAF calculation_final.csv")
dPOP <- select(dPOP, c("gender", "age_group", "population"))

dAAF <- merge(dAAF, dPOP, by = c("gender", "age_group"))

dAAF_weighted_mean <- dAAF %>%
      group_by(im_condition) %>% 
      summarise(weighted_AAF = weighted.mean(af_value, population))

dAAF_weighted_mean$gender <- "total"
dAAF_weighted_mean$age_group <- "15+"
      
dAAF <- select(dAAF, c("region", "year", "im_condition",  
                       "condition_category", "af_key"))

dAAF_weighted_mean <- left_join(dAAF_weighted_mean, dAAF, by = c("im_condition"))
dAAF_weighted_mean <- dAAF_weighted_mean[!duplicated(dAAF_weighted_mean),]

dAAF_weighted_mean <- dAAF_weighted_mean[ , c("region", "year",  "gender", "age_group", "im_condition", "weighted_AAF", "condition_category", "af_key")]

write.csv(dAAF_weighted_mean, "LE/AAF_estimates_US 2018_weighted_mean.csv")
