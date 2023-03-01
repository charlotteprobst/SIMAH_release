# script to generate MSM model for education for education transitions paper
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(readxl)
library(doParallel)
library(foreach)
library(parallel)

# setwd("/home/cbuckley")
setwd("~/Google Drive/SIMAH Sheffield")

source("SIMAH_code/education_transitions/2_analysis/new_analysis/1_setup_markov_model.R")

# data <- read_csv("SIMAH_workplace/education_transitions/new_PSID_processed_weighted.csv") %>%
#   dplyr::select(-`...1`) %>% group_by(uniqueID) %>% mutate(consecID=1:n())
# 
# # # # # # # # function for assigning IDs - for each replication of an individual append a number to the end of the original ID
# IDfunction <- function(data){
#   n <- nrow(data)
#   data$ID <- 1:n
#   data$ID <- ifelse(data$ID==10, 172,
#                     ifelse(data$ID==100, 173,
#                            ifelse(data$ID==20, 174,
#                                   ifelse(data$ID==30, 175,
#                                          ifelse(data$ID==40, 176,
#                                                 ifelse(data$ID==50, 161,
#                                                        ifelse(data$ID==60, 162,
#                                                               ifelse(data$ID==70, 163,
#                                                                      ifelse(data$ID==80, 164,
#                                                                             ifelse(data$ID==90, 165,
#                                                                                    ifelse(data$ID==110, 166,
#                                                                                           ifelse(data$ID==120, 167,
#                                                                                                  ifelse(data$ID==130, 168,
#                                                                                                         ifelse(data$ID==140, 169,
#                                                                                                                ifelse(data$ID==150, 171,
#                                                                                    data$ID)))))))))))))))
#   data$newID <- paste(data$uniqueID2,data$ID,sep=".")
#   return(data)
# }
# # # # # # # apply the ID function to each original individual
# data <- data %>% mutate(uniqueID2 = uniqueID) %>% group_by(uniqueID,year) %>%
#   group_modify(~IDfunction(.))
# # #
# data$newID <- as.numeric(data$newID)
# # # # # # # # # check that there are no duplicate newIDs for different original IDs
# test <- data %>% group_by(newID,year) %>% tally()
# # #
# write.csv(data, "SIMAH_workplace/education_transitions/new_PSID_weighted_IDs.csv", row.names=F)

#### SCRIPT CAN BE STARTED FROM HERE IF REWEIGHTED DATA WITH IDS EXISTS ####
data <- read_csv("SIMAH_workplace/education_transitions/new_PSID_weighted_IDs.csv")

# do the first analysis on the split time periods 

# # setup the datasets for both time periods
# 
datat1 <- setup_markov_model(data, y=2009)
datat2 <- setup_markov_model(data, y=2011)

Q <- rbind( c(0.5, 0.5, 0, 0, 0),
            c(0, 0.5, 0.5, 0, 0),
            c(0, 0, 0.5, 0.5, 0),
            c(0, 0, 0, 0.5, 0.5),
            c(0, 0, 0, 0, 0.5))
# specify baseline models - just race and ethnicity 
modelt1_baseline <- msm(educNUM~year, newID, data=datat1, qmatrix=Q,
                                   center=FALSE,
                                   covariates=~agescaled + agesqscaled + sex*racefinal2,
                        control=list(trace=1))
modelt1_baseline

modelt1_income <- msm(educNUM~year, newID, data=datat1, qmatrix=Q,
                        center=FALSE,
                        covariates=~agescaled + agesqscaled + sex + racefinal2 + total_fam_income,
                        control=list(trace=1))
modelt1_income

saveRDS(modelt1_baseline, "SIMAH_workplace/education_transitions/final_models/modelt1_baseline.RDS")

modelt2_baseline <-   model <- msm(educNUM~year, newID, data=datat2, qmatrix=Q,
                                   center=FALSE,
                                   covariates=~agescaled + agesqscaled + sex*racefinal2,
                                   control=list(trace=1, fnscale=1000))
modelt2_baseline

saveRDS(modelt2_baseline, "SIMAH_workplace/education_transitions/final_models/modelt2_baseline.RDS")

modelt1_interaction <- msm(educNUM~year, newID, data=datat1, qmatrix=Q,
                        center=FALSE,
                        covariates=~agescaled + agesqscaled + sex + racefinal2*onecollegeplus,
                        control=list(trace=1))
modelt1_interaction

saveRDS(modelt1_interaction, "SIMAH_workplace/education_transitions/final_models/modelt1_interaction.RDS")

modelt2_interaction <- msm(educNUM~year, newID, data=datat2, qmatrix=Q,
                           center=FALSE,
                           covariates=~agescaled + agesqscaled + sex + racefinal2*onecollegeplus,
                           control=list(trace=1))
modelt2_interaction

saveRDS(modelt2_interaction, "SIMAH_workplace/education_transitions/final_models/modelt2_interaction.RDS")

# calculate AIC for paper
AIC(modelt1_baseline, modelt1_interaction)
AIC(modelt2_baseline, modelt2_interaction)