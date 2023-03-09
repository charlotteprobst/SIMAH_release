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

setwd("/home/cbuckley")

source("SIMAH_code/education_transitions/2_analysis/1_setup_markov_model.R")

# data <- read_rds("SIMAH_workplace/education_transitions/PSID_reweighted_2019_imputed_parental_income.RDS")
# 
# data1 <- data %>% filter(imp==1) %>% dplyr::select(uniqueID) %>% distinct() %>% 
#   ungroup() %>% mutate(consecID=1:n())
# data <- left_join(data, data1)
# # 
# # # # # # # function for assigning IDs - for each replication of an individual append a number to the end of the original ID
# IDfunction <- function(data){
#   n <- nrow(data)
#   data$ID <- 1:n
#   data$ID <- ifelse(data$ID==10, 155,
#                     ifelse(data$ID==100, 156, 
#                            ifelse(data$ID==20, 157,
#                                   ifelse(data$ID==30, 158,
#                                          ifelse(data$ID==40, 159, 
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
#   data$newID <- paste(data$consecID,data$ID,sep=".")
#   return(data)
# }
# # 
# data1 <- data %>% filter(imp==1)
# # 
# # # # # # apply the ID function to each original individual
# data1 <- data1 %>% mutate(uniqueID2 = uniqueID) %>% group_by(uniqueID, year,) %>%
#   group_modify(~IDfunction(.))
# #
# data1$newID <- as.numeric(data1$newID)
# # # # # # # # check that there are no duplicate newIDs for different original IDs
# test <- data1 %>% group_by(newID,year) %>% tally()
# # 
# # # # # # # no there aren't - each newID is based on on a single original ID
# IDS <- data1 %>% dplyr::select(uniqueID, year, newID)
# # 
# dataList <- list()
# for(i in 1:20){
#   dataList[[paste(i)]] <- data %>% filter(imp==i)
#   dataList[[paste(i)]] <- dataList[[paste(i)]][order(dataList[[paste(i)]]$uniqueID, dataList[[paste(i)]]$year),]
#   IDS <- IDS[order(IDS$uniqueID, IDS$year),]
#   dataList[[paste(i)]]$newID <- IDS$newID
# }
# 
# # # # # # save copy of the reweighted data with the new IDs
# saveRDS(dataList, "SIMAH_workplace/reweighted_PSID_imp_list.RDS")

#### SCRIPT CAN BE STARTED FROM HERE IF REWEIGHTED DATA WITH IDS EXISTS ####
# data <- read_rds("SIMAH_workplace/reweighted_PSID_imp_list.RDS")
# 
# # setup the datasets for both time periods
# 
# datat1 <- lapply(data, setup_markov_model, y=2009)
# datat2 <- lapply(data, setup_markov_model, y=2011)
# 
# saveRDS(datat1, "SIMAH_workplace/education_transitions/datat1.RDS")
# saveRDS(datat2, "SIMAH_workplace/education_transitions/datat2.RDS")

# who in the data transitions backwards, i.e. education gets lower - not allowed
# initQt1 <- list()
# initQt2 <- list()
# for(i in 1:20){
#   initQt1[[paste(i)]] <- crudeinits.msm(educNUM~year, newID, data=datat1[[paste(i)]], qmatrix=Q)
#   initQt2[[paste(i)]] <- crudeinits.msm(educNUM~year, newID, data=datat2[[paste(i)]], qmatrix=Q)
# }

#### SCRIPT CAN BE STARTED FROM HERE IF REWEIGHTED DATA WITH IDS EXISTS ####

datat1 <- read_rds("SIMAH_workplace/education_transitions/datat1.RDS")
datat2 <- read_rds("SIMAH_workplace/education_transitions/datat2.RDS")

# set up parallel
registerDoParallel(20)
options(future.rng.onMisuse="ignore")
options(future.globals.maxSize = 10000 * 1024^3)
options(future.fork.multithreading.enable = FALSE)


# specify baseline models 
modelst1_baseline <- foreach(i=1:length(datat1), .inorder=FALSE,
                             .packages=c("dplyr","tidyr","foreach")) %dopar% {
                               run_markov_model_baseline(datat1[[i]])
                             }
saveRDS(modelst1_baseline, "SIMAH_workplace/education_transitions/final_models/t1_baseline_parent.RDS")

modelst2_baseline <- foreach(i=1:length(datat1), .inorder=FALSE,
                             .packages=c("dplyr","tidyr","foreach")) %dopar% {
                               run_markov_model_baseline(datat2[[i]])
                             }
saveRDS(modelst1_baseline, "SIMAH_workplace/education_transitions/final_models/t2_baseline_parent.RDS")

# specify interaction models
modelst1_interaction <- foreach(i=1:length(datat1), .inorder=FALSE,
                             .packages=c("dplyr","tidyr","foreach")) %dopar% {
                               run_markov_model_parent(datat1[[i]])
                             }


data <- datat1[[1]]

model <- run_markov_model_parent(data)
saveRDS(modelst1_interaction, "SIMAH_workplace/education_transitions/final_models/t1_interaction.RDS")

modelst2_interaction <- foreach(i=1:length(datat1), .inorder=FALSE,
                             .packages=c("dplyr","tidyr","foreach")) %dopar% {
                               run_markov_model_parent(datat2[[i]])
                             }
saveRDS(modelst2_interaction, "SIMAH_workplace/education_transitions/final_models/t2_interaction.RDS")

# # calculate AIC for paper 
# 
AIC_baselinet1 <- lapply(modelst1_baseline, calculate_AIC) %>% do.call(rbind,.) %>% data.frame() %>%
  mutate(type="t1_baseline")
AIC_baselinet2 <- lapply(modelst2_baseline, calculate_AIC) %>% bind_rows() %>% data.frame() %>%
  mutate(type="t2_baseline")
AIC_parentt1 <- lapply(modelst1_parent, calculate_AIC) %>% bind_rows() %>% data.frame() %>%
  mutate(type="t1_parent")
AIC_parentt2 <- lapply(modelst2_parent, calculate_AIC) %>% bind_rows() %>% data.frame() %>%
  mutate(type="t2_parent")
# 
AIC <- rbind(AIC_baselinet1, AIC_baselinet2, AIC_parentt1, AIC_parentt2) 
# %>% group_by(type) %>% 
#   summarise(mean = mean(AIC))
# 
write.csv(AIC, "SIMAH_workplace/education_transitions/final_models/AIC.csv", row.names=F)
