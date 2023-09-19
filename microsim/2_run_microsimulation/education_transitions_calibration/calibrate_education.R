# SIMAH project November 2021 

# code for calibration of MSM model parameters to state-level education outputs 

# first set up for microsimulation 
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
suppressPackageStartupMessages(library("dplyr"))
library(devtools)
library(roxygen2)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(fitdistrplus)
library(lhs)
library(truncnorm)
library(data.table)
library(doParallel)
options(scipen=999)
# set seed for reproducibility - IMPORTANT - DO NOT CHANGE
# note - this also needs to be ran straight after R has been opened
set.seed(42)

####EDIT ONLY BELOW HERE ### 
###set working directory to the main "Microsimulation" folder in your directory 
# WorkingDirectory <- "U:/SIMAH/"
# WorkingDirectory <- "C:/Users/laura/Documents/CAMH/SIMAH/"
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/1_input_data/")

# load in microsim R package
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)

####which geography -  needs to be written as USA, California, Minnesota, New York, Texas, Tennessee
States <- c("California","Colorado","Florida","Indiana",
            "Louisiana","Massachusetts","Michigan","Minnesota",
            "Missouri","New York", "Oregon", "Pennsylvania",
            "Tennessee","Texas","USA")

# for(s in States){
SelectedState <- "USA"

source("SIMAH_code/microsim/2_run_microsimulation/0_model_settings.R")
lhs <- lhs[[1]]

# now sample parameters for the education transitions
nsamples <- 5
source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/extract_uncertainty.R")

# save samples 
saveRDS(transitionsList, paste("SIMAH_workplace/microsim/2_output_data/education_calibration/transitionsList", SelectedState,".RDS",sep=""))
saveRDS(estimates, paste("SIMAH_workplace/microsim/2_output_data/education_calibration/sampled_markov", SelectedState, ".RDS"))

registerDoParallel(15)
# registerDoSNOW(c1)
# plan(multicore, workers=24)
options(future.rng.onMisuse="ignore")
options(future.globals.maxSize = 10000 * 1024^3)
options(future.fork.multithreading.enable = FALSE)
Output <- list()

sampleseeds <- expand.grid(samplenum = 1:length(transitionsList), seeds=1)

baseorig <- basepop

rm(education_transitions)

Output <- foreach(i=1:nrow(sampleseeds), .inorder=TRUE, .combine=rbind) %do% {
  print(i)
  samplenum <- as.numeric(sampleseeds$samplenum[i])
  seed <- as.numeric(sampleseeds$seed[i])
  basepop <- baseorig
  education_transitions <- transitionsList[[samplenum]]
  run_microsim( seed,samplenum,basepop,brfss,
                death_counts,
                updatingeducation, education_transitions,
                migration_counts,
                updatingalcohol, alcohol_transitions,
                catcontmodel, Hep, drinkingdistributions,
                base_counts, diseases, lhs, liverinteraction,
                policy=0, percentreduction=0.1, year_policy, inflation_factors,
                age_categories,
                update_base_rate,
                minyear=2000, maxyear=2003, output="demographics")
}

# save the output 
write.csv(Output, "SIMAH_workplace/microsim/2_output_data/education_calibration/education_transitions_range_reps.csv", row.names=F)
                       
#         
# # get target data 
# source("SIMAH_code/microsim/2_run_microsimulation/2_postprocessing_scripts/process_education_compare.R")
# 
# # calculate error from target data 
# Output <- do.call(rbind,Output)
# Output <- Output %>% group_by(samplenum, year, microsim.init.sex, microsim.init.education) %>% 
#   summarise(n=sum(n)) %>% ungroup() %>% 
#   group_by(samplenum, year, microsim.init.sex) %>% 
#   mutate(microsimpercent = n/sum(n),
#          year=as.numeric(year))
# Output <- left_join(Output, target)
# 
# error <- left_join(Output,target) %>% ungroup() %>% 
#   group_by(samplenum) %>% 
#   mutate(errorsq = (microsimpercent-targetpercent)^2) %>% 
#   group_by(samplenum) %>% 
#   summarise(RMSE = sqrt(mean(errorsq)))
# 
# write.csv(error, paste("SIMAH_workplace/microsim/2_output_data/error_RMSE",SelectedState,".csv",sep=""), row.names=F)
# 
# final <- error[which(error$RMSE==min(error$RMSE)),]$samplenum
# transitions <- transitionsList[[final]]
# saveRDS(transitions, paste0("SIMAH_workplace/microsim/2_output_data/final_ed_transitions", SelectedState, ".RDS"))
# 
# # graph <- Output %>% pivot_longer(cols=microsimpercent:targetpercent, values_to="percent") %>% 
# #   mutate(samplenum = ifelse(name=="targetpercent", "target", samplenum))
# 
# Output <- Output %>% mutate(microsim.init.sex = recode(microsim.init.sex,
#                                                        "f"="Women","m"="Men"),
#                             microsim.init.education = recode(microsim.init.education,
#                                                              "LEHS"="High school or less",
#                                                              "SomeC"="Some college",
#                                                              "College"="College degree plus"),
#                             microsim.init.education = factor(microsim.init.education,
#                                                              levels=c("High school or less",
#                                                                       "Some college",
#                                                                       "College degree plus")))
# scaleFUN <- function(x) sprintf("%.2f", x)
# ggplot(data=Output, aes(x=year, y=microsimpercent, colour=as.factor(samplenum))) + 
#   geom_line(linetype="dashed") + geom_line(aes(x=year, y=targetpercent), colour="black") + 
#   facet_grid(cols=vars(microsim.init.sex), rows=vars(microsim.init.education), scales="free") + 
#   theme_bw() + scale_y_continuous(labels=scales::percent, limits=c(0,NA)) + 
#   theme(legend.position="bottom",
#         legend.title=element_blank()) + 
#   ylab("percentage in category")
# ggsave(paste("SIMAH_workplace/microsim/2_output_data/plots/education_states_compare",SelectedState,".png",sep=""),
#        dpi=300, width=33, height=19, units="cm")
# 
# bestrate <- Output %>% filter(samplenum==final)
# ggplot(data=bestrate, aes(x=year, y=microsimpercent, colour=as.factor(samplenum))) + 
#   geom_line(linetype="dashed") + geom_line(aes(x=year, y=targetpercent), colour="black") + 
#   facet_grid(cols=vars(microsim.init.sex), rows=vars(microsim.init.education), scales="free") + 
#   theme_bw() + scale_y_continuous(labels=scales::percent, limits=c(0,NA)) + 
#   theme(legend.position="bottom",
#         legend.title=element_blank()) + 
#   ylab("percentage in category")
# ggsave(paste("SIMAH_workplace/microsim/2_output_data/plots/education_states_bestrate",SelectedState, ".png",sep=""),
#        dpi=300, width=33, height=19, units="cm")


