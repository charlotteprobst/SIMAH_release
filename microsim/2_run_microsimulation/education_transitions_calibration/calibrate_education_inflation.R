# SIMAH project November 2021 

# code for calibration of MSM model parameters to state-level education outputs 

# first set up for microsimulation 
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
library(devtools)
library(roxygen2)
library(gatbxr)
# if having trouble with loading this package - run the below two lines
# install.packages("remotes")
# remotes::install_github("drizztxx/gatbxr")
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(fitdistrplus)
library(lhs)
library(truncnorm)
library(data.table)
library(gridExtra)
library(doParallel)
library(splitstackshape)

options(dplyr.summarise.inform = FALSE)
# set seed for reproducibility - IMPORTANT - DO NOT CHANGE
# note - this also needs to be ran straight after R has been opened
set.seed(42)

####EDIT ONLY BELOW HERE ### 
###set working directory to the main "Microsimulation" folder in your directory 
# WorkingDirectory <- "U:/SIMAH/"
# WorkingDirectory <- "C:/Users/laura/Documents/CAMH/SIMAH/"
# WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
WorkingDirectory <- "/home/cbuckley/"
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
nsamples <- 200

# number of different versions of the inflation to run
n_inflations <- 10

source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/extract_uncertainty_inflations.R")

# save samples 
saveRDS(transitionsList, paste("SIMAH_workplace/microsim/2_output_data/education_calibration/transitionsList", SelectedState,".RDS",sep=""))
saveRDS(estimates, paste("SIMAH_workplace/microsim/2_output_data/education_calibration/sampled_markov", SelectedState, ".RDS"))

# set to 1 if running on local machine 
registerDoParallel(15)
# registerDoSNOW(c1)
# plan(multicore, workers=24)
options(future.rng.onMisuse="ignore")
options(future.globals.maxSize = 10000 * 1024^3)
options(future.fork.multithreading.enable = FALSE)
Output <- list()

sampleseeds <- expand.grid(samplenum = 1:length(transitionsList), seeds=1)

rm(education_transitions)

Output <- foreach(i=1:nrow(sampleseeds), .inorder=TRUE) %dopar% {
  print(i)
  samplenum <- as.numeric(sampleseeds$samplenum[i])
  seed <- as.numeric(sampleseeds$seed[i])
  # basepop <- baseorig
  education_transitions <- transitionsList[[samplenum]]
  run_microsim(seed,samplenum,basepop,brfss,
                death_counts,
                updatingeducation, education_transitions,
                migration_counts,
                updatingalcohol, alcohol_transitions,
                catcontmodel, Hep, drinkingdistributions,
                base_counts, diseases, lhs, liverinteraction,
                policy=0, percentreduction=0.1, year_policy, inflation_factors,
                age_inflated,
                update_base_rate,
                minyear=2000, maxyear=2019, output="demographics")
}

for(i in 1:length(Output)){
  Output[[i]]$inflation <- unique(transitionsList[[i]]$inflation)
}

Output <- do.call(rbind,Output)

# save the output 
write.csv(Output, "SIMAH_workplace/microsim/2_output_data/education_calibration/prior_range_inflations.csv", row.names=F)

# plot the data compared to target
# data <- Output %>% 
#   mutate(AGECAT = cut(microsim.init.age,
#                       breaks=c(0,24,34,44,54,64,79),
#                       labels=c("18-24","25-34","35-44","45-54",
#                                "55-64","65-79")),
#          SEX = ifelse(microsim.init.sex=="m", "Men","Women"),
#          RACE = recode(microsim.init.race, "BLA"="Black","WHI"="White","SPA"="Hispanic",
#                        "OTH"="Others")) %>% 
#   rename(EDUC=microsim.init.education, YEAR=year) %>% 
#   group_by(YEAR, samplenum, SEX, AGECAT, RACE,
#            EDUC) %>% 
#   summarise(n=sum(n)) %>% 
#   ungroup() %>% 
#   group_by(YEAR, samplenum, SEX, AGECAT, RACE) %>% 
#   mutate(prop=n/sum(n), YEAR=as.integer(as.character(YEAR))) %>% 
#   dplyr::select(-n) %>% drop_na()
# 
# # read in target data 
# targets <- read.csv("SIMAH_workplace/microsim/2_output_data/education_calibration/target_data.csv") %>% 
#   dplyr::select(-n) %>% 
#   rename(target=prop)
# 
# data <- left_join(data,targets) %>% 
#   mutate(EDUC = factor(EDUC, levels=c("LEHS","SomeC","College")))
# 
# ggplot(data=subset(data, SEX=="Men" & AGECAT=="18-24"), 
#        aes(x=as.numeric(YEAR), y=prop, colour=as.factor(samplenum))) + 
#   geom_line(linewidth=1) + 
#   geom_line(aes(x=YEAR,y=target), colour="darkblue",linewidth=1) + 
#   facet_grid(cols=vars(RACE), rows=vars(EDUC)) + 
#   theme_bw() + 
#   theme(legend.position = "none") + 
#   xlab("Year") + 
#   ggtitle("Men") + 
#   scale_y_continuous(labels=scales::percent)
# 
# ggplot(data=subset(data, SEX=="Women" & AGECAT=="18-24"), 
#        aes(x=as.numeric(YEAR), y=prop, colour=as.factor(samplenum))) + 
#   geom_line(linewidth=1) + 
#   geom_line(aes(x=YEAR,y=target), colour="darkblue",linewidth=1) + 
#   facet_grid(cols=vars(RACE), rows=vars(EDUC)) + 
#   theme_bw() + 
#   theme(legend.position = "none") + 
#   xlab("Year") + 
#   ggtitle("Women") + 
#   scale_y_continuous(labels=scales::percent)

                     