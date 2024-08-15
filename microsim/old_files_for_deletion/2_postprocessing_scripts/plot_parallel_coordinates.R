#####Wrapper code for dynamic microsimulation
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(GGally)
library(viridis)
library(plotly)
library(parcoords)

options(scipen=999)

###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
setwd(paste(WorkingDirectory))

# do age specific parameters first 
toplhs <- read.csv("SIMAH_workplace/microsim/2_output_data/lhsSamples_wave15_agesp.csv") %>% 
  mutate(SampleNum=as.factor(SampleNum))

implausibility <- read.csv("SIMAH_workplace/microsim/2_output_data/calibration_output/implausibility_wave15.csv") %>% 
 rename(SampleNum=samplenum) %>% 
  mutate(SampleNum=as.factor(SampleNum))
  
toplhs <- left_join(toplhs, implausibility) %>% mutate(rank=ntile(maximplausibility, 100))
# p <- ggparcoord(toplhs, columns=2:15, groupColumn=1,
#            showPoints = TRUE,
#            alphaLines=0.3,
#            scale="center") +
#   theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.5),
#         legend.position="none")
# 
# ggplotly(p)

forplot <- toplhs %>% dplyr::select(BETA_MALE_MORTALITY:maximplausibility) %>% 
  rename(MortM = BETA_MALE_MORTALITY,
         MortF = BETA_FEMALE_MORTALITY,
         FDM = BETA_FORMER_DRINKERS_MEN,
         FDF = BETA_FORMER_DRINKERS_WOMEN,
         Met1M = METABOLIC_BETA1_MALE,
         Met2M = METABOLIC_BETA2_MALE,
         Met1F = METABOLIC_BETA1_FEMALE,
         Met2F = METABOLIC_BETA2_FEMALE,
         Hep = BETA_HEPATITIS,
         Thres = THRESHOLD,
         ThresMod = THRESHOLD_MODIFIER,
         IRR = IRR_correlation,
         Decay = DECAY_SPEED,
         Imp = maximplausibility)

parcoords(forplot,
          reorderable = TRUE,
          brushMode = "2d-strums",
          height=1000, width=1000)
