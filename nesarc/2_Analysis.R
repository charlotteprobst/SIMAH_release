
### SIMAH - NESARC Data Analysis

library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(msm)        # model transition probabilities
library(tableone)   # create descriptives table



## Set the working directory
setwd("C:/Users/klajd/OneDrive/SIMAH")
data    <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Data/"
output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Output/"

# Load data 
nesarc <- readRDS(paste0(data, "nesarc_clean.rds")) 


# Descriptives ------------------------------------------------------------------------------------------

# At baseline
nesarc_wave1 <- filter(nesarc, wave==1)

tab1 <-CreateTableOne(vars= c("female", "age", "race.factor", "married.factor", "edu3.factor", "income3.factor", "alc4.factor"), 
                      factorVars = c("female", "race.factor", "married.factor", "edu3.factor", "income3.factor", "alc4.factor"), 
                      data=nesarc_wave1)
summary(tab1)
table1 <- print(tab1, noSpaces = TRUE, catDigits = 0, contDigits = 1, pDigits = 2, printToggle = FALSE)   
write.csv(table1, file=paste0(output,"table1.csv"))  # export to excel, to copy/paste into manuscript
kableone(table1)                             # view in R; R Markdown friendly version

# Transition probabilities -------------------------------------------------------------------------------

# Descriptives of transitions
statetable.msm(alc4.factor, idnum, data=nesarc)

