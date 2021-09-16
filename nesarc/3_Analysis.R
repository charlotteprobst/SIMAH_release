
# SIMAH - NESARC Alcohol Transitions
# Data Analysis

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

# Count of transitions 
statetable.msm(alc4, idnum, data=nesarc)


# Specify transition intensity matrix (Q) - i.e., what (instanteneous) transitions are allowed (specified by the non-zero entries)
# Will only allow transitions to an adjacent state 

Q <- rbind ( c(0,     0.25, 0,     0),
             c(0.25,  0,    0.25,  0),
             c(0,     0.25, 0,     0.25),
             c(0,     0,    0.25,  0) )
 
# Specify initial values 
Q.crude <- crudeinits.msm(alc4 ~ years, idnum, data=nesarc, qmatrix=Q)

 
# Running MSM model
alc4.msm1 <- msm (alc4 ~ years, subject=idnum, data = nesarc, qmatrix = Q, control = list(fnscale = 50000))
alc4.msm1
 
 
 