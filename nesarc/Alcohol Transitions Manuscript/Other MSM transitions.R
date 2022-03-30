
# SIMAH - NESARC Alcohol Transitions
# Model selection

library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(janitor)    # data management
library(msm)        # model transition probabilities
library(tableone)   # create descriptives table
library(irr)        # calculate kappa for true and predicted alcohol use
# options(scipen=999) # prevent the use of scientific notation


# Specify the data and output file locations
data    <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Data/"
output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Output/"
models  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Output/Models/"
source("0_Functions.R")


# Load data / functions
nesarc <- readRDS(paste0(data, "nesarc_clean.rds")) 
nesarc_expanded <- readRDS(paste0(data, "nesarc_clean_expanded.rds")) 


# 1) ALCOHOL CONSUMPTION  ------------------------------------------------------------------------------------------------
## 1a) Run AlcUse MSM Model (9 models; least to most restrained, sequentally) ------------------------------------------------------------------------------------------------

# Count of transitions 
statetable.msm(alc5, idnum, data=nesarc)
statetable.msm(alc5, idnum, data=nesarc_expanded)



# Run different models using different transition intensity matrixes (Q) - i.e., what (instanteneous) transitions are allowed (specified by the non-zero entries)
# Start with the full model and gradually restricted the allowed transitions beginning with the most remote transitions;

# Model 1: allow all transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q1<- rbind ( c(0,     0,    0.25,  0.25, 0.25),
             c(0,     0,    0.25,  0.25, 0.25),
             c(0,     0.25, 0,     0.25, 0.25),
             c(0,     0.25, 0.25,  0,    0.25),
             c(0,     0.25, 0.25,  0.25, 0))

    # Run MSM model
    Q1 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q1)   # Specifies initial values; change Q value here, if needed
    alc5.msm1 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                       qmatrix = Q1, center=FALSE,                                # Change Q value here, if needed
                       control = list(trace=1, maxit=2000, fnscale = 3100000),    # the value of 3100000 was obtained from running the same model earlier (not shown) but also including the statement: fixedpars=TRUE 
                       covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
    saveRDS(alc5.msm1, paste0(models, "alc5.msm1.RDS")) # Save Results


    
# Model 2: remove Abstinence --> Category III
Q2<- rbind (c(0,     0,    0.25,  0.25, 0),
            c(0,     0,    0.25,  0.25, 0.25),
            c(0,     0.25, 0,     0.25, 0.25),
            c(0,     0.25, 0.25,  0,    0.25),
            c(0,     0.25, 0.25,  0.25, 0))
    
      # Run MSM model
      Q2 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q2)   # Specifies initial values; change Q value here, if needed
      alc5.msm2 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                        qmatrix = Q2, center=FALSE,                                # Change Q value here, if needed
                        control = list(trace=1, maxit=2000, fnscale = 3000000, reltol = 1e-16, ndeps = rep(1e-6, 4)),
                        covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
      saveRDS(alc5.msm2, paste0(models, "alc5.msm2.RDS")) # Save Results
      
      

      
# Model 3: remove Former --> Category III
Q3<- rbind (c(0,     0,    0.25,  0.25, 0),
            c(0,     0,    0.25,  0.25, 0),
            c(0,     0.25, 0,     0.25, 0.25),
            c(0,     0.25, 0.25,  0,    0.25),
            c(0,     0.25, 0.25,  0.25, 0))
    
      # Run MSM model
      Q3 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q3)   # Specifies initial values; change Q value here, if needed
      alc5.msm3 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                        qmatrix = Q3, center=FALSE,                                # Change Q value here, if needed
                        control = list(trace=1, maxit=1000, fnscale = 3000000),
                        covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
      saveRDS(alc5.msm3, paste0(models, "alc5.msm3.RDS")) # Save Results

    

      
          
# Model 4: remove Category III --> Former
Q4<- rbind (c(0,     0,    0.25,  0.25, 0),
            c(0,     0,    0.25,  0.25, 0),
            c(0,     0.25, 0,     0.25, 0.25),
            c(0,     0.25, 0.25,  0,    0.25),
            c(0,     0,    0.25,  0.25, 0))
  
      # Run MSM model
      Q4 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q4)   # Specifies initial values; change Q value here, if needed
      alc5.msm4 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                        qmatrix = Q4, center=FALSE,                                # Change Q value here, if needed
                        control = list(trace=1, maxit=1000, fnscale = 3000000),
                        covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
      saveRDS(alc5.msm4, paste0(models, "alc5.msm4.RDS")) # Save Results



      
      
      
# Model 5: remove Abstainer --> Category II
Q5<- rbind (c(0,     0,    0.25,  0,    0),
            c(0,     0,    0.25,  0.25, 0),
            c(0,     0.25, 0,     0.25, 0.25),
            c(0,     0.25, 0.25,  0,    0.25),
            c(0,     0,    0.25,  0.25, 0))

      # Run MSM model
      Q5 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q5)   # Specifies initial values; change Q value here, if needed
      alc5.msm5 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                        qmatrix = Q5, center=FALSE,                                # Change Q value here, if needed
                        control = list(trace=1, maxit=1000, fnscale = 3000000),
                        covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
      saveRDS(alc5.msm5, paste0(models, "alc5.msm5.RDS")) # Save Results


      
      
    
# Model 6: remove Former --> Category II
Q6<- rbind (c(0,     0,    0.25,  0,    0),
            c(0,     0,    0.25,  0,    0),
            c(0,     0.25, 0,     0.25, 0.25),
            c(0,     0.25, 0.25,  0,    0.25),
            c(0,     0,    0.25,  0.25, 0))

      # Run MSM model
      Q6 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q6)   # Specifies initial values; change Q value here, if needed
      alc5.msm6 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                        qmatrix = Q6, center=FALSE,                                # Change Q value here, if needed
                        control = list(trace=1, maxit=1000, fnscale = 3000000),
                        covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
      saveRDS(alc5.msm6, paste0(models, "alc5.msm6.RDS")) # Save Results


      
      

# Model 7: remove Category I --> III
Q7<- rbind (c(0,     0,    0.25,  0,    0),
            c(0,     0,    0.25,  0,    0),
            c(0,     0.25, 0,     0.25, 0),
            c(0,     0.25, 0.25,  0,    0.25),
            c(0,     0,    0.25,  0.25, 0))

      # Run MSM model
      Q7 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q7)   # Specifies initial values; change Q value here, if needed
      alc5.msm7 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                        qmatrix = Q7, center=FALSE,                                # Change Q value here, if needed
                        control = list(trace=1, maxit=1000, fnscale = 3000000),
                        covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
      saveRDS(alc5.msm7, paste0(models, "alc5.msm7.RDS")) # Save Results


      
      

# Model 8: remove Category III --> I
Q8<- rbind (c(0,     0,    0.25,  0,    0),
            c(0,     0,    0.25,  0,    0),
            c(0,     0.25, 0,     0.25, 0),
            c(0,     0.25, 0.25,  0,    0.25),
            c(0,     0,    0,  0.25, 0))


      # Run MSM model
      Q8 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q8)   # Specifies initial values; change Q value here, if needed
      alc5.msm8 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                        qmatrix = Q8, center=FALSE,                                # Change Q value here, if needed
                        control = list(trace=1, maxit=1000, fnscale = 3000000),
                        covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
      saveRDS(alc5.msm8, paste0(models, "alc5.msm8.RDS")) # Save Results


      
      

# Model 9: remove Category II --> Former
# Only allows transitions to an adjacent state, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q9 <- rbind ( c(0,     0,    0.25,  0,    0),
              c(0,     0,    0.25,  0,    0),
              c(0,     0.25, 0,     0.25, 0),
              c(0,     0,    0.25,  0,    0.25),
              c(0,     0,    0,     0.25, 0))
    

    # Run MSM model
    Q9 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q9)  # Specifies initial values; Change Q value here, if needed
    alc5.msm9 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                        qmatrix = Q9, center=FALSE,                             # Change Q value here, if needed
                        control = list(trace=1, maxit=500, fnscale = 3000000),
                        covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
    saveRDS(alc5.msm9, paste0(models, "alc5.msm9.RDS")) # Save Results
    

    
# Load the models
memory.limit(size=1e+13)
alc5.msm1 <- readRDS(paste0(models, "alc5.msm1.RDS")) # does not run
alc5.msm2 <- readRDS(paste0(models, "alc5.msm2.RDS")) # does not run
alc5.msm3 <- readRDS(paste0(models, "alc5.msm3.RDS")) # does not run
alc5.msm4 <- readRDS(paste0(models, "alc5.msm4.RDS")) # does not run
alc5.msm5 <- readRDS(paste0(models, "alc5.msm5.RDS")) # does not run
alc5.msm6 <- readRDS(paste0(models, "alc5.msm6.RDS")) # runs
alc5.msm7 <- readRDS(paste0(models, "alc5.msm7.RDS")) # runs
alc5.msm8 <- readRDS(paste0(models, "alc5.msm8.RDS")) # runs
alc5.msm9 <- readRDS(paste0(models, "alc5.msm9.RDS")) # runs




# Compare models
AIC(alc5.msm1, alc5.msm2, alc5.msm3, alc5.msm4, alc5.msm5, alc5.msm6, alc5.msm7, alc5.msm8, alc5.msm9)
    

    
## 1b) Run AlcUse MSM Model (3 models; therory based) ------------------------------------------------------------------------------------------------
# Count of transitions 
statetable.msm(alc5, idnum, data=nesarc)
statetable.msm(alc5, idnum, data=nesarc_expanded)

# Run different models using different transition intensity matrixes (Q) - i.e., what (instanteneous) transitions are allowed (specified by the non-zero entries)
# Start with the full model (no restrictions), then remove non-adjacent worsening transitions (allow non-adjacent recovery transitions), then only allow adjacent transitions


# Model 1: allow all transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q1<- rbind ( c(0,     0,    0.25,  0.25, 0.25),
  c(0,     0,    0.25,  0.25, 0.25),
  c(0,     0.25, 0,     0.25, 0.25),
  c(0,     0.25, 0.25,  0,    0.25),
  c(0,     0.25, 0.25,  0.25, 0))

    # Run MSM model
    Q1 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q1)   # Specifies initial values; change Q value here, if needed
    alc5.msm1_v2 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                          qmatrix = Q1, center=FALSE,                                # Change Q value here, if needed
                          control = list(trace=1, maxit=1000, fnscale = 3100000),    # the value of 3100000 was obtained from running the same model earlier (not shown) but also including the statement: fixedpars=TRUE 
                          covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
    saveRDS(alc5.msm1_v2, paste0(models, "alc5.msm1_v2.RDS")) # Save Results



    
# Model 2: allow for adjacent transitions and direct transitions to 'former drinker'
Q2<- rbind (c(0,     0,    0.25,  0,    0),
            c(0,     0,    0.25,  0,    0),
            c(0,     0.25, 0,     0.25, 0),
            c(0,     0.25, 0.25,  0,    0.25),
            c(0,     0.25, 0,     0.25, 0))

    # Run MSM model
    Q2 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q2)   # Specifies initial values; change Q value here, if needed
    alc5.msm2_v2 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                          qmatrix = Q2, center=FALSE,                                # Change Q value here, if needed
                          control = list(trace=1, maxit=2000, fnscale = 3000000, reltol = 1e-16, ndeps = rep(1e-6, 4)),
                          covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
    saveRDS(alc5.msm2_v2, paste0(models, "alc5.msm2_v2.RDS")) # Save Results



# Model 3: remove all non-adjacent transitions; its the same as the model from the previous section


# Load Models:
alc5.msm1_v2 <- readRDS(paste0(models, "alc5.msm1_v2.RDS"))
alc5.msm2_v2 <- readRDS(paste0(models, "alc5.msm2_v2.RDS"))
alc5.msm3_v2 <- readRDS(paste0(models, "alc5.msm9.RDS"))

alc5.msm1_v2
alc5.msm2_v2
alc5.msm3_v2

# Compare models
AIC(alc5.msm1_v2, alc5.msm2_v2, alc5.msm3_v2)



