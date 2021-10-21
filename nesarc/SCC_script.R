
# SIMAH - NESARC Alcohol Transitions
# Data Analysis

library(msm)        # model transition probabilities


# SCC Server:
setwd("/external/mgmt3/imaging/scratch/Imhpr/kpuka/nesarc/")
data    <- "Data/"
output  <- "Output/"

# Load data / functions
nesarc <- readRDS(paste0(data, "nesarc_clean.rds")) 
nesarc_expanded <- readRDS(paste0(data, "nesarc_clean_expanded.rds")) 

## Run AlcUse MSM Model ------------------------------------------------------------------------------------------------

# Run different models using different transition intensity matrixes (Q) - i.e., what (instanteneous) transitions are allowed (specified by the non-zero entries)
# Start with the full model (no restrictions), then remove non-adjacent worsening transitions (allow non-adjacent recovery transitions), then only allow adjacent transitions


# Model 1: allow all transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q1<- rbind (c(0,     0,    0.25,  0.25, 0.25),
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
saveRDS(alc5.msm1_v2, paste0(output, "alc5.msm1_v2.RDS")) # Save Results



# Model 2: remove non-adjacent 'worsening' transitions
Q1<- rbind (c(0,     0,    0.25,  0,    0),
            c(0,     0,    0.25,  0,    0),
            c(0,     0.25, 0,     0.25, 0),
            c(0,     0.25, 0.25,  0,    0.25),
            c(0,     0.25, 0.25,  0.25, 0))

# Run MSM model
Q2 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q2)   # Specifies initial values; change Q value here, if needed
alc5.msm2_v2 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
  qmatrix = Q2, center=FALSE,                                # Change Q value here, if needed
  control = list(trace=1, maxit=1000, fnscale = 3000000),
  covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
saveRDS(alc5.msm2_v2, paste0(output, "alc5.msm2_v2.RDS")) # Save Results





# Model 3: remove all non-adjacent transitions 
# Only allows transitions to an adjacent state, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q3 <- rbind ( c(0,     0,    0.25,  0,    0),
              c(0,     0,    0.25,  0,    0),
              c(0,     0.25, 0,     0.25, 0),
              c(0,     0,    0.25,  0,    0.25),
              c(0,     0,    0,     0.25, 0))


# Run MSM model
Q3 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q3)  # Specifies initial values; Change Q value here, if needed
alc5.msm3_v2 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
  qmatrix = Q3, center=FALSE,                             # Change Q value here, if needed
  control = list(trace=1, maxit=1000, fnscale = 3000000),
  covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
saveRDS(alc5.msm3_v2, paste0(output, "alc5.msm3_v2.RDS")) # Save Results


# Compare models
AIC(alc5.msm1_v2, alc5.msm2_v2, alc5.msm3_v2)


