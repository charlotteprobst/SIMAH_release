
# SIMAH - NESARC Alcohol Transitions
# Model selection

# Load packages and data
library(tidyverse)  # data management
library(msm)        # model transition probabilities

# Specify the data and output file locations
data    <- "~/Google Drive/SIMAH Sheffield/SIMAH_workplace/nesarc/Processed data/"  # Location of data
models  <- "~/Google Drive/SIMAH Sheffield/SIMAH_workplace/nesarc/Models/"          # Location of saved MSM models

# data    <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/2_Processed data/"  # Location of data
# models  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Alcohol Transitions/Models/"          # Location of saved MSM models

# MSM 3: AlcUse (4levels) ---------------------------------------------------------------------------------------------

# Specify allowed transitions; only allow adjacent transitions
Q <- rbind ( c(0,    0.25,  0,    0),
             c(0.25, 0,     0.25, 0),
             c(0,    0.25,  0,    0.25),
             c(0,    0,     0.25, 0))
        
nesarc_expanded <- nesarc_expanded %>% filter(age<=79)

deterministic_pop <- read_csv("/Users/charlottebuckley/Google Drive/SIMAH Sheffield/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/NESARC_optimisation_output/full_pop_deterministic10000.csv") 

Poplong <- deterministic_pop %>% 
  dplyr::select(microsim.init.id, year, microsim.init.sex,microsim.init.race,
                microsim.init.age, microsim.init.education, AlcCAT) %>% 
  group_by(microsim.init.id) %>% 
  add_tally() %>% 
  filter(n>1)
  
Poplong$catnum <- ifelse(Poplong$AlcCAT=="Non-drinker", 1,
                         ifelse(Poplong$AlcCAT=="Low risk", 2, 
                                ifelse(Poplong$AlcCAT=="Medium risk", 3,
                                       ifelse(Poplong$AlcCAT=="High risk", 4, NA))))

Poplong <- Poplong %>% 
  arrange(microsim.init.id, year)

# Specifies initial values
Q_allAges  <- crudeinits.msm(catnum ~ year, microsim.init.id, data=Poplong, qmatrix=Q)      # When age is categorical
Q_le90 <- crudeinits.msm(alc4 ~ years, idnum, data=nesarc_expanded_le90, qmatrix=Q) # When age is continuous 


Poplong$age3 <- cut(Poplong$microsim.init.age,
                            breaks=c(0,24,64,100),
                            labels=c("18-24","25-64","65+"))

Poplong$microsim.init.sex <- as.factor(Poplong$microsim.init.sex)
Poplong$microsim.init.education <- as.factor(Poplong$microsim.init.education)
Poplong$microsim.init.race <- as.factor(Poplong$microsim.init.race)

Poplong$microsim.init.race <- relevel(Poplong$microsim.init.race, ref="OTH")

Poplong <- Poplong %>% filter(year>=2001)

# MSM 3: All ages **************************************************************************************
# MSM 3A: Age (3 categories)
msm3a <- msm (catnum ~ year, subject=microsim.init.id, data = Poplong, qmatrix = Q_allAges, 
              center=FALSE, control = list(trace=1, maxit=600, fnscale = 2632448),
                  covariates = ~ microsim.init.sex + age3 + microsim.init.education + microsim.init.race)
        saveRDS(msm3a, paste0(models, "msm_deterministic.RDS")) 

   
# MSM 3B: Age (7 categories)
msm3b <- msm (alc4 ~ years, subject=idnum, data = nesarc_expanded, qmatrix = Q_allAges,
              center=FALSE, control = list(trace=1, maxit=600, fnscale = 3000000),
              covariates = ~ female_w1 + age7 + edu3 + race_w1)
        saveRDS(msm3b, paste0(models, "msm3b_new.RDS")) # Save Results

        
        
        
# MSM 3: Ages less than 90years (le90) *********************************************************************************    
# MSM 3A: Age (3 categories)
msm3a_le90 <- msm (alc4 ~ years, subject=idnum, data = nesarc_expanded_le90, qmatrix = Q_le90, 
              center=FALSE, control = list(trace=1, maxit=600, fnscale = 3000000),
              covariates = ~ female_w1 + age3 + edu3 + race_w1)
      saveRDS(msm3a_le90, paste0(models, "msm3a_le90.RDS")) 


# MSM 3B: Age (7 categories)
msm3b_le90 <- msm (alc4 ~ years, subject=idnum, data = nesarc_expanded_le90, qmatrix = Q_le90,
              center=FALSE, control = list(trace=1, maxit=600, fnscale = 3000000),
              covariates = ~ female_w1 + age7 + edu3 + race_w1)
      saveRDS(msm3b_le90, paste0(models, "msm3b_le90.RDS")) # Save Results
        

# MSM 3C: Age (continuous)
msm3c_le90 <- msm(alc4 ~ years, subject=idnum, data = nesarc_expanded_le90, qmatrix = Q_le90,
              center=FALSE, control = list(trace=1, maxit=5000, fnscale = 750000),
              covariates = ~ female_w1 + age.c + edu3 + race_w1)
       saveRDS(msm3c_le90, paste0(models, "msm3c_le90.RDS")) 

      
# MSM 3D_le90: Age (squared)
msm3d_le90 <- msm(alc4 ~ years, subject=idnum, data = nesarc_expanded_le90, qmatrix = Q_le90,
            center=FALSE, control = list(trace=1, maxit=5000, fnscale = 750000),
            covariates = ~ female_w1 + age.c + age_sq.c + edu3 + race_w1)
        saveRDS(msm3d_le90, paste0(models, "msm3d_le90.RDS")) # Save Results

     


# Compare the MSM models (with adjacent transitions) -------------------------------------------------------

# Models with participants aged less than 90 years (where some models used continous age)
msm1a_le90 <- readRDS(paste0(models, "msm1a_le90.RDS"))
msm1b_le90 <- readRDS(paste0(models, "msm1b_le90.RDS"))
msm1c_le90 <- readRDS(paste0(models, "msm1c_le90.RDS"))
msm1d_le90 <- readRDS(paste0(models, "msm1d_le90.RDS"))
msm2a_le90 <- readRDS(paste0(models, "msm2a_le90.RDS"))
msm2b_le90 <- readRDS(paste0(models, "msm2b_le90.RDS"))
msm2c_le90 <- readRDS(paste0(models, "msm2c_le90.RDS"))
msm2d_le90 <- readRDS(paste0(models, "msm2d_le90.RDS"))
msm3a_le90 <- readRDS(paste0(models, "msm3a_le90.RDS"))
msm3b_le90 <- readRDS(paste0(models, "msm3b_le90.RDS"))
msm3c_le90 <- readRDS(paste0(models, "msm3c_le90.RDS"))
msm3d_le90 <- readRDS(paste0(models, "msm3d_le90.RDS"))

AIC(msm1a_le90, msm1b_le90, msm1c_le90, msm1d_le90, 
    msm2a_le90, msm2b_le90, msm2c_le90, msm2d_le90, 
    msm3a_le90, msm3b_le90, msm3c_le90, msm3d_le90) %>% 
  arrange(AIC)

# Best model: msm3b_le90 -> AlcUse 4 categotries, age 7 categories



# Models with all participants
msm1a <- readRDS(paste0(models, "msm1a.RDS"))
msm1b <- readRDS(paste0(models, "msm1b.RDS"))
msm2a <- readRDS(paste0(models, "msm2a.RDS"))
msm2b <- readRDS(paste0(models, "msm2b.RDS"))
msm3a <- readRDS(paste0(models, "msm3a.RDS"))
msm3b <- readRDS(paste0(models, "msm3b.RDS"))

AIC(msm1a, msm1b, msm2a, msm2b, msm3a, msm3b) %>% arrange(AIC)

# Best model: msm3b -> AlcUse 4 levels, age 7 categories



# Non-adjacent transitions (AlcUse 4 levels; Age 7 levels) -----------------------------------------------------------

# Specify allowed transitions
# Only allow adjacent transitions, and transitions back to non-drinker
Q <- rbind (c(0,    0.25,  0,    0),
            c(0.25, 0,     0.25, 0),
            c(0.25, 0.25,  0,    0.25),
            c(0.25, 0,     0.25, 0))


# Specifies initial values
Q <- crudeinits.msm(alc4 ~ years, idnum, data=nesarc_expanded, qmatrix=Q)

# MSM 3E: Age (7 categories); non-adjacent transitions 
msm3b_non_adjecent <- msm (alc4 ~ years, subject=idnum, data = nesarc_expanded, qmatrix = Q,
              center=FALSE, control = list(trace=1, maxit=600, fnscale = 3000000),
              covariates = ~ female_w1 + age7 + edu3 + race_w1)
      saveRDS(msm3b_non_adjecent, paste0(models, "msm3b_non_adjecent.RDS")) 

# Compare the models
msm3b <- readRDS(paste0(models, "msm3b.RDS"))
msm3b_non_adjecent <- readRDS(paste0(models, "msm3b_non_adjecent.RDS"))

AIC(msm3b, msm3b_non_adjecent)

# Best model: msm3b -> AlcUse 7 categotries, age 7 categories, with adjacent transitions only  



# Unadjusted version of optimal MSM model -----------------------------------------------------------

# Specify allowed transitions; only allow adjacent transitions
Q <- rbind (c(0,    0.25,  0,    0),
            c(0.25, 0,     0.25, 0),
            c(0,    0.25,  0,    0.25),
            c(0,    0,     0.25, 0))

# Specifies initial values
Q <- crudeinits.msm(alc4 ~ years, idnum, data=nesarc_expanded, qmatrix=Q)


# Unadjusted Model; AlcUse (4 categories)
msm3b_crude <- msm (alc4 ~ years, subject=idnum, data = nesarc_expanded, qmatrix = Q,
                    center=FALSE, control = list(trace=1, maxit=600, fnscale = 3000000))
    saveRDS(msm3b_crude, paste0(models, "msm3b_crude_new.RDS")) 

# HED Model -----------------------------------------------------------------------------------

# Specify allowed transitions
# only allow adjacent transitions
Q <- rbind (c(0,     0.25,    0,     0,    0),
            c(0.25,  0,       0.25,  0,    0),
            c(0,     0.25,    0,     0.25, 0),
            c(0,     0,       0.25,  0,    0.25),
            c(0,     0,       0,     0.25, 0))

# Specify initial values 
Q <- crudeinits.msm(hed ~ years, idnum, data=nesarc_expanded, qmatrix=Q)

# Run MSM model (unadjusted)
hed_crude.msm <- msm (hed ~ years, subject=idnum, data = nesarc_expanded, qmatrix = Q, 
                      center=FALSE, control = list(trace=1, maxit=500, fnscale = 3000000))
saveRDS(hed_crude.msm, paste0(models, "hed_crude.msm_new.RDS"))


# Run MSM model (adjusted for covariates)
hed.msm <- msm (hed ~ years, subject=idnum, data = nesarc_expanded, qmatrix = Q, 
                center=FALSE, control = list(trace=1, maxit=500, fnscale = 3000000),
                covariates = ~ female_w1 + age7 + edu3 + race_w1)  # For functions to work, the order of covariates should be: sex, age, edu, race
saveRDS(hed.msm, paste0(models, "hed.msm_new.RDS"))

