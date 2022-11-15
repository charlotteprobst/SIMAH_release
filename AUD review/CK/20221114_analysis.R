# ========================================================================================================
# ========================================================================================================

# Alcohol Use Disorder dose response meta-analysis
# Tessa & Carolin 
# Start date: 20221114

# ========================================================================================================
# ========================================================================================================

# Load libraries 

library(tidyverse)  # data management
library(readxl)     # import excel data
library(janitor)    # Edit data formatting
library(skimr)      # descriptive statistics
library(tableone)   # create table one
library(data.table) # enable data table
library(powerjoin)  # extension of dplyr package for powerjoin

library(dosresmeta)
library(mvtnorm)
library(ellipse)
library(mvmeta)
library(rms)
library(meta)
library(metafor)
library(rmeta)

# Date

DATE <- 20221115

# Specify the data and output file locations

file_location <- "/Users/carolinkilian/Desktop/SIMAH_workplace/AUD review/Data/"   # Location of data
output <- "/Users/carolinkilian/Desktop/SIMAH_workplace/AUD review/"        # Location of figures/tables

# Load data

## COMMENT FOR TESSA: I renamed the file so that we hopefully do not mess up with the different files -> this one is the one of today (indicated by the date)

data <- read_xlsx(paste0(file_location, "20221115_analysis_data_AUD_mortality.xlsx"), na="")

# Prepare data 
# Add logRR, loglowerRR, logupperRR, se, inverse_se columns

## COMMENT FOR TESSA: we have to remove the following line to not exclude the Russian study at this time "filter(!str_detect(group, "age"))"
## COMMENT FOR TESSA: %>% as.data.table says that data1 is now a data.table

data1 <- data %>%
  mutate(
    logRR = log(RR),
    loglowerRR = log(lowerRR),
    logupperRR = log(upperRR),
    se = ifelse(upperRR!=1 & lowerRR!=1, (logupperRR - loglowerRR)/3.92, NA),
    inver_se = 1/se) %>%
  as.data.table

  
  
# ========================================================================================================
  
# Fixed-effect model to combine estimates for Zaridze 2009

# Function to run fixed-effect model by group

fixed.meta <- function(data){
  
  data <- as.data.table(data)
  m <- length(unique(data$alc_daily_g))
  out <- data.frame(matrix(ncol = 11, nrow = m))
  colnames(out) <- c("alc_daily_g", "total_n", "outcome_n", "RR", "lowerRR", "upperRR", "logRR", "loglowerRR", "logupperRR", "se", "inver_se") 
  
  for (i in 1:m) {
    
    out$alc_daily_g[i] <- unique(data$alc_daily_g)[i]
    subgroup <- unique(data$alc_daily_g)[i]
    
    ## COMMENT FOR TESSA: we also have to update total_n and outcome_n so this code is new, see also changes above related to defining the out dataframe
    
    # new total_n and outcome_n
    out$total_n[i] <- sum(data[alc_daily_g == subgroup]$total_n)
    out$outcome_n[i] <- sum(data[alc_daily_g == subgroup]$outcome_n)
    
    ## COMMENT FOR TESSA: I've now integrated the condition for being a reference category into the function 
      
      if (data$lowerRR[i] != 1 & data$upperRR[i] != 1) {
         
        # run fixed-effect model
        model <- rma.uni(yi = logRR, sei = se, data = data[alc_daily_g == subgroup & RR != 0], method = "FE") # COMMENT FOR TESSA: some studies have a RR = 0 leading to infinite values after logarithmizing, we have to remove these observations
        sum <- summary(model)
        
        # log output
        out$logRR[i] <- sum[[1]]
        out$loglowerRR[i] <- sum[[6]]
        out$logupperRR[i] <- sum[[7]]
        out$se[i] <- sum[[3]]
        out$inver_se[i] <- 1 / out$se[i]
        
        # run prediction
        predict <- predict(model, transf = exp)
        
        # exp output
        out$RR[i] <- predict[[1]]
        out$lowerRR[i] <- predict[[3]]
        out$upperRR[i] <- predict[[4]]
    }
    
    else if (data$lowerRR[i] == 1 & data$upperRR[i] == 1) {

      # log output
      out$logRR[i] <- 0
      out$loglowerRR[i] <- 0
      out$logupperRR[i] <- 0
      out$se[i] <- NA
      out$inver_se[i] <- NA
      
      # exp output
      out$RR[i] <- 1
      out$lowerRR[i] <- 1
      out$upperRR[i] <- 1
      
    }
    
    else {
      
      paste0("ERROR")
      
    }
  }
  return(out)
}

# run meta-analysis model

fmeta <- fixed.meta(data1[first_author == "Zaridze"])

# combine new fixed-effect data with original data

dat.zaridze <- data1[first_author == "Zaridze" & group == "Men ages 15-54"]  
dat.zaridze$group <- "All participants"

dat.zaridze <- power_left_join(dat.zaridze, fmeta, by = "alc_daily_g", conflict = rw ~ .y)

# NEW MAIN DATA

data_all <- rbind(data1[first_author != "Zaridze"], dat.zaridze)


# ========================================================================================================

dose5 <- rma(yi = logRR, sei = se, data = data1[c(10,15),], method = "FE", )
predict(dose5, transf = exp)

# select subsample
data_all <- data1 %>%
  filter(group == "All participants")

#Create scatter plot
ggplot(data_all, aes(alc_daily_g, RR, size=inver_se)) + 
  scale_y_continuous(trans = log2_trans()) +
  geom_point(shape=1, colour="black") + scale_size_area(max_size=20)

#Create linear model
lin_mod <- dosresmeta(formula=logRR ~ alc_daily_g, proc="1stage",
                      id=id_study, type="ir", se=se, cases=outcome_n, n=total_n, data=data_all)

summary(lin_mod)
predict(lin_mod, delta=10, exp=TRUE)

#Plot
predict(lin_mod, data.frame(alc_daily_g=seq(0, 150, 1)), order=TRUE, exp=TRUE) %>% 
  ggplot(aes(x=alc_daily_g, y=pred)) +  scale_y_continuous(trans = log2_trans()) + 
  geom_line() + 
  geom_ribbon(aes(ymin= ci.lb, ymax=ci.ub), alpha=0.1) + 
  coord_cartesian(ylim=c(-5, 200))+
  theme_bw()

#Create quadratic model 
quad_mod <- dosresmeta(formula=logRR ~ alc_daily_g + I(alc_daily_g^2), proc= "1stage", id=id_study, type="cc", se=se, cases=outcome_n, n=total_n, data=data_all)

summary(quad_mod)

predict(quad_mod, data.frame(alc_daily_g=seq(0, 150, 1)), order=TRUE, exp=TRUE) %>% 
  ggplot(aes(x=alc_daily_g, y=pred)) +  geom_line() + 
  geom_ribbon(aes(ymin= ci.lb, ymax=ci.ub), alpha=0.1) + 
  coord_cartesian(ylim=c(-5, 200))+
  theme_bw()


#to remove any data file rm()