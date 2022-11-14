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

DATE <- 20221114

# Specify the data and output file locations

file_location <- "/Users/carolinkilian/Desktop/SIMAH_workplace/AUD review/Data/"   # Location of data
output <- "/Users/carolinkilian/Desktop/SIMAH_workplace/AUD review/"        # Location of figures/tables

# Load data

data <- read_xlsx(paste0(file_location, "Analysis_data_AUD_mortality.xlsx"), na="")

# Prepare data 
# Add logRR, loglowerRR, logupperRR, se, inverse_se columns

data1 <- data %>%
  mutate(
    logRR = log(RR),
    loglowerRR = log(lowerRR),
    logupperRR = log(upperRR),
    se = ifelse(upperRR!=1 & lowerRR!=1, (logupperRR - loglowerRR)/3.92, NA),
    inver_se = 1/se) %>%
  filter(!str_detect(group, "age"))

# ========================================================================================================
  
# Fixed-effect model to combine estimates for Zaridze 2009

# select data

pdat <- data1 %>%
  filter(first_author == "Zaridze" & group != "All participants")

# function to run fixed-effect model by group

fixed.meta <- function(data){
  data <- as.data.table(copy(data))
  m <- length(unique(data$alc_daily_g)) - 1
  out <- data.frame(matrix(ncol = 9, nrow = m))
  colnames(out) <- c("alc_daily_g", "RR", "lowerRR", "upperRR", "logRR", "loglowerRR", "logupperRR", "se", "inver_se")
  for (i in 1:m) {
      out$alc_daily_g[i] <- unique(data$alc_daily_g)[i+1]
      subgroup <- unique(data$alc_daily_g)[i+1]
      
      # run fixed-effect model
      model <- rma.uni(yi = logRR, sei = se, data = data[alc_daily_g == subgroup], method = "FE")
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
  return(out)
}

# run meta-analysis model

fmeta <- fixed.meta(pdat)

# combine new fixed-effect data with original data
  
data1 <- as.data.table(copy(data1))

pdat2 <- power_left_join(data1[first_author == "Zaridze" & group == "All participants" & alc_daily_g > 0], 
                         fmeta, by = "alc_daily_g", conflict = rw ~ .y)

dat.zaridze <- rbind(data1[first_author == "Zaridze" & group == "All participants" & alc_daily_g == 0], pdat2)

# new main data

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