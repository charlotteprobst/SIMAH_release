# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: ALCOHOL POLICY REVIEW 
## Aim: Data preparation
## Author: Carolin Kilian
## Start Date: 29.09.2022
# ----------------------------------------------------------------
# ----------------------------------------------------------------

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# LIBARIES
# ----------------------------------------------------------------
# ----------------------------------------------------------------

library(tidyverse)
library(data.table)
library(dplyr)
library(powerjoin) 

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------
# ----------------------------------------------------------------

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/lit_reviews/ACP/")
DATE <- 29112022

rdat.tax <- data.table(read.csv("raw data/rdata_acp_TAX_29112022.csv", na.strings = "."))
rdat.mup <- data.table(read.csv("raw data/rdata_acp_MUP_07112022.csv", na.strings = "."))
rdat.tav <- data.table(read.csv("raw data/rdata_acp_TAV_07112022.csv", na.strings = "."))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# 1) ALCOHOL TAXATION
# ----------------------------------------------------------------
# ----------------------------------------------------------------

dat.tax <- rdat.tax[dat_include == 1, .(id, ref, study_type, design_level, country, region, tax_bev, tax_change, tax_base_NC, tax_base_ref, int_year, out_calc, out_level, out_period, out_base, out_fu, cons_base, cons_sd, comparator, subgroup, n, effect_size, se, lb, ub, p, t, rob)]

# ----------------------------------------------------------------
# DATA INSPECTION
# ----------------------------------------------------------------

dat.tax[, table(country, study_type)]
dat.tax[, table(design_level, study_type)]
dat.tax[, table(out_calc, study_type)]

# ----------------------------------------------------------------
# ESTIMATING RELATIVE CONSUMPTION CHANGES
# ----------------------------------------------------------------

# estimate relative change in consumption

dat.tax[out_calc %like% "relative", perc.change := effect_size]
dat.tax[out_calc %like% "absolute", perc.change := effect_size / cons_base]

# estimate relative change for Freeman 2011 and Stehr 2007 (taxes indicated in $)

dat.tax[ref %like% "Freeman", tax_change := tax_base_NC / (tax_base_NC + 1)]
dat.tax[ref %like% "Freeman", abs.change := effect_size / (tax_change * 100)]
dat.tax[ref %like% "Freeman",  perc.change := abs.change / cons_base * 100]

dat.tax[ref %like% "Stehr", tax_change := tax_base_NC / (tax_base_NC + 1)]

# ----------------------------------------------------------------
# ESTIMATING STANDARD ERRORS
# ----------------------------------------------------------------

dat.tax[is.na(se), table(out_calc, ref)]

# estimate standard error if confidence interval is known (formula 1)

dat.tax[is.na(se) & !is.na(lb), table(n)] # all n ≥ 50
dat.tax[out_calc %like% "relative" & is.na(se) & !is.na(lb), se := abs(ub - lb) / (1.96 * 2)]

# estimate standard error if t statistic is known (formula 4)

dat.tax[out_calc %like% "relative" & is.na(se) & is.na(lb) & !is.na(t), se := abs(perc.change) / abs(t)]

# estimate standard error if relative consumption change was estimated (formula 5, 6)

dat.tax[out_calc %like% "absolute" & !is.na(se) & is.na(cons_sd), table(ref)]
dat.tax[out_calc %like% "absolute" & !is.na(se) & !is.na(cons_sd), table(ref)]
dat.tax[out_calc %like% "absolute" & is.na(se) & is.na(cons_sd), table(ref)]
dat.tax[out_calc %like% "absolute" & is.na(se) & !is.na(cons_sd), table(ref)]

dat.tax[out_calc %like% "absolute" & !is.na(se) & is.na(cons_sd), se := se / cons_base] 

dat.tax[out_calc %like% "absolute" & is.na(se) & !is.na(cons_sd), se.beta := abs(effect_size) / qnorm(1-(as.numeric(p)/2))] 
dat.tax[out_calc %like% "absolute" & is.na(se) & !is.na(cons_sd), se.y := cons_sd / sqrt(n)]
dat.tax[out_calc %like% "absolute" & is.na(se) & !is.na(cons_sd), se := sqrt((se.beta^2) + ((se.y^2) * ((effect_size / cons_base)^2))) / cons_base]

# check for missings

dat.tax[is.na(se), table(out_calc, ref)]

# ----------------------------------------------------------------
# 1.1) ALCOHOL TAXATION BY SUBGROUP
# ----------------------------------------------------------------

dat.tax.sub <- rdat.tax[dat_include == 2, .(id, ref, study_type, design_level, country, region, tax_bev, tax_change, tax_base_NC, tax_base_ref, int_year, out_calc, out_level, out_period, out_base, out_fu, cons_base, cons_sd, comparator, subgroup, n, effect_size, se, lb, ub, p, t, rob)]

# ----------------------------------------------------------------
# DATA INSPECTION
# ----------------------------------------------------------------

dat.tax.sub[, table(ref, subgroup)]

# ----------------------------------------------------------------
# ESTIMATING RELATIVE CONSUMPTION CHANGES
# ----------------------------------------------------------------

# estimate relative change in consumption
dat.tax.sub[out_calc %like% "relative", perc.change := effect_size]
dat.tax.sub[out_calc %like% "absolute", perc.change := effect_size / cons_base]

# ----------------------------------------------------------------
# ESTIMATING STANDARD ERRORS
# ----------------------------------------------------------------

dat.tax.sub[is.na(se), table(out_calc, ref)]

# estimate standard error if confidence interval is known (formula 1)

dat.tax.sub[is.na(se) & !is.na(lb), table(n)] # all n ≥ 50
dat.tax.sub[out_calc %like% "relative" & is.na(se) & !is.na(lb), se := abs(ub - lb) / (1.96 * 2)]

# estimate standard error if t statistic is known (formula 4)

dat.tax.sub[out_calc %like% "relative" & is.na(se) & is.na(lb) & !is.na(t), se := abs(perc.change) / abs(t)]

# estimate standard error if relative consumption change was estimated (formula 6)

dat.tax.sub[out_calc %like% "absolute" & !is.na(se), table(ref)]
dat.tax.sub[out_calc %like% "absolute" & is.na(se), table(ref)]
dat.tax.sub[out_calc %like% "absolute" & !is.na(se) & is.na(cons_sd), table(ref)]
dat.tax.sub[out_calc %like% "absolute" & !is.na(se) & !is.na(cons_sd), table(ref)]

dat.tax.sub[out_calc %like% "absolute" & !is.na(se) & !is.na(cons_sd), se.y := cons_sd / sqrt(n)]
dat.tax.sub[out_calc %like% "absolute" & !is.na(se) & !is.na(cons_sd), se := sqrt((se^2) + ((se.y^2) * ((effect_size / cons_base)^2))) / cons_base]

# ----------------------------------------------------------------
# DRINKERS ONLY CORRECTION
# ----------------------------------------------------------------
#(Subbaraman et al 2020: alcohol use prevalence = 0.6397)

dat.tax.sub[ref %like% "Subbaraman" & design_level == "individual", perc.change := perc.change * 0.6397]

# MERGE
dat.tax[, sub := 0]
dat.tax.sub[, sub := 1]

dat.tax.comb <- rbind(dat.tax[,.(id, ref, study_type, design_level, country, region, tax_bev, tax_base_NC, tax_base_ref, tax_change, int_year, out_level, out_period, out_base, out_fu, comparator, subgroup, n, perc.change, se, p, sub, rob)], 
                      dat.tax.sub[,.(id, ref, study_type, design_level, country, region, tax_bev, tax_base_NC, tax_base_ref, tax_change, int_year, out_level, out_period, out_base, out_fu, comparator, subgroup, n, perc.change, se, p, sub, rob)]) 


# ----------------------------------------------------------------
# COMBINE TWO AGE GROUPS Alexeev et al_2021
# ----------------------------------------------------------------

# Function to run fixed-effect model by group

fixed.meta <- function(data){
  
  data <- as.data.table(data)
  m <- length(unique(data$out_period))
  out <- data.frame(matrix(ncol = 3, nrow = m))
  colnames(out) <- c("out_period", "perc.change", "se") 
  
  for (i in 1:m) {
    
    # run fixed-effect model
    model <- rma.uni(yi = perc.change, sei = se, data = data[out_period == unique(data$out_period)[i]], method = "FE")
    sum <- summary(model)
      
    # output
    out$out_period[i] <- unique(data$out_period)[i]
    out$perc.change[i] <- sum[[1]]
    out$se[i] <- sum[[3]]
  }
  return(out)
}

# run meta-analysis model

fmeta <- fixed.meta(dat.tax.comb[ref == "Alexeev et al_2021"])

# combine new fixed-effect data with original data

dat.alexeev <- dat.tax.comb[ref == "Alexeev et al_2021" & subgroup == "25-69"]  
dat.alexeev$subgroup <- "15-69"

dat.alexeev <- power_left_join(dat.alexeev, fmeta, by = "out_period", conflict = rw ~ .y)

# NEW MAIN DATA

dat.tax.comb <- rbind(dat.tax.comb[ref != "Alexeev et al_2021"], dat.alexeev)

# ----------------------------------------------------------------
# EXPORT
# ----------------------------------------------------------------

write.csv(dat.tax.comb, paste0("data_acp_TAX_", DATE, ".csv"))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# 2) MINIMUM UNIT PRICING
# ----------------------------------------------------------------
# ----------------------------------------------------------------

dat.mup <- rdat.mup[dat_include == 1, .(id, ref, study_type, design_level, country, region, mp_bev, mp_level, mp_unit, int_year, out_calc, out_level, out_period, out_base, out_fu, cons_base, cons_base_se, comparator, subgroup, n, effect_size, se, lb, ub, p, t, rob)]

# ----------------------------------------------------------------
# DATA INSPECTION
# ----------------------------------------------------------------

dat.mup[, table(country, study_type)]
dat.mup[, table(design_level, study_type)]
dat.mup[, table(out_calc, study_type)]

# ----------------------------------------------------------------
# ESTIMATING RELATIVE CONSUMPTION CHANGES
# ----------------------------------------------------------------

# estimate relative change in consumption

dat.mup[out_calc %like% "relative", perc.change := effect_size]
dat.mup[out_calc %like% "absolute", perc.change := effect_size / cons_base]

# ----------------------------------------------------------------
# ESTIMATING STANDARD ERRORS
# ----------------------------------------------------------------

# estimate standard error if relative consumption change was estimated (formula 5)

dat.mup[out_calc %like% "absolute" & is.na(se), table(ref)]
dat.mup[out_calc %like% "absolute" & !is.na(se), table(ref)]
dat.mup[out_calc %like% "absolute" & is.na(se) & !is.na(cons_base_se), table(ref)]
dat.mup[out_calc %like% "absolute" & !is.na(se) & !is.na(cons_base_se), table(ref)]

dat.mup[out_calc %like% "absolute" & !is.na(se) & !is.na(cons_base_se), se := ((se^2) + (cons_base_se^2) + ((effect_size / cons_base)^2)) / cons_base]

dat.mup[out_calc %like% "absolute" & is.na(se) & !is.na(lb), table(n)] # all n ≥ 50
dat.mup[out_calc %like% "absolute" & is.na(se) & !is.na(lb) & ref != "O'Brien et al_2021", se.beta := abs(ub - lb) / (1.96 * 2)] #95% confidence interval
dat.mup[out_calc %like% "absolute" & is.na(se) & !is.na(lb) & ref == "O'Brien et al_2021", se.beta := abs(ub - lb) / (2.59 * 2)] #99% confidence interval
dat.mup[out_calc %like% "absolute" & is.na(se) & is.na(cons_base_se), se := se.beta / cons_base] 
dat.mup[out_calc %like% "absolute" & is.na(se) & !is.na(cons_base_se), se := sqrt((se.beta^2) + ((cons_base_se^2) * ((effect_size / cons_base)^2))) / cons_base] 

# ----------------------------------------------------------------
# EXPORT
# ----------------------------------------------------------------

write.csv(dat.mup[,.(id, ref, study_type, design_level, country, region, mp_bev, mp_level, mp_unit, int_year, out_level, out_period, out_base, out_fu, comparator, subgroup, n, perc.change, se, p, rob)],
          paste0("data_acp_MUP_", DATE, ".csv"))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# 3) TEMPORAL AVAILABILITY
# ----------------------------------------------------------------
# ----------------------------------------------------------------

dat.tav <- rdat.tav[dat_include == 1, .(id, ref, study_type, design_level, country, region, intervention, int_type, int_direction, int_year, out_bev, out_calc, out_level, out_period, out_base, out_fu, cons_base, comparator, subgroup, n, effect_size, se, lb, ub, p, t, rob)]

# ----------------------------------------------------------------
# DATA INSPECTION
# ----------------------------------------------------------------

dat.tav[, table(country, study_type)]
dat.tav[, table(design_level, study_type)]
dat.tav[, table(int_type, study_type)]
dat.tav[, table(out_calc, study_type)]

dat.tav <- copy(dat.tav[int_type %like% "day", ])

# ----------------------------------------------------------------
# ESTIMATING RELATIVE CONSUMPTION CHANGES
# ----------------------------------------------------------------

# estimate relative change in consumption

dat.tav[out_calc %like% "relative", perc.change := effect_size]
dat.tav[out_calc %like% "absolute", perc.change := effect_size / cons_base]

# reverse effects for permitting sales

dat.tav[int_direction %like% "liberal", perc.change := perc.change * -1]


# ----------------------------------------------------------------
# ESTIMATING STANDARD ERRORS
# ----------------------------------------------------------------

# data inspection

dat.tav[is.na(se), table(out_calc, ref)]
dat.tav[out_calc %like% "absolute", table(out_calc, ref)]

# estimate standard error if p value is known (formula 3)

dat.tav[out_calc %like% "relative" & is.na(se) & is.na(lb) & !is.na(p), se := abs(perc.change) / (qnorm(1-(as.numeric(p)/2)))]

# estimate standard error if t statistic is known (formula 4)

dat.tav[out_calc %like% "relative" & is.na(se) & is.na(lb) & !is.na(t), se := abs(perc.change) / abs(t)]

# estimate standard error if relative consumption change was estimated, SE(y) not known (formula 5)

dat.tav[out_calc %like% "absolute", se.beta := se]
dat.tav[out_calc %like% "absolute", se := se.beta / cons_base]

# ----------------------------------------------------------------
# EXPORT
# ----------------------------------------------------------------

write.csv(dat.tav[,.(id, ref, study_type, design_level, country, region, int_type, int_direction, int_year, out_bev, out_level, out_period, out_base, out_fu, comparator, subgroup, n, perc.change, se, p, rob)],
          paste0("data_acp_TAV_", DATE, ".csv"))
