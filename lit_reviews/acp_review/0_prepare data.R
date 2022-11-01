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

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------
# ----------------------------------------------------------------

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/lit_reviews/ACP/")
DATE <- 29092022

rdat.tax <- data.table(read.csv("raw data/rdata_acp_TAX_29092022.csv", na.strings = "."))
rdat.mup <- data.table(read.csv("raw data/rdata_acp_MUP_29092022.csv", na.strings = "."))
rdat.tav <- data.table(read.csv("raw data/rdata_acp_TAV_29092022.csv", na.strings = "."))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# 1) ALCOHOL TAXATION
# ----------------------------------------------------------------
# ----------------------------------------------------------------

dat.tax <- rdat.tax[dat_include == 1, .(id, ref, study_type, design_level, country, region, tax_bev, tax_change, tax_base_NC, tax_base_ref, int_year, out_calc, out_level, out_period, out_base, out_fu, cons_base, cons_sd, comparator, subgroup, n, effect_size, se, lb, ub, p, t)]

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

# estimate relative change for Freeman 2011

dat.tax[ref %like% "Freeman", tax.perc := tax_base_NC / (tax_base_NC + 1)]
dat.tax[ref %like% "Freeman", abs.change := effect_size / (tax.perc * 100)]
dat.tax[ref %like% "Freeman",  perc.change := abs.change / cons_base * 100]

# ----------------------------------------------------------------
# ESTIMATING STANDARD ERRORS
# ----------------------------------------------------------------

dat.tax[is.na(se), table(out_calc, ref)]

# estimate standard error if confidence interval is known (formula 1)

dat.tax[is.na(se) & !is.na(lb), table(n)] # all n ≥ 50
#dat.tax[out_calc %like% "relative" & is.na(se) & !is.na(lb), test := abs(ub - lb)]
dat.tax[out_calc %like% "relative" & is.na(se) & !is.na(lb), se := abs(ub - lb) / (1.96 * 2)]

# estimate standard error if t statistic is known (formula 4)

dat.tax[out_calc %like% "relative" & is.na(se) & is.na(lb) & !is.na(t), se := abs(perc.change) / abs(t)]

# estimate standard error if relative consumption change was estimated (formula 5, 6)

dat.tax[out_calc %like% "absolute" & !is.na(se) & is.na(cons_sd), table(ref)]
dat.tax[out_calc %like% "absolute" & !is.na(se) & !is.na(cons_sd), table(ref)]
dat.tax[out_calc %like% "absolute" & is.na(se) & is.na(cons_sd), table(ref)]
dat.tax[out_calc %like% "absolute" & is.na(se) & !is.na(cons_sd), table(ref)]

dat.tax[out_calc %like% "absolute" & !is.na(se) & is.na(cons_sd), se := se / cons_base] 

dat.tax[out_calc %like% "absolute" & is.na(se) & !is.na(cons_sd), se.beta := abs(perc.change) / qnorm(1-(as.numeric(p)/2))] 
dat.tax[out_calc %like% "absolute" & is.na(se) & !is.na(cons_sd), se.y := cons_sd / sqrt(n)]
dat.tax[out_calc %like% "absolute" & is.na(se) & !is.na(cons_sd), se := ((se.beta^2) + (se.y^2) + ((effect_size / cons_base)^2)) / cons_base]

# check for missings

dat.tax[is.na(se), table(out_calc, ref)]

# ----------------------------------------------------------------
# DRINKERS ONLY CORRECTION
# ----------------------------------------------------------------
# (Subbaraman et al 2020: alcohol use prevalence = 0.6397)

dat.tax[ref %like% "Subbaraman", perc.change := perc.change * 0.6397]

# ----------------------------------------------------------------
# 1.1) ALCOHOL TAXATION BY SUBGROUP
# ----------------------------------------------------------------

dat.tax.sub <- rdat.tax[dat_include == 2, .(id, ref, study_type, design_level, country, region, tax_bev, tax_change, tax_base_NC, tax_base_ref, int_year, out_calc, out_level, out_period, out_base, out_fu, cons_base, cons_sd, comparator, subgroup, n, effect_size, se, lb, ub, p, t)]

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
dat.tax.sub[out_calc %like% "absolute" & !is.na(se) & !is.na(cons_sd), se := ((se^2) + (se.y^2) + ((effect_size / cons_base)^2)) / cons_base]

# ----------------------------------------------------------------
# DRINKERS ONLY CORRECTION
# ----------------------------------------------------------------
#(Subbaraman et al 2020: alcohol use prevalence = 0.6397)

dat.tax.sub[ref %like% "Subbaraman", perc.change := perc.change * 0.6397]

# ----------------------------------------------------------------
# MERGE AND EXPORT
# ----------------------------------------------------------------

dat.tax[, sub := 0]
dat.tax.sub[, sub := 1]

dat.tax.comb <- rbind(dat.tax[,.(id, ref, study_type, design_level, country, region, tax_bev, tax_base_NC, tax_base_ref, tax_change, int_year, out_level, out_period, out_base, out_fu, comparator, subgroup, n, perc.change, se, p, sub)], 
                      dat.tax.sub[,.(id, ref, study_type, design_level, country, region, tax_bev, tax_base_NC, tax_base_ref, tax_change, int_year, out_level, out_period, out_base, out_fu, comparator, subgroup, n, perc.change, se, p, sub)]) 

write.csv(dat.tax.comb, paste0("data_acp_TAX_", DATE, ".csv"))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# 2) MINIMUM UNIT PRICING
# ----------------------------------------------------------------
# ----------------------------------------------------------------

dat.mup <- rdat.mup[dat_include == 1, .(id, ref, study_type, design_level, country, region, mp_bev, mp_level, mp_unit, int_year, out_calc, out_level, out_period, out_base, out_fu, cons_base, cons_base_se, comparator, subgroup, n, effect_size, se, lb, ub, p, t)]

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
dat.mup[out_calc %like% "absolute" & is.na(se) & !is.na(lb), se.beta := abs(ub - lb) / 1.96 * 2] 
dat.mup[out_calc %like% "absolute" & is.na(se) & is.na(cons_base_se), se := se.beta / cons_base] 

# ----------------------------------------------------------------
# EXPORT
# ----------------------------------------------------------------

write.csv(dat.mup[,.(id, ref, study_type, design_level, country, region, mp_bev, mp_level, mp_unit, int_year, out_level, out_period, out_base, out_fu, comparator, subgroup, n, perc.change, se, p)],
          paste0("data_acp_MUP_", DATE, ".csv"))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# 3) TEMPORAL AVAILABILITY
# ----------------------------------------------------------------
# ----------------------------------------------------------------

dat.tav <- rdat.tav[dat_include == 1, .(id, ref, study_type, design_level, country, region, intervention, int_type, int_direction, int_year, out_bev, out_calc, out_level, out_period, out_base, out_fu, cons_base, comparator, subgroup, n, effect_size, se, lb, ub, p, t)]

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

# estimate standard error if t statistic is known (formula 4)

dat.tav[out_calc %like% "relative" & is.na(se) & is.na(lb) & !is.na(t), se := abs(perc.change) / abs(t)]

# estimate standard error if relative consumption change was estimated, SE(y) not known (formula 5)

dat.tav[out_calc %like% "absolute", se.beta := se]
dat.tav[out_calc %like% "absolute", se := se.beta / cons_base]

# ----------------------------------------------------------------
# EXPORT
# ----------------------------------------------------------------

write.csv(dat.tav[,.(id, ref, study_type, design_level, country, region, int_type, int_direction, int_year, out_bev, out_level, out_period, out_base, out_fu, comparator, subgroup, n, perc.change, se, p)],
          paste0("data_acp_TAV_", DATE, ".csv"))
