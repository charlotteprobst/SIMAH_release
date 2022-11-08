# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: ALCOHOL POLICY REVIEW 
## Aim: Meta analysis Temporal Availability
## Author: Carolin Kilian
## Start Date: 03.11.2022
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
library(meta)
library(metafor)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------
# ----------------------------------------------------------------

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/lit_reviews/ACP/")
DATE <- 07112022

dat.tav <- data.table(read.csv("data_acp_TAV_7112022.csv", na.strings = c("NA", "")))

# --------------------------------------------------------------------------------------

dat.tav[, table(ref, out_period)]
dat.tav[, table(ref, design_level)]
dat.tav[, table(ref, out_bev)]

#exclude "alcohol" estimates in Anderson et al and Xhurxhi et al

data <- dat.tav[ref %like% "Carpenter|Grönqvist|Stehr" | ref %like% "Norström|Yörük" & out_bev != "alcohol"]
data.short <- data[out_period %like% "short"]
  
# label

data.short[, lab := paste0(ref, ": ", out_bev)]
data.short <- data.short[order(data.short$perc.change), ]

# ----------------------------------------------------------------
# META ANALYSIS 
# ----------------------------------------------------------------

# main model

meta.tav <- rma.uni(yi = perc.change, sei = se, slab = lab, method = "DL", data = data.short)
meta.tav

forest(meta.tav)

# publication bias
regtest(meta.tav, model="rma")
funnel(meta.tav)

# leave-one-out
leave1out(meta.tav)

# sensitivity analysis: rob
meta.tav.sens <- rma.uni(yi = perc.change, sei = se, slab = lab, mods = ~ rob, method = "DL", data = data.short)
summary(meta.tav.sens)
regtest(meta.tav.sens, model="rma")
