# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: ALCOHOL POLICY REVIEW 
## Aim: Meta analysis MUP
## Author: Carolin Kilian
## Start Date: 19.10.2022
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
DATE <- 19102022

dat.mup <- data.table(read.csv("data_acp_MUP_1112022.csv", na.strings = c("NA", "")))

# --------------------------------------------------------------------------------------

dat.mup[, table(ref, out_period)]
dat.mup[out_period %like% "long", table(region, mp_bev)]

#exclude "alcohol" estimates in Anderson et al and Xhurxhi et al

data <- dat.mup[ref %like% "Taylor" | ref %like% "Anderson|Xhurxhi" & mp_bev != "alcohol"]

# label

data[, lab := ifelse(is.na(region), paste0(ref, ": ", mp_bev, " (", country, ")"), paste0(ref, ": ", mp_bev, " (", region, ")"))]

# ----------------------------------------------------------------
# META ANALYSIS MUP - short term effect
# ----------------------------------------------------------------

# main model

meta.mup.short <- rma.uni(yi = perc.change, sei = se, slab = lab, method = "DL", data = data[out_period %like% "short" & se > 0])
meta.mup.short

forest(meta.mup.short)

# publication bias
regtest(meta.mup.short, model="rma")
funnel(meta.mup.short)

# leave-one-out
leave1out(meta.mup.short)

# sensitivity analysis: include alcoholic beverage as moderator

data[, bev := ifelse(mp_bev %like% "cider|RTD|alcohol", "other", mp_bev)]
data[, bev := factor(bev, levels = c("beer", "wine", "spirits", "other"))]

meta.mup.short.sens <- rma.uni(yi = perc.change, sei = se, slab = lab, mods = ~ bev, method = "DL", data = data[out_period %like% "short" & se > 0])
summary(meta.mup.short.sens)

# sensitivity analysis: rob


# ----------------------------------------------------------------
# META ANALYSIS MUP - long term effect
# ----------------------------------------------------------------

meta.mup.long <- rma.uni(yi = perc.change, sei = se, slab = lab, method = "DL", data = data[out_period %like% "long"])
meta.mup.long

forest(meta.mup.long)

# publication bias
regtest(meta.mup.long, model="rma")
funnel(meta.mup.long)

# leave-one-out
leave1out(meta.mup.long)

# sensitivity analysis: rob