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
DATE <- 07112022

dat.mup <- data.table(read.csv("data_acp_MUP_7112022.csv", na.strings = c("NA", "")))

# --------------------------------------------------------------------------------------

dat.mup[, table(ref, mp_bev)]
dat.mup[, table(ref, out_period)]
dat.mup[out_period %like% "long", table(region, mp_bev)]

#exclude "alcohol" estimates in Anderson et al and Xhurxhi et al

data <- dat.mup[ref %like% "Taylor|Brien" | ref %like% "Anderson|Xhurxhi" & mp_bev != "alcohol"]

# get weighted estimates for the multiple assessments in O'Brien et al.

OBrien_short <- rma.uni(yi = perc.change, sei = se, method = "FE", data = data[ref %like% "Brien" & out_period %like% "short"])
OBrien_short

OBrien_long <- rma.uni(yi = perc.change, sei = se, method = "FE", data = data[ref %like% "Brien" & out_period %like% "long"])
OBrien_long

data.OBrien <- dat.mup[ref %like% "Brien"] %>%
  group_by(out_period) %>%
  filter(row_number() == 1) %>%
  as.data.table

data.OBrien[out_period %like% "short",]$perc.change <- OBrien_short[[1]]
data.OBrien[out_period %like% "short",]$se <- OBrien_short[[3]]

data.OBrien[out_period %like% "long",]$perc.change <- OBrien_long[[1]]
data.OBrien[out_period %like% "long",]$se <- OBrien_long[[3]]

data <- rbind(data[ref != "O'Brien et al_2021"], data.OBrien)

# label

data[, lab := ifelse(is.na(region), paste0(ref, ": ", mp_bev, " (", country, ")"), paste0(ref, ": ", mp_bev, " (", region, ")"))]
data <- data[order(data$perc.change), ]

# ----------------------------------------------------------------
# META ANALYSIS MUP - short term effect
# ----------------------------------------------------------------

# main model
# note: RTD Wales Anderson et al_2021 excluded as effect = 0, se = 0

meta.mup.short <- rma.uni(yi = perc.change, sei = se, slab = lab, method = "DL", data = data[out_period %like% "short" & se>0])
meta.mup.short

forest(meta.mup.short)

# publication bias
regtest(meta.mup.short, model="rma")
funnel(meta.mup.short)

# leave-one-out
leave1out(meta.mup.short)

# sensitivity analysis: include alcoholic beverage as moderator

meta.mup.short.sens <- rma.uni(yi = perc.change, sei = se, slab = lab, mods = ~ as.factor(mp_bev) + as.factor(design_level), method = "DL", data = data[out_period %like% "short" & se > 0])
summary(meta.mup.short.sens)
regtest(meta.mup.short.sens, model="rma")

# sensitivity analysis: ROB
meta.mup.short.sens2 <- rma.uni(yi = perc.change, sei = se, slab = lab, mods = ~ rob, method = "DL", data = data[out_period %like% "short" & se > 0])
summary(meta.mup.short.sens2)
regtest(meta.mup.short.sens2, model="rma")


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
meta.mup.long.sens <- rma.uni(yi = perc.change, sei = se, slab = lab, mods = ~ rob, method = "DL", data = data[out_period %like% "long"])
summary(meta.mup.long.sens)
regtest(meta.mup.long.sens, model="rma")



