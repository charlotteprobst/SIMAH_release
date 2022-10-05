# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: ALCOHOL POLICY REVIEW 
## Aim: Meta regression alcohol taxation
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
library(metafor)
library(meta)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------
# ----------------------------------------------------------------

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/lit_reviews/ACP/")
DATE <- 04102022

dat.tax <- data.table(read.csv("data_acp_TAX_29092022.csv", na.strings = c("NA", "")))
gdp <- data.table(read.csv("gdp_ppp_29092022.csv", na.strings = ""))
int.dollar <- data.table(read.csv("gdp_conversionfactor_29092022.csv", na.strings = ""))
alc <- data.table(read.csv("alc_country level_181009.csv", na.strings = "."))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# 1) META REGRESSION
# ----------------------------------------------------------------
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# DATA PREPARATION
# ----------------------------------------------------------------

# GDP

gdp.long <- melt(gdp, id.vars = c("Country.Name", "Country.Code", "Indicator.Name", "Indicator.Code"))
gdp.long[, year := gsub("X","", as.character(variable))]

gdp.long <- rename(gdp.long, "gdp" = "value")

gdp.sub <- gdp.long[,.(Country.Name, Country.Code, year, gdp)]

# conversion factor

int.dollar.long <- melt(int.dollar, id.vars = c("Country.Name", "Country.Code", "Indicator.Name", "Indicator.Code"))
int.dollar.long[, year := gsub("X","", as.character(variable))]

int.dollar.long <- rename(int.dollar.long, "int.dollar" = "value")

int.dollar.sub <- int.dollar.long[,.(Country.Name, Country.Code, year, int.dollar)]

# merge

dat.tax[country %like% "USA", country := "United States"]
data <- merge(dat.tax[sub == 0 & study_type %like% "intervention", ], gdp.sub, by.x = c("country", "int_year"), by.y = c("Country.Name", "year"), all.x = T)
data[is.na(gdp), table(ref, country)]

data <- merge(data, int.dollar.sub, by.x = c("country", "int_year", "Country.Code"), by.y = c("Country.Name", "year", "Country.Code"), all.x = T)
data[is.na(int.dollar), table(ref, country)]

# pre-intervention tax level in international dollar

data[, table(ref, tax_base_ref)]

data[tax_base_ref %like% "litre 100% alc", tax_base := tax_base_NC]
data[tax_base_ref %like% "gallon 5% alc", tax_base := tax_base_NC * (100/5) / 4.54609]
data[tax_base_ref %like% "gallon 12% alc", tax_base := tax_base_NC * (100/12) / 4.54609]
data[tax_base_ref %like% "gallon 40% alc", tax_base := tax_base_NC * (100/40) / 4.54609]

data[, pre_tax := tax_base / int.dollar]

# prevalence data

#data[, int_year := as.numeric(int_year)]
#data <- merge(data, alc[sex %like% "TOTAL" & age %like% "15_99", .(iso3a, year, CD)], by.x = c("Country.Code", "int_year"), by.y = c("iso3a", "year"))

# ----------------------------------------------------------------
# META REGRESSION
# ----------------------------------------------------------------

# model
metareg.tax <- rma.uni(yi = perc.change, sei = se, slab = ref, mods = ~ tax_change + gdp, method = "REML", data = data[out_period %like% "short"])

metareg.tax
confint(metareg.tax)

regplot(metareg.tax, mod = "tax_change", refline = 0, xlab = "Relative change in alcohol tax", ylab = "Relative change in alcohol consumption", label = "piout", labsize = 0.5) 
regplot(metareg.tax, mod = "gdp", refline = 0, legend=T, xlab = "GDP PPP", ylab = "Relative change in alcohol consumption", label = "piout", labsize = 0.5)

# repeat but exclude Thailand / exclude Switzerland
metareg.sens <- rma.uni(yi = perc.change, sei = se, slab = ref, mods = ~ tax_change + gdp, method = "REML", data = data[out_period %like% "short" & ref != "Sornpaisarn et al_2013"])
metareg.sens
regplot(metareg.sens, mod = "tax_change", refline = 0, xlab = "Relative change in alcohol tax", ylab = "Relative change in alcohol consumption", label = "piout", labsize = 0.5) 

metareg.sens2 <- rma.uni(yi = perc.change, sei = se, slab = ref, mods = ~ tax_change + gdp, method = "REML", data = data[out_period %like% "short" & ref != "Heeb et al_2003"])
metareg.sens2
regplot(metareg.sens2, mod = "tax_change", refline = 0, xlab = "Relative change in alcohol tax", ylab = "Relative change in alcohol consumption", label = "piout", labsize = 0.5) 

# publication bias
regtest(metareg.tax, model="rma")


# ----------------------------------------------------------------
# META ANALYSIS TAX ELASTICITY
# ----------------------------------------------------------------

tax.elast.data <- dat.tax[sub == 0 & study_type %like% "correlation", ]
tax.elast.data[, lab := paste0(ref, ": ", tax_bev)]

# model
meta.tax <- rma.uni(yi = perc.change, sei = se, slab = lab, method = "REML", data = tax.elast.data)

meta.tax
confint(meta.tax)

forest(meta.tax)

# publication bias
regtest(meta.tax, model="rma")

# leave-one-out
leave1out(meta.tax)
