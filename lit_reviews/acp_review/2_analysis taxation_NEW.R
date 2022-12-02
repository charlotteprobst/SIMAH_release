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
library(dmetar)
library(esc)
library(PerformanceAnalytics)
library(ggthemes)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------
# ----------------------------------------------------------------

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/lit_reviews/ACP/")
DATE <- 29112022

dat.tax <- data.table(read.csv("data_acp_TAX_29112022.csv", na.strings = c("NA", "")))
gdp <- data.table(read.csv("gdp_ppp_29092022.csv", na.strings = ""))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# DATA PREPARATION
# ----------------------------------------------------------------

# GDP

gdp.long <- melt(gdp, id.vars = c("Country.Name", "Country.Code", "Indicator.Name", "Indicator.Code"))
gdp.long[, year := gsub("X","", as.character(variable))]

gdp.long <- rename(gdp.long, "gdp" = "value")

gdp.sub <- gdp.long[,.(Country.Name, Country.Code, year, gdp)]

# merge

dat.tax <- dat.tax %>% 
  separate(int_year, sep = "-", into = c("min.year", "max.year")) %>%
  mutate(m.year = ifelse(is.na(max.year), min.year,
                         ifelse(!is.na(max.year), round(((as.numeric(min.year) + as.numeric(max.year)) / 2), 0), NA)),
         m.year = ifelse(m.year < 1990, 1990, m.year)) 

dat.tax[country %like% "USA", country := "United States"]
data <- merge(dat.tax[sub == 0, ], gdp.sub, by.x = c("country", "m.year"), by.y = c("Country.Name", "year"), all.x = T)
data[is.na(gdp), table(ref, country)]

# z-standardised gdp

data[, z.gdp := (gdp - mean(gdp)) / sd(gdp)]

# rescale correlation studies to represent 10% tax change 

data[study_type %like% "correlation", perc.change := perc.change / (tax_change * 10)]
data[study_type %like% "correlation", se := se / (tax_change * 10)]
data[study_type %like% "correlation", tax_change := tax_change / (tax_change * 10)]

# period for correlation studies: short

data[study_type %like% "correlation", out_period := "short"]

# reference as factor variable

data[, ref := as.factor(ref)]

# variance

data[, var := se^2]

# policy model

data[, table(ref, tax_bev)] 
data[, beverage := as.factor(ifelse(tax_bev %like% "beer|wine|RTD|spirits", "specific", ifelse(tax_bev %like% "total", "holistic", NA)))]
data[ref %like% "Gehrsitz", beverage := "holistic"]
data[, table(ref, beverage)] 

# identifier

pdat <- data %>% 
  filter(sub == 0 & out_period %like% "short") %>%
  mutate(id = 1:length(perc.change))

# ----------------------------------------------------------------
# META REGRESSION -> needs to be decided whether single- or multi-level model
# ----------------------------------------------------------------

hist(pdat$perc.change)

# main model

metareg <- rma.mv(yi = perc.change,  V = var, slab = ref, mods = ~ tax_change + z.gdp, random = ~ 1 | ref/id, method = "REML", data = pdat)
summary(metareg)

metareg.sens <- rma.uni(yi = perc.change,  sei = se, slab = ref, mods = ~ tax_change + z.gdp + ref, test = "knha", method = "DL", data = pdat)
summary(metareg.sens)

# publication bias

pdf(paste0("figures/", DATE, "_Funnel_TAX.pdf"), width=8, height=6)
funnel(metareg)
dev.off()

# I2

i2 <- var.comp(metareg)
summary(i2) #98.3

# repeat but exclude Heeb et al_2003 and Alexeev et al_2021

metareg.sens2 <- rma.mv(yi = perc.change, V = var, slab = ref, mods = ~ tax_change + z.gdp, random = ~ 1 | ref/id, method = "REML", data = pdat[ref != "Heeb et al_2003" & ref != "Alexeev et al_2021"])
metareg.sens2 # no impact

i2 <- var.comp(metareg.sens2)
summary(i2) #97.9

# investigation of additional covariates

# individual vs. aggregated data

data[, table(ref, design_level)] 
data[, design_level := as.factor(design_level)]

metareg.sens3 <- rma.mv(yi = perc.change, V = var, slab = ref, mods = ~ tax_change + z.gdp + design_level, random = ~ 1 | ref/id, method = "REML", data = pdat)
metareg.sens3 # no impact

i2 <- var.comp(metareg.sens3)
summary(i2) #98.8

# beverage-specific versus holistic policy

metareg.sens4 <- rma.mv(yi = perc.change, V = var, slab = ref, mods = ~ tax_change + z.gdp + beverage, random = ~ 1 | ref/id, method = "REML", data = pdat)
metareg.sens4 # no impact

i2 <- var.comp(metareg.sens4)
summary(i2) #98.7

# ROB

metareg.sens5 <- rma.mv(yi = perc.change, V = var, slab = ref, mods = ~ tax_change + z.gdp + rob, random = ~ 1 | ref/id, method = "REML", data = pdat)
metareg.sens5 # no impact

i2 <- var.comp(metareg.sens5)
summary(i2) #98.8

# ----------------------------------------------------------------
# VISUALISATION
# ----------------------------------------------------------------

# shape

shape <- c(23, 21)

# labels

pdat[, design.ordered := factor(pdat$design_level, levels = c("individual", "aggregate"))]
pdat[, bev.ordered := factor(pdat$beverage, levels = c("specific", "holistic"))]
#pdat[, bev.ordered := factor(pdat$tax_bev, levels = c("beer", "wine", "spirits", "RTD", "total"))]
pdat[, bias := factor(ifelse(rob < 4, 1, ifelse(rob > 3, 0, NA)))]
pdat[, bias.design := factor(ifelse(bias == 1 & design.ordered == "individual", "high risk / individual",
                                 ifelse(bias == 1 & design.ordered == "aggregate", "high risk / aggregate", 
                                        ifelse(bias == 0 & design.ordered == "individual", "low-mid risk / individual",
                                               ifelse(bias == 0 & design.ordered == "aggregate", "low-mid risk / aggregate", NA)))))]
pdat[, inv.var := 1 / (se^2)]

# meta reg prediction

pred.val <- c(-.5, -.4, -.3, -.2, -.1, 0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1, 1.1, 1.2)
pred <- cbind(pred.val, as.data.frame(predict(metareg, newmods=cbind(pred.val, c(0)))))

# plot

ggplot() +
  geom_point(data = pdat, 
             aes(x=tax_change, y=perc.change, size=inv.var, shape=bev.ordered, colour = bias, fill=bias.design)) +
  geom_vline(xintercept = 0, size = .2) +
  geom_hline(yintercept = 0, size = .2) +
  geom_line(data = pred, aes(x=pred.val, y = pred), colour = "#765874") +
  geom_line(data = pred, aes(x=pred.val, y = ci.lb), linetype = "dashed", colour = "#B397AB") +
  geom_line(data = pred, aes(x=pred.val, y = ci.ub), linetype = "dashed", colour = "#B397AB") +
  scale_size(range = c(3,9)) +
  scale_color_manual(values = c("black", "grey"), name = "Independent variable") +
  scale_fill_manual(values = c("grey", NA, "black"), name = "", na.value = "white") +
  scale_shape_manual(values = shape, name = "") +
  scale_x_continuous(breaks = c(-.5, -.25, 0, .25, .5, .75, 1, 1.25), 
                     limits = c(-.5, 1.25), 
                     labels = scales::percent_format(),
                     name = "\nChange in alcohol excise tax") + 
  scale_y_continuous(name = "Change in alcohol consumption\n", 
                     breaks = c(-.2, -.1, 0, .1, .2, .3),
                     limits = c(-.25, .3), 
                     labels = scales::percent_format()) + 
  guides(size = "none", fill = "none", shape = "none", colour = "none") +  
  theme_base() +
  theme(axis.title = element_text(size = 14))

#ggsave(paste0("figures/", DATE, "_Fig 1_TAX2.png"), dpi=300, width = 8, height = 6)

