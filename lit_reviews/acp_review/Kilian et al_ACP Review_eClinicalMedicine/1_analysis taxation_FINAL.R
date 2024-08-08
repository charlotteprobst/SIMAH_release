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
DATE <- 20230222

dat.tax <- data.table(read.csv("data_acp_TAX_20230222.csv", na.strings = c("NA", "")))
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
# META REGRESSION 
# ----------------------------------------------------------------

hist(pdat$perc.change)

# main model (random intercept)

metareg <- rma.mv(yi = perc.change,  V = var, slab = ref, mods = ~ tax_change + z.gdp, random = ~ 1 | ref/id, method = "REML", data = pdat)
summary(metareg)
predict(metareg, newmods = c(1, 0))

# I2
i2 <- var.comp(metareg)
summary(i2) #98.4

# publication bias
pdf(paste0("figures/", DATE, "_Funnel_TAX.pdf"), width=8, height=6)
funnel(metareg)
dev.off()

# SA1 (covariate model)

metareg.sens <- rma.uni(yi = perc.change,  sei = se, slab = ref, mods = ~ tax_change + z.gdp + ref, test = "knha", method = "DL", data = pdat)
summary(metareg.sens) # I2 = 31.7%

# SA2: tax increase only

metareg.sens2 <- rma.mv(yi = perc.change, V = var, slab = ref, mods = ~ tax_change + z.gdp, random = ~ 1 | ref/id, method = "REML", data = pdat[tax_change >= 0])
metareg.sens2 # no impact

#I2
i2 <- var.comp(metareg.sens2)
summary(i2) #98.5

# investigation of additional covariates

# SA3: individual vs. aggregated data

data[, table(ref, design_level)] 
data[, design_level := as.factor(design_level)]

metareg.sens3 <- rma.mv(yi = perc.change, V = var, slab = ref, mods = ~ tax_change + z.gdp + as.factor(design_level), random = ~ 1 | ref/id, method = "REML", data = pdat)
metareg.sens3 # no impact

#I2
i2 <- var.comp(metareg.sens3)
summary(i2) #98.9

# SA4: beverage-specific versus holistic policy

metareg.sens4 <- rma.mv(yi = perc.change, V = var, slab = ref, mods = ~ tax_change + z.gdp + as.factor(beverage), random = ~ 1 | ref/id, method = "REML", data = pdat)
metareg.sens4 # no impact

#I2
i2 <- var.comp(metareg.sens4)
summary(i2) #98.8

# SA5: excluding those with critical ROB
metareg.sens5 <- rma.mv(yi = perc.change, V = var, slab = ref, mods = ~ tax_change + z.gdp, random = ~ 1 | ref/id, method = "REML", data = pdat[rob == 0])
metareg.sens5 # no impact

#I2
i2 <- var.comp(metareg.sens5)
summary(i2) #99.1

# SA6: exclude with repeated cross-sectional study design
metareg.sens6 <- rma.mv(yi = perc.change, V = var, slab = ref, mods = ~ tax_change + z.gdp, random = ~ 1 | ref/id, method = "REML", data = pdat[study_design %like% "longitudinal"])
metareg.sens6 # no impact

#I2
i2 <- var.comp(metareg.sens6)
summary(i2) #92.5

# ----------------------------------------------------------------
# VISUALISATION
# ----------------------------------------------------------------

# shape

shape <- c(24, 23)

# labels

pdat[, bev.ordered := factor(ifelse(study_type %like% "intervention" & beverage %like% "holistic" & rob == 0, "holistic/low", 
                                    ifelse(study_type %like% "intervention" & beverage %like% "holistic" & rob == 1, "holistic/high", 
                                          ifelse(study_type %like% "intervention" & beverage %like% "specific" & rob == 0, "specific/low",
                                                 ifelse(study_type %like% "intervention" & beverage %like% "specific" & rob == 1, "specific/high",
                                                        ifelse(study_type %like% "correlation" & rob == 0, "correlation/low", 
                                                               ifelse(study_type %like% "correlation" & rob == 1, "correlation/high", NA)))))), 
                             levels = c("specific/low", "specific/high", "holistic/low", "holistic/high", "correlation/low", "correlation/high"))]
pdat[, tax.policy := as.factor(beverage)]
pdat[, study_type := as.factor(study_type)]
pdat[, rob := as.factor(rob)]
pdat[, inv.var := 1 / (se^2)]

# meta reg prediction

pred.val <- c(-.5, -.4, -.3, -.2, -.1, 0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1, 1.1, 1.2)
pred <- cbind(pred.val, as.data.frame(predict(metareg, newmods=cbind(pred.val, c(0)))))

# plot

ggplot() +
  geom_point(data = pdat, 
             aes(x=tax_change, y=perc.change, size=inv.var, shape=tax.policy, fill=bev.ordered, color=rob)) +
  geom_vline(xintercept = 0, size = .2) +
  geom_hline(yintercept = 0, size = .2) +
  geom_line(data = pred, aes(x=pred.val, y = pred), colour = "#765874") +
  geom_line(data = pred, aes(x=pred.val, y = ci.lb), linetype = "dashed", colour = "#B397AB") +
  geom_line(data = pred, aes(x=pred.val, y = ci.ub), linetype = "dashed", colour = "#B397AB") +
  geom_line(data = pred, aes(x=pred.val, y = pi.lb), linetype = "dotted", colour = "grey") +
  geom_line(data = pred, aes(x=pred.val, y = pi.ub), linetype = "dotted", colour = "grey") +
  scale_size(range = c(3,9)) +
  scale_color_manual(values = c("black", "grey"), name = "") +
  scale_fill_manual(values = c("grey", "black", "grey", NA, NA), name = "", na.value = "white") +
  scale_shape_manual(values = shape, name = "") +
  scale_x_continuous(breaks = c(-.5, -.25, 0, .25, .5, .75, 1, 1.25), 
                     limits = c(-.5, 1.25), 
                     labels = scales::percent_format(),
                     name = "\nChanges in alcohol excise tax") + 
  scale_y_continuous(name = "Changes in alcohol consumption\n", 
                     breaks = c(-.2, -.1, 0, .1, .2, .3),
                     limits = c(-.25, .3), 
                     labels = scales::percent_format()) + 
  guides(size = "none", fill = "none", shape = "none", colour = "none") +  
  theme_base() +
  theme(axis.title = element_text(size = 14))

#ggsave(paste0("figures/", DATE, "_Fig 1_TAX.png"), dpi=500, width = 8, height = 6)

