# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: ALCOHOL POLICY REVIEW 
## Aim: Explorative data visualisation
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
library(ggplot2)
library(ggforce)
library(dplyr)
library(hrbrthemes)
library(viridis)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------
# ----------------------------------------------------------------

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/lit_reviews/ACP/")
DATE <- 01112022

dat.tax <- data.table(read.csv("data_acp_TAX_1112022.csv"))
dat.mup <- data.table(read.csv("data_acp_MUP_1112022.csv"))
dat.tav <- data.table(read.csv("data_acp_TAV_1112022.csv"))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# GGPLOT STYLE VECTORS
# ----------------------------------------------------------------
# ----------------------------------------------------------------

tax.shape <- c(22, 23, 24, 25, 21)
elasticity.shape <- c(22, 24, 21)
mup.shape <- c(22, 23, 24, 25, 4, 21)
tav.shape <- c(22, 23, 24, 21)

c.tax.ref <- c("#B7CF85", "#9CC674", "#599755", "#EFCE00", "#C65E5B", "#C5DCFF", "#9BAADF", "#565C80", "#CFA5FF", "#A070BF", "#6F4980")
c.tax.lab <- c("#B7CF85", "#9CC674", "#EFCE00", "#C5DCFF", "#9BAADF", "#565C80", "#CFA5FF", "#A070BF", "#6F4980")
c.elasticity.ref <- c("#4F8F00", "#76D6FF", "#0096FF", "#005493", "#D783FF", "#4E1B7F")
c.elasticity.lab <- c("#76D6FF", "#0096FF", "#D783FF")
c.tax.sex <- c("#C9C671", "#98A5C8", "#849F87")
c.mup.ref <- c("#9CC674", "#76D6FF", "#002060")
c.mup.lab <- c("#9CC674", "#76D6FF")
c.tav.ref <- c("#009193", "#B0936E", "#774A13", "#CFA5FF", "#6F4980")
c.tav.lab <- c("#B0936E", "#774A13", "#CFA5FF", "#6F4980")

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# 1) ALCOHOL TAXATION
# ----------------------------------------------------------------
# ----------------------------------------------------------------

# labels

dat.tax[is.na(region), lab := paste0(country, " ", int_year)]
dat.tax[!is.na(region), lab := paste0(country, "/", region, " ", int_year)]
dat.tax[, lab := ifelse(tax_bev %like% "total", paste0(lab, ": ", "alcohol taxes"), paste0(lab, ": ", tax_bev, " tax"))]
dat.tax[, lab.fill := ifelse(design_level %like% "aggregate", lab, NA)]
dat.tax[, bev.ordered := factor(dat.tax$tax_bev, levels = c("beer", "wine", "spirits", "RTD", "total"))]
dat.tax[, ref.ordered := factor(dat.tax$ref, levels = rev(sort(unique(dat.tax$ref))))]

# ----------------------------------------------------------------
# A) INTERVENTION STUDIES
# ----------------------------------------------------------------

ggplot(dat.tax[study_type %like% "intervention" & sub == 0], aes(x=tax_change, y=perc.change, size=1/se)) +
  geom_point(aes(shape = bev.ordered, color=as.factor(lab), fill=as.factor(lab.fill)), alpha=0.7) +
  geom_vline(xintercept = 0, size = .2) +
  geom_hline(yintercept = 0, size = .2) +
  scale_size(range = c(3,8)) +
  scale_color_manual(values = c.tax.ref, name = "Independent variable") +
  scale_fill_manual(values = c.tax.lab, name = "", na.value = "white") +
  scale_shape_manual(values = tax.shape, name = "Dependent variable", labels = c("beer consumption", "wine consumption", "spirits consumption", "spirits-based RTD consumption", "alcohol consumption")) +
  scale_x_continuous(breaks = c(-.5, -.25, 0, .25, .5, .75, 1, 1.25), 
                     limits = c(-.5, 1.25), 
                     labels = scales::percent_format()) + 
  scale_y_continuous(name = "Change in alcohol consumption\n", 
                     breaks = c(-.3, -.2, -.1, 0, .1, .2, .3),
                     limits = c(-.3, .3), 
                     labels = scales::percent_format()) + 
  guides(size = "none", fill = "none", shape = guide_legend(order = 2), col = guide_legend(order = 1)) +  
  theme(legend.position="right") +
  xlab("\nChange in alcohol excise tax") +
  theme_bw()

#ggsave(paste0("figures/Fig 1_TAX_", DATE, ".png"), dpi=300, width = 10, height = 7)

# ----------------------------------------------------------------
# B) TAX ELASTICITY
# ----------------------------------------------------------------

dat.tax[study_type %like% "correlation", lab]
dat.tax[study_type %like% "correlation", lab.ordered := factor(dat.tax[study_type %like% "correlation"]$lab, 
                                                               levels = c( "USA 1984-2009: alcohol taxes", "USA 1970-2007: beer tax", "USA 1990-2004: beer tax", "USA 2000-2013: beer tax", "USA 1990-2004: spirits tax", "USA 2000-2013: spirits tax"))]
dat.tax[study_type %like% "correlation", ref.ordered := factor(dat.tax[study_type %like% "correlation"]$ref, 
                                                               levels = c("An et al_2011", "Freeman_2011", "Stehr et al_2007", "Subbaraman et al_2020"))]


ggplot(dat.tax[study_type %like% "correlation"  & sub == 0], aes(x=perc.change, y=ref.ordered, size=1/se)) +
  geom_point(aes(shape = bev.ordered, color=as.factor(lab.ordered), fill=as.factor(lab.fill)), alpha=0.7) +
  geom_vline(xintercept = 0, size = .2) +
  scale_size(range = c(3,8)) +
  scale_x_continuous(breaks = c(-.5, -.4, -.3, -.2, -.1, 0, .1), 
                     limits = c(-.5, .05),
                     labels = scales::percent_format()) + 
  scale_color_manual(values = c.elasticity.ref, name = "Independent variable") +
  scale_fill_manual(values = c.elasticity.lab, name = "", na.value = "white") +
  scale_shape_manual(values = elasticity.shape, name = "Dependent variable", labels = c("beer consumption", "spirits consumption", "alcohol consumption")) +
  guides(size = "none", fill = "none", shape = guide_legend(order = 2),col = guide_legend(order = 1)) +  
  xlab("\nChange in alcohol consumption") +
  ylab("") +
  theme(legend.position="right") +
  theme_bw()

#ggsave(paste0("figures/Fig 2_TAX ELASTICITY_", DATE, ".png"), dpi=300, width = 8, height = 5)

# ----------------------------------------------------------------
# c) TAXATION BY RACE/ETHNICITY
# ----------------------------------------------------------------

dat.tax.sub <- dat.tax[sub == 1 & ref %like% "An et al|Subbaraman",]

# labels
dat.tax.sub[, x.lab := ifelse(subgroup %like% "White", "White", ifelse(subgroup %like% "African", "Black", 
                                                                        ifelse(subgroup %like% "Hispanic", "Hispanic", "Other")))]
dat.tax.sub[, x.lab := factor(dat.tax.sub$x.lab, levels = c("White", "Black", "Hispanic", "Other"))]
dat.tax.sub[, sex := ifelse(subgroup %like% "women", "women", ifelse(subgroup %like% "men", "men", "total"))]
dat.tax.sub[, sex := factor(dat.tax.sub$sex, levels = c("total", "men", "women"))]
dat.tax.sub[, lab.fill := ifelse(ref %like% "Subbaraman", NA, sex)]
dat.tax.sub[, tax.ordered := factor(dat.tax.sub$tax_bev, levels = c("total", "beer", "spirits"))]

ggplot(dat.tax.sub, aes(x=x.lab, y=perc.change, size=1/se)) +
  geom_point(aes(shape = tax.ordered, color=as.factor(sex), fill=as.factor(lab.fill)), alpha=0.7) +
  geom_hline(yintercept = 0, size = .2) +
  scale_size(range = c(1,6)) +
  scale_color_manual(values = c.tax.sex, name = "Sex") +
  scale_fill_manual(values = c.tax.sex, name = "", na.value = "white") +
  scale_shape_manual(values = elasticity.shape, name = "Independent variable", labels = c("alcohol tax", "beer tax", "spirits tax")) +
  scale_y_continuous(name = "Change in alcohol consumption\n", 
                     breaks = c(-.8, -.6, -.4, -.2, 0, .2), 
                     limits = c(-.8, .2), 
                     labels = scales::percent_format()) + 
  guides(size = "none", fill = "none", shape = guide_legend(order = 1), col = guide_legend(order = 2)) +  
  theme(legend.position="right") +
  xlab("") +
  theme_bw()

#ggsave(paste0("figures/Fig 3_TAX_RACE_", DATE, ".png"), dpi=300, width = 6, height = 4)

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# 2) MINIMUM UNIT PRICING
# ----------------------------------------------------------------
# ----------------------------------------------------------------

# MUP per 10 grams pure alcohol in international dollars

dat.mup[, mup := mp_level / mp_unit * 10]
dat.mup[country %like% "Australia", mup.stand := mup / 1.48]
dat.mup[country %like% "United Kingdom", mup.stand := mup / 0.69]

# labels

dat.mup[region %like% "Darwin", region := "DPA"]
dat.mup[!is.na(region), lab := paste0(country, "/", region, " ", int_year, ": Int$", round(mup.stand, 2))]
dat.mup[is.na(region), lab := paste0(country, " ", int_year, ": Int$", round(mup.stand, 2))]
dat.mup[, lab.fill := ifelse(design_level %like% "aggregate", lab, NA)]
dat.mup[, bev.ordered := factor(dat.mup$mp_bev, levels = c("beer", "wine", "spirits", "RTD", "cider", "alcohol"))]
dat.mup[, ref.ordered := factor(dat.mup$ref, levels = rev(sort(unique(dat.mup$ref))))]

# summary plot intervention studies

ggplot(dat.mup[study_type %like% "intervention"], aes(x=perc.change, y=ref.ordered, size=1/se)) +
  geom_point(aes(shape = bev.ordered, color=as.factor(lab), fill=as.factor(lab.fill)), alpha=0.7) +
  geom_vline(xintercept = 0, size = .2) +
  scale_size(range = c(3,8)) +
  scale_x_continuous(breaks = c(-0.6, -0.5, -.4, -.3, -.2, -.1, 0, .1), 
                     limits = c(-0.6, .1),
                     labels = scales::percent_format()) + 
  scale_color_manual(values = c.mup.ref, name = "Independent variable") +
  scale_fill_manual(values = c.mup.lab, name = "", na.value = "white") +
  scale_shape_manual(values = mup.shape, name = "Dependent variable", labels = c("beer consumption", "wine consumption", "spirits consumption", "RTD consumption", "cider consumption", "alcohol consumption")) +
  guides(size = "none", fill = "none", shape = guide_legend(order = 2),col = guide_legend(order = 1)) +  
  xlab("\nChange in alcohol consumption") +
  ylab("") +
  theme(legend.position="right") +
  theme_bw()

#ggsave(paste0("figures/Fig 4_MUP_", DATE, ".png"), dpi=300, width = 8, height = 4)


# ----------------------------------------------------------------
# ----------------------------------------------------------------
# 3) TEMPORAL AVAILABILITY
# ----------------------------------------------------------------
# ----------------------------------------------------------------

# labels

dat.tav[country %like% "Sweden", region := "selected counties"]
dat.tav[country %like% "USA", region := "selected states"]
dat.tav[!is.na(region), lab := paste0(country, "/", region, " ", int_year)]
dat.tav[is.na(region), lab := paste0(country, " ", int_year)]
dat.tav[, lab.fill := ifelse(design_level %like% "aggregate", lab, NA)]
dat.tav[, bev.ordered := factor(dat.tav$out_bev, levels = c("beer", "wine", "spirits", "alcohol"))]
dat.tav[, ref.ordered := factor(dat.tav$ref, levels = rev(sort(unique(dat.tav$ref))))]

# summary plot intervention studies

ggplot(dat.tav, aes(x=perc.change, y=ref.ordered, size=1/se)) +
  geom_point(aes(shape = bev.ordered, color=as.factor(lab), fill=as.factor(lab.fill)), alpha=0.7) +
  geom_vline(xintercept = 0, size = .2) +
  scale_size(range = c(3,8)) +
  scale_x_continuous(breaks = c(-.1, -.075, -.05, -.025, 0, .025), 
                     limits = c(-.1, .025),
                     labels = scales::percent_format()) + 
  scale_color_manual(values = c.tav.ref, name = "Independent variable") +
  scale_fill_manual(values = c.tav.lab, name = "", na.value = "white") +
  scale_shape_manual(values = tav.shape, name = "Dependent variable", labels = c("beer consumption", "wine consumption", "spirits consumption", "alcohol consumption")) +
  guides(size = "none", fill = "none", shape = guide_legend(order = 2),col = guide_legend(order = 1)) +  
  xlab("\nChange in alcohol consumption") +
  ylab("") +
  theme(legend.position="right") +
  theme_bw()


#ggsave(paste0("figures/Fig 5_TAV_", DATE, ".png"), dpi=300, width = 7, height = 5)

