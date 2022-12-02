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
library(ggthemes)
library(ggh4x)
library(dmetar)

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

# variance

data[, var := se^2]

# beverages

data[, beverage := ifelse(mp_bev %like% "RTD|cider", "other", mp_bev)]
data[, beverage := factor(beverage, levels = c("alcohol", "beer", "wine", "spirits", "other"))]
data[, table(mp_bev, beverage)]

# ----------------------------------------------------------------
# META ANALYSIS MUP - short term effect
# ----------------------------------------------------------------

# levels

data.short <- data %>% 
  filter(out_period %like% "short" & se>0) %>%
  arrange(ref) %>%
  mutate(id = 1:length(perc.change),
         ref = as.factor(ref)) %>%
  arrange(perc.change)

# main model
# note: RTD Wales Anderson et al_2021 excluded as effect = 0, se = 0

meta.mup.short <- rma.uni(yi = perc.change, sei = se, slab = lab, method = "DL", data = data.short)
meta.mup.short

pdf(paste0("figures/", DATE, "_Forest_MUPShort.pdf"), width=12, height=6)
forest(meta.mup.short)
dev.off()

# publication bias
regtest(meta.mup.short, model="rma") #.219
pdf(paste0("figures/", DATE, "_Funnel_MUPShort.pdf"), width=8, height=6)
funnel(meta.mup.short)
dev.off()

# leave-one-out
leave1out(meta.mup.short)

# sensitivity analysis (without intercept): include alcoholic beverage as covariate

meta.mup.short.sens <- rma.mv(yi = perc.change, V = var, slab = lab, mods = ~ as.factor(beverage) -1, random = ~ 1 | ref/id, method = "REML", data = data.short)
summary(meta.mup.short.sens)

i2 <- var.comp(meta.mup.short.sens)
summary(i2) #98.8

# sensitivity analysis (without intercept): include alcoholic use assessment as covariate

meta.mup.short.sens2 <- rma.mv(yi = perc.change, V = var, slab = lab, mods = ~ as.factor(design_level) -1, random = ~ 1 | ref/id, method = "REML", data = data.short)
summary(meta.mup.short.sens2)

i2 <- var.comp(meta.mup.short.sens2)
summary(i2) #99.9

# sensitivity analysis: ROB

meta.mup.short.sens3 <- rma.mv(yi = perc.change, V = var, slab = lab, mods = ~ rob, random = ~ 1 | ref/id, method = "REML", data = data.short)
summary(meta.mup.short.sens3)

i2 <- var.comp(meta.mup.short.sens3)
summary(i2) #99.9

# ----------------------------------------------------------------
# META ANALYSIS MUP - long term effect
# ----------------------------------------------------------------

meta.mup.long <- rma.uni(yi = perc.change, sei = se, slab = lab, method = "DL", data = data[out_period %like% "long"])
meta.mup.long

pdf(paste0("figures/", DATE, "_Forest_MUPLong.pdf"), width=12, height=6)
forest(meta.mup.long)
dev.off()

# publication bias
regtest(meta.mup.long, model="rma")
pdf(paste0("figures/", DATE, "_Funnel_MUPLong.pdf"), width=8, height=6)
funnel(meta.mup.long)
dev.off()

# leave-one-out
leave1out(meta.mup.long)

# sensitivity analysis: rob
meta.mup.long.sens <- rma.mv(yi = perc.change, V = var, slab = lab, mods = ~ rob, random = ~ 1 | ref/id, method = "REML", data = data[out_period %like% "long"])
summary(meta.mup.long.sens)

i2 <- var.comp(meta.mup.long.sens)
summary(i2) #99.3


# ----------------------------------------------------------------
# VISUALISATION - SHORT
# ----------------------------------------------------------------

# confidence intervals for original estimates

data.short <- data.short %>% 
  mutate(ref = ifelse(ref %like% "Anderson", "Anderson et al 2021",
                      ifelse(ref %like% "Brien", "O'Brien et al 2021",
                             ifelse(ref %like% "Taylor", "Taylor et al 2021", 
                                    ifelse(ref %like% "Xhurxhi", "Xhurxhi et al 2020", NA)))),
         country = ifelse(country == "United Kingdom", "UK", ifelse(country == "Australia", "AUS", country)),
         region = ifelse(region == "Darwin/Palmerston area", "DPA Northern Territory", region),
         lab = paste0(ref, ":\n", region, " (", country, ")")) %>%
  group_by(lab) %>%
  arrange(-perc.change) %>%
  mutate(id = 1:length(perc.change),
         lci = perc.change - (1.96*se),
         uci = perc.change + (1.96*se)) %>%
  as.data.table

# new data with pooled estimate

pdat.est <- as.data.frame(matrix(nrow = 1, ncol = 13))
colnames(pdat.est) <- c("id", "lab", "country", "region", "design_level", "mp_bev", "int_year", "perc.change", "se", "var", "lci", "uci", "p")
pdat.est <- pdat.est %>%
  mutate(id = max(data.short$id) + 1,
         lab = "Weighted average",
         design_level = "total",
         mp_bev = "",
         perc.change = meta.mup.short$b,
         se = meta.mup.short$se,
         lci = meta.mup.short$ci.lb,
         uci = meta.mup.short$ci.ub,
         p = meta.mup.short$pval)

# combine files
pdat <- rbind(data.short[,.(id, lab, country, region, design_level, mp_bev, int_year, perc.change, se, var, lci, uci, p)], pdat.est)

# labels

pdat[, design.ordered := factor(design_level, levels = c("individual", "aggregate"))]
pdat[, bev.ordered := factor(mp_bev, levels = c("RTD", "cider", "spirits", "wine", "beer", "alcohol", ""))]
pdat[, bev.ordered := factor(paste0(lab, "_", mp_bev),
                             levels = c("Weighted average_",
                                        "Anderson et al 2021:\nWales (UK)_cider", "Anderson et al 2021:\nWales (UK)_spirits", "Anderson et al 2021:\nWales (UK)_wine", "Anderson et al 2021:\nWales (UK)_beer",
                                        "Xhurxhi et al 2020:\nScotland (UK)_cider", "Xhurxhi et al 2020:\nScotland (UK)_spirits", "Xhurxhi et al 2020:\nScotland (UK)_wine", "Xhurxhi et al 2020:\nScotland (UK)_beer",
                                        "Anderson et al 2021:\nScotland (UK)_RTD", "Anderson et al 2021:\nScotland (UK)_cider", "Anderson et al 2021:\nScotland (UK)_spirits", "Anderson et al 2021:\nScotland (UK)_wine", "Anderson et al 2021:\nScotland (UK)_beer",
                                        "O'Brien et al 2021:\nNorthern Territory (AUS)_alcohol",
                                        "Taylor et al 2021:\nDPA Northern Territory (AUS)_wine", "Taylor et al 2021:\nDPA Northern Territory (AUS)_alcohol"))]
pdat <- separate(pdat, col=bev.ordered, into=c("lab.rest", "bev.lab"), sep="_", remove = F)
pdat[, lab.ordered := factor(lab, levels = c("Taylor et al 2021:\nDPA Northern Territory (AUS)",
                                             "O'Brien et al 2021:\nNorthern Territory (AUS)",
                                             "Anderson et al 2021:\nScotland (UK)",
                                             "Xhurxhi et al 2020:\nScotland (UK)",
                                             "Anderson et al 2021:\nWales (UK)",
                                             "Weighted average"))]
pdat[, est.lab := paste0(format(round(pdat$perc.change*100, 1), digits = 1), "% (",
                         format(round(pdat$lci*100, 1), digits = 2), "%, ", 
                         format(round(pdat$uci*100, 1), digits = 1), "%)")]

# plot

ggplot(data = pdat, aes(x = bev.ordered, y = perc.change)) +
  geom_hline(colour = "grey", yintercept = 0, size = .4) + 
  geom_point(aes(fill = design.ordered, colour = design.ordered, size = design.ordered), shape = 23) +
  geom_errorbar(aes(ymin = lci, ymax = uci, colour = design.ordered), width = 0, size = .4) + 
  geom_hline(colour = "#765874", yintercept = meta.mup.short$b, linetype = 2, size = .4) +
  scale_x_discrete(breaks = pdat$bev.ordered, labels = pdat$bev.lab) +
  scale_y_continuous(breaks = c(-.6, -.5, -.4, -.3, -.2, -.1, 0, 0.1), limits = c(-.6, 0.12), 
                     labels = scales::percent_format(),
                     name = "\nChange in alcohol consumption") +
  scale_fill_manual(values = c("black", "white"), name = "", na.value = "#765874") +
  scale_colour_manual(values = c("black", "black"), name = "", na.value = "#765874") +
  scale_size_manual(values = c(3,3), na.value = 4) +
  guides(size = "none", fill = "none", shape = "none", colour = "none",
         y.sec = guide_axis_manual(breaks = pdat$bev.ordered, labels = pdat$est.lab)) +
  facet_grid(rows = vars(lab.ordered), scales = "free_y", space = "free", switch = "y") +
  xlab("") + 
  theme_base() +
  theme(axis.title = element_text(size = 14),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        axis.text.y.right = element_text(vjust = 0.5),
        axis.ticks.y = element_blank(),
        panel.border = element_blank()) + 
  coord_flip(clip = "off")

#ggsave(paste0("figures/", DATE, "_Fig 2_MUPShort.png"), dpi=300, width = 10, height = 8)


# ----------------------------------------------------------------
# VISUALISATION - LONG
# ----------------------------------------------------------------

# confidence intervals for original estimates

data.long <- data[out_period %like% "long"] %>% 
  mutate(ref = ifelse(ref %like% "Anderson", "Anderson et al 2021",
                      ifelse(ref %like% "Brien", "O'Brien et al 2021", NA)),
         country = ifelse(country == "United Kingdom", "UK", ifelse(country == "Australia", "AUS", country)),
         lab = paste0(ref, ":\n", region, " (", country, ")")) %>%
  group_by(lab) %>%
  arrange(-perc.change) %>%
  mutate(id = 1:length(perc.change),
         lci = perc.change - (1.96*se),
         uci = perc.change + (1.96*se)) %>%
  as.data.table

# new data with pooled estimate

pdat.est <- as.data.frame(matrix(nrow = 1, ncol = 13))
colnames(pdat.est) <- c("id", "lab", "country", "region", "design_level", "mp_bev", "int_year", "perc.change", "se", "var", "lci", "uci", "p")
pdat.est <- pdat.est %>%
  mutate(id = max(data.long$id) + 1,
         lab = "Weighted average",
         design_level = "total",
         mp_bev = "",
         perc.change = meta.mup.long$b,
         se = meta.mup.long$se,
         lci = meta.mup.long$ci.lb,
         uci = meta.mup.long$ci.ub,
         p = meta.mup.long$pval)

# combine files
pdat <- rbind(data.long[,.(id, lab, country, region, design_level, mp_bev, int_year, perc.change, se, var, lci, uci, p)], pdat.est)

# labels

pdat[, design.ordered := factor(design_level, levels = c("individual", "aggregate"))]
pdat[, bev.ordered := factor(mp_bev, levels = c("RTD", "cider", "spirits", "wine", "beer", "alcohol", ""))]
pdat[, bev.ordered := factor(paste0(lab, "_", mp_bev),
                             levels = c("Weighted average_",
                                        "Anderson et al 2021:\nScotland (UK)_RTD", "Anderson et al 2021:\nScotland (UK)_cider", "Anderson et al 2021:\nScotland (UK)_spirits", "Anderson et al 2021:\nScotland (UK)_wine", "Anderson et al 2021:\nScotland (UK)_beer",
                                        "O'Brien et al 2021:\nNorthern Territory (AUS)_alcohol"))]
pdat <- separate(pdat, col=bev.ordered, into=c("lab.rest", "bev.lab"), sep="_", remove = F)
pdat[, lab.ordered := factor(lab, levels = c("O'Brien et al 2021:\nNorthern Territory (AUS)",
                                             "Anderson et al 2021:\nScotland (UK)",
                                             "Weighted average"))]
pdat[, est.lab := paste0(format(round(pdat$perc.change*100, 1), digits = 1), "% (",
                         format(round(pdat$lci*100, 1), digits = 2), "%, ", 
                         format(round(pdat$uci*100, 1), digits = 1), "%)")]

# plot

ggplot(data = pdat, aes(x = bev.ordered, y = perc.change)) +
  geom_hline(colour = "grey", yintercept = 0, size = .4) + 
  geom_point(aes(fill = design.ordered, colour = design.ordered, size = design.ordered), shape = 23) +
  geom_errorbar(aes(ymin = lci, ymax = uci, colour = design.ordered), width = 0, size = .4) + 
  geom_hline(colour = "#765874", yintercept = meta.mup.long$b, linetype = 2, size = .4) +
  scale_x_discrete(breaks = pdat$bev.ordered, labels = pdat$bev.lab) +
  scale_y_continuous(breaks = c(-.6, -.5, -.4, -.3, -.2, -.1, 0, 0.1), limits = c(-.6, 0.12), 
                     labels = scales::percent_format(),
                     name = "\nChange in alcohol consumption") +
  scale_fill_manual(values = c("black", "white"), name = "", na.value = "#765874") +
  scale_colour_manual(values = c("black", "black"), name = "", na.value = "#765874") +
  scale_size_manual(values = c(3,3), na.value = 4) +
  guides(size = "none", fill = "none", shape = "none", colour = "none",
         y.sec = guide_axis_manual(breaks = pdat$bev.ordered, labels = pdat$est.lab)) +
  facet_grid(rows = vars(lab.ordered), scales = "free_y", space = "free", switch = "y") +
  xlab("") + 
  theme_base() +
  theme(axis.title = element_text(size = 14),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        axis.text.y.right = element_text(vjust = 0.5),
        axis.ticks.y = element_blank(),
        panel.border = element_blank()) + 
  coord_flip(clip = "off")

#ggsave(paste0("figures/", DATE, "_Fig 3_MUPLong.png"), dpi=300, width = 10, height = 4)

