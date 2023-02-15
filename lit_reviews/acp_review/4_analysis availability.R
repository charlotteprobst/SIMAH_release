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
library(dmetar)
library(ggh4x)

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

# variance 

data.short[, var := se^2]

# levels

data.short <- data.short %>% 
  mutate(id = 1:length(perc.change),
         ref = as.factor(ref))


# ----------------------------------------------------------------
# META ANALYSIS 
# ----------------------------------------------------------------

# main model

meta.tav <- rma.uni(yi = perc.change, sei = se, slab = lab, method = "DL", data = data.short)
meta.tav

pdf(paste0("figures/", DATE, "_Forest_TAV.pdf"), width=8, height=6)
forest(meta.tav)
dev.off()

# publication bias
regtest(meta.tav, model="rma")
pdf(paste0("figures/", DATE, "_Funnel_TAV.pdf"), width=8, height=6)
funnel(meta.tav)
dev.off()

# leave-one-out
leave1out(meta.tav)

# sensitivity analysis: include alcoholic beverage as covariate
data.short[, table(out_bev)]

meta.tav.sens <- rma.mv(yi = perc.change, V = var, slab = lab, mods = ~ as.factor(out_bev)-1, random = ~ 1 | ref/id, method = "REML", data = data.short)
summary(meta.tav.sens)

i2 <- var.comp(meta.tav.sens)
summary(i2) #76.2


# sensitivity analysis: rob
meta.tav.sens2 <- rma.mv(yi = perc.change, V = var, slab = lab, mods = ~ rob, random = ~ 1 | ref/id, method = "REML", data = data.short)
summary(meta.tav.sens2)

i2 <- var.comp(meta.tav.sens2)
summary(i2) #81.4

# ----------------------------------------------------------------
# VISUALISATION 
# ----------------------------------------------------------------

# confidence intervals for original estimates

data.short <- data.short %>% 
  mutate(ref = ifelse(ref %like% "Carpenter", "Carpenter et al 2009",
                      ifelse(ref %like% "Grönqvist", "Grönqvist et al 2014", 
                             ifelse(ref %like% "Norström", "Norström et al 2005", 
                                    ifelse(ref %like% "Stehr", "Stehr et al 2007", 
                                           ifelse(ref %like% "Yörük", "Yörük et al 2013", NA))))),
         lab = paste0(ref, ":\n", country)) %>%
  group_by(lab) %>%
  arrange(-perc.change) %>%
  mutate(id = 1:length(perc.change),
         lci = perc.change - (1.96*se),
         uci = perc.change + (1.96*se)) %>%
  as.data.table

# new data with pooled estimate

pdat.est <- as.data.frame(matrix(nrow = 1, ncol = 13))
colnames(pdat.est) <- c("id", "lab", "country", "region", "design_level", "out_bev", "int_year", "perc.change", "se", "var", "lci", "uci", "p")
pdat.est <- pdat.est %>%
  mutate(id = max(data.short$id) + 1,
         lab = "Weighted average",
         design_level = "total",
         out_bev = "",
         perc.change = meta.tav$b,
         se = meta.tav$se,
         lci = meta.tav$ci.lb,
         uci = meta.tav$ci.ub,
         p = meta.tav$pval)

# combine files
pdat <- rbind(data.short[,.(id, lab, country, region, design_level, out_bev, int_year, perc.change, se, var, lci, uci, p)], pdat.est)

# labels

pdat[, design.ordered := factor(design_level, levels = c("individual", "aggregate"))]
pdat[, bev.ordered := factor(out_bev, levels = c("RTD", "cider", "spirits", "wine", "beer", "alcohol", ""))]
pdat[, bev.ordered := factor(paste0(lab, "_", out_bev),
                             levels = c("Weighted average_",
                                        "Norström et al 2005:\nSweden_spirits", "Norström et al 2005:\nSweden_wine", "Norström et al 2005:\nSweden_beer",
                                        "Grönqvist et al 2014:\nSweden_alcohol",
                                        "Yörük et al 2013:\nUSA_spirits", "Yörük et al 2013:\nUSA_wine", "Yörük et al 2013:\nUSA_beer",
                                        "Stehr et al 2007:\nUSA_spirits", "Stehr et al 2007:\nUSA_beer",
                                        "Carpenter et al 2009:\nCanada_alcohol"))]
pdat <- separate(pdat, col=bev.ordered, into=c("lab.rest", "bev.lab"), sep="_", remove = F)
pdat[, lab.ordered := factor(lab, levels = c("Carpenter et al 2009:\nCanada",
                                             "Stehr et al 2007:\nUSA",
                                             "Yörük et al 2013:\nUSA",
                                             "Grönqvist et al 2014:\nSweden",
                                             "Norström et al 2005:\nSweden",
                                             "Weighted average"))]
pdat[, est.lab := paste0(format(round(pdat$perc.change*100, 1), digits = 1), "% (",
                         format(round(pdat$lci*100, 1), digits = 2), "%, ", 
                         format(round(pdat$uci*100, 1), digits = 1), "%)")]

# plot

ggplot(data = pdat, aes(x = bev.ordered, y = perc.change)) +
  geom_hline(colour = "grey", yintercept = 0, size = .4) + 
  geom_point(aes(fill = design.ordered, colour = design.ordered, size = design.ordered), shape = 23) +
  geom_errorbar(aes(ymin = lci, ymax = uci, colour = design.ordered), width = 0, size = .4) + 
  geom_hline(colour = "#765874", yintercept = meta.tav$b, linetype = 2, size = .4) +
  scale_x_discrete(breaks = pdat$bev.ordered, labels = pdat$bev.lab) +
  scale_y_continuous(breaks = c(-.2, -.15, -.1, -.05, 0, 0.05, 0.1), limits = c(-.2, 0.1), 
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

#ggsave(paste0("figures/", DATE, "_Fig 4_TAV.png"), dpi=300, width = 10, height = 6)

