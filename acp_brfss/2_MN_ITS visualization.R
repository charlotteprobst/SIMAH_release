# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: Sunday sales ban  
## State: Minnesota
## Author: Carolin Kilian
## Start Date: 11/08/2023
# ----------------------------------------------------------------
# ----------------------------------------------------------------

# --------------------------------------------------------------------------------------

rm(list = ls())

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# LIBARIES
# ----------------------------------------------------------------
# ----------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(openxlsx)
#library(forecast)
library(mgcv)
library(smooth)
library(ggpubr)

# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230925

# prepared data file 
data <- read.csv("acp_brfss/data/20230712_AGEST_MORTALITY_MINNESOTA.csv")

# covariates
datUNEMP <- read.csv("acp_brfss/data/20230712_UNEMP_MINNESOTA.csv")

# combine data
pdat <- left_join(data, datUNEMP) %>% 
  mutate(z.unemp.rate = (log(unemp.rate) - mean(log(unemp.rate))) / sd(log(unemp.rate)))

# set plot design

ggdesign <- theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 12), strip.text = element_text(size = 12),
        axis.text = element_text(size = 12, color = "black"), 
        axis.title.y = element_text(size = 12, color = "black"), 
        axis.title.x = element_blank(), axis.ticks = element_blank())

colobs <- "#1D9A6C"
colfit <- "#0A2F51"

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# SELECT DATA
# ----------------------------------------------------------------

datMLEHS <- pdat %>% filter(edclass == "LEHS" & sex == 1 & year < 2020) %>% 
  mutate(qnum = 1:nrow(.), eff.level = ifelse(QYEAR < 2017.625, F, T), eff.slope = ifelse(QYEAR <= 2017.625, 0, qnum - 71))
datMSomeC <- pdat %>% filter(edclass == "SomeC" & sex == 1 & year < 2020) %>% 
  mutate(qnum = 1:nrow(.), eff.level = ifelse(QYEAR < 2017.625, F, T), eff.slope = ifelse(QYEAR <= 2017.625, 0, qnum - 71))
datMCollege <- pdat %>% filter(edclass == "College" & sex == 1 & year < 2020) %>% 
  mutate(qnum = 1:nrow(.), eff.level = ifelse(QYEAR < 2017.625, F, T), eff.slope = ifelse(QYEAR <= 2017.625, 0, qnum - 71))
datWLEHS <- pdat %>% filter(edclass == "LEHS" & sex == 2 & year < 2020) %>% 
  mutate(qnum = 1:nrow(.), eff.level = ifelse(QYEAR < 2017.625, F, T), eff.slope = ifelse(QYEAR <= 2017.625, 0, qnum - 71))
datWSomeC <- pdat %>% filter(edclass == "SomeC" & sex == 2 & year < 2020) %>% 
  mutate(qnum = 1:nrow(.), eff.level = ifelse(QYEAR < 2017.625, F, T), eff.slope = ifelse(QYEAR <= 2017.625, 0, qnum - 71))
datWCollege <- pdat %>% filter(edclass == "College" & sex == 2 & year < 2020) %>% 
  mutate(qnum = 1:nrow(.), eff.level = ifelse(QYEAR < 2017.625, F, T), eff.slope = ifelse(QYEAR <= 2017.625, 0, qnum - 71))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# GET BEST FITTING MODELS
# ----------------------------------------------------------------

# MEN LEHS
TS.MLEHS <- gam(AAMrate ~ s(qnum, bs="cs", k=9) + eff.slope, data = datMLEHS, method = "ML")

# MEN SOMEC
TS.MSomeC <- lm(AAMrate ~ qnum + z.unemp.rate + eff.level, data = datMSomeC)

# MEN COLLEGE
TS.MCollege <- gam(AAMrate ~ s(Q, bs="cc", k=4) + eff.level + eff.slope, data = datMCollege, method = "ML")

# WOMEN LEHS
TS.WLEHS <- lm(AAMrate ~ qnum + + eff.slope, data = datWLEHS)

# WOMEN SOMEC
TS.WSomeC <- lm(AAMrate ~ qnum + eff.slope, data = datWSomeC)

# WOMEN COLLEGE
TS.WCollege <- lm(AAMrate ~ qnum + eff.level, data = datWCollege)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# VISUALISATION
# ----------------------------------------------------------------

# COMBINED DATAFRAME
pdat <- cbind(datMLEHS$QYEAR, datMLEHS$AAMrate, TS.MLEHS$fitted.values,
              datMSomeC$AAMrate, TS.MSomeC$fitted.values, datMCollege$AAMrate, TS.MCollege$fitted.values,
              datWLEHS$AAMrate, TS.WLEHS$fitted.values, datWSomeC$AAMrate, TS.WSomeC$fitted.values,
              datWCollege$AAMrate, TS.WCollege$fitted.values) %>% data.frame()
colnames(pdat) <- c("QYEAR", "MLEHSobs", "MLEHSfit", "MSOMECobs", "MSOMECfit", "MCOLLEGEobs", "MCOLLEGEfit", 
                    "WLEHSobs", "WLEHSfit", "WSOMECobs", "WSOMECfit", "WCOLLEGEobs", "WCOLLEGEfit")

# MEN LEHS
FigMLEHS <- ggplot(pdat) + geom_point(aes(x = QYEAR, y = MLEHSobs), color = colobs) +
  geom_line(aes(x = QYEAR, y = MLEHSfit), color = colfit) +
  geom_vline(xintercept=2017.5, color = "black", linetype = "dashed") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "\n", y = "\nAge-standardized mortality rate (100% AA)\n") + ggdesign

# MEN SOMEC
FigMSOMEC <- ggplot(pdat) + geom_point(aes(x = QYEAR, y = MSOMECobs), color = colobs) +
  geom_line(aes(x = QYEAR, y = MSOMECfit), color = colfit) +
  geom_vline(xintercept=2017.5, color = "black", linetype = "dashed") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "\n", y = "\nAge-standardized mortality rate (100% AA)\n") + ggdesign

# MEN COLLEGE
FigMCOLLEGE <- ggplot(pdat) + geom_point(aes(x = QYEAR, y = MCOLLEGEobs), color = colobs) +
  geom_line(aes(x = QYEAR, y = MCOLLEGEfit), color = colfit) +
  geom_vline(xintercept=2017.5, color = "black", linetype = "dashed") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "\n", y = "\nAge-standardized mortality rate (100% AA)\n") + ggdesign

# WOMEN LEHS
FigWLEHS <- ggplot(pdat) + geom_point(aes(x = QYEAR, y = WLEHSobs), color = colobs) +
  geom_line(aes(x = QYEAR, y = WLEHSfit), color = colfit) +
  geom_vline(xintercept=2017.5, color = "black", linetype = "dashed") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "\n", y = "\nAge-standardized mortality rate (100% AA)\n") + ggdesign

# WOMEN SOMEC
FigWSOMEC <- ggplot(pdat) + geom_point(aes(x = QYEAR, y = WSOMECobs), color = colobs) +
  geom_line(aes(x = QYEAR, y = WSOMECfit), color = colfit) +
  geom_vline(xintercept=2017.5, color = "black", linetype = "dashed") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "\n", y = "\nAge-standardized mortality rate (100% AA)\n") + ggdesign

# MEN COLLEGE
FigWCOLLEGE <- ggplot(pdat) + geom_point(aes(x = QYEAR, y = WCOLLEGEobs), color = colobs) +
  geom_line(aes(x = QYEAR, y = WCOLLEGEfit), color = colfit) +
  geom_vline(xintercept=2017.5, color = "black", linetype = "dashed") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "\n", y = "\nAge-standardized mortality rate (100% AA)\n") + ggdesign

# COMBINE
ggarrange(FigMLEHS, FigMSOMEC, FigMCOLLEGE, FigWLEHS, FigWSOMEC, FigWCOLLEGE,
          labels = c("1A: MEN, low", "2A: MEN, medium", "3A: MEN, high", 
                     "1B: WOMEN, low", "2B: WOMEN, medium", "3B: WOMEN, high"),
          ncol = 3, nrow = 2)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# EXPORT
# ----------------------------------------------------------------

ggsave(paste0("acp_brfss/outputs/figures/", DATE, "_TS_MN.jpg"), dpi = 300, width = 12, height = 8)
