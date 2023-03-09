# extract TPs from NESARC alcohol models
gc()
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(ggplot2)
library(metafor)

setwd("~/Google Drive/SIMAH Sheffield/")

msm1.old <- readRDS("SIMAH_workplace/nesarc/Models/alc4_age7.msm.RDS")
msm1.new <- readRDS("SIMAH_workplace/nesarc/Models/msm3b.RDS")

dataold <- msm1.old$data$mf
datanew <- msm1.new$data$mf

mappingold <- expand.grid(female_wave1.factor=unique(dataold$female_wave1.factor),
                          age7 = unique(dataold$age7),
                          edu3.factor = unique(dataold$edu3.factor),
                          race_wave1.factor = unique(dataold$race_wave1.factor))

mappingnew <- expand.grid(female_w1 = unique(datanew$female_w1),
                          age7 = unique(datanew$age7),
                          edu3 = unique(datanew$edu3),
                          race_w1 = unique(datanew$race_w1))

probold <- extractTP_old(msm1.old, mappingold)

probnew <- extractTP_new(msm1.new, mappingnew)

write.csv(probold, "SIMAH_workplace/microsim/1_input_data/alcohol_transitions_old.csv", row.names=F)
write.csv(probnew, "SIMAH_workplace/microsim/1_input_data/alcohol_transitions_new.csv", row.names=F)
