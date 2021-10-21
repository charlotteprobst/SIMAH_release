####SIMAH OCT 2021 brfss processing - reading in the raw data files 
# BRFSS data 1999- 2020
library(foreign)
library(SASxport)
library(readr)
library(dplyr)
library(tidyr)

wd <- "~/Google Drive/SIMAH Sheffield/"
setwd(wd)

####read in the data files - read in all .XPT files in the directory
gc()
names <- Sys.glob(c("SIMAH_workplace/brfss/raw_data/data/*.XPT", 
                    "SIMAH_workplace/brfss/raw_data/data/*.xpt",
                    "SIMAH_workplace/brfss/raw_data/data/*.XPT "))
dataFiles <- lapply(names, read.xport)
names(dataFiles) <- names 

names(dataFiles) <- gsub("SIMAH_workplace/brfss/raw_data/data/", "", names(dataFiles))
names(dataFiles) <- gsub("CDBRFS", "", names(dataFiles))
names(dataFiles) <- gsub(".XPT", "", names(dataFiles))
names(dataFiles) <- gsub(".XPT ", "", names(dataFiles))
names(dataFiles) <- gsub("LLCP20", "", names(dataFiles))
names(dataFiles) <- gsub("cdbrfs", "", names(dataFiles))
names(dataFiles) <- gsub(".xpt", "", names(dataFiles))
as.numeric(names(dataFiles))

names(dataFiles) <- ifelse(as.numeric(names(dataFiles))>=84 & 
                             as.numeric(names(dataFiles))<=99, 
                           paste("19", as.character(names(dataFiles)),sep=""),
                           paste("20",as.character(names(dataFiles)), sep=""))
gc()

names(dataFiles) <- parse_number(names(dataFiles))

rm(list=setdiff(ls(), c("dataFiles")))

dataFiles <- dataFiles[order(names(dataFiles))]

saveRDS(dataFiles, file="SIMAH_workplace/brfss/raw_data/data/brfss_full.RDS")


