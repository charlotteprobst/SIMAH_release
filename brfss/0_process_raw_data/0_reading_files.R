####SIMAH OCT 2022 brfss processing - reading in the raw data files 
# BRFSS data 2000- 2022
library(foreign)
#library(SASxport)
library(readr)
library(dplyr)
library(tidyr)
library(haven)

#wd <- "~/Google Drive/SIMAH Sheffield/"
#wd <- setwd("/Users/carolinkilian/Desktop/")
#setwd(wd)

####read in the data files - read in all .XPT files in the directory
gc()
#names <- Sys.glob(c("/Users/carolinkilian/Desktop/SIMAH_workplace/brfss/raw_data/data/*.XPT", 
  #                  "/Users/carolinkilian/Desktop/SIMAH_workplace/brfss/raw_data/data/*.xpt",
 #                   "/Users/carolinkilian/Desktop/SIMAH_workplace/brfss/raw_data/data/*.XPT "))
#dataFiles <- lapply(names, read_xpt)

setwd(wd)

####read in the data files - read in all .XPT files in the directory
gc()
names <- Sys.glob(c("SIMAH_workplace/brfss/raw_data/data/*.XPT", 
                    "SIMAH_workplace/brfss/raw_data/data/*.xpt",
                    "SIMAH_workplace/brfss/raw_data/data/*.XPT "))
names <- names[-c(10:25)]
dataFiles <- lapply(names, read.xport)

names(dataFiles) <- names 

# rename the files - only leave the year of the data 

names(dataFiles) <- gsub("SIMAH_workplace/brfss/raw_data/data/", "", names(dataFiles))
names(dataFiles) <- gsub("/Users/carolinkilian/Desktop/", "", names(dataFiles))

#names(dataFiles) <- gsub("CDBRFS", "", names(dataFiles))
#names(dataFiles) <- gsub(".XPT", "", names(dataFiles))
#names(dataFiles) <- gsub(".XPT ", "", names(dataFiles))
#names(dataFiles) <- gsub("LLCP20", "", names(dataFiles))
#names(dataFiles) <- gsub("cdbrfs", "", names(dataFiles))
#names(dataFiles) <- gsub(".xpt", "", names(dataFiles))

as.numeric(names(dataFiles))

# prefix of 19 for pre 2000 data and 20 for 2000's data
names(dataFiles) <- ifelse(as.numeric(names(dataFiles))>=84 & 
                             as.numeric(names(dataFiles))<=99, 
                           paste("19", as.character(names(dataFiles)),sep=""),
                           paste("20",as.character(names(dataFiles)), sep=""))
gc()

names(dataFiles) <- parse_number(names(dataFiles))

rm(list=setdiff(ls(), c("dataFiles")))

dataFiles <- dataFiles[order(names(dataFiles))]

# Read in any existing brfss processed data file (in this case 2000-2020)
dataFiles_2 <- readRDS(file="SIMAH_workplace/brfss/raw_data/data/brfss_full_selected.RDS")

dataFiles_combined <- c(dataFiles_2, dataFiles)

# save RDS of full data list 
#saveRDS(dataFiles, file="/Users/carolinkilian/Desktop/SIMAH_workplace/brfss/raw_data/data/brfss_full.RDS")
saveRDS(dataFiles_combined, file="SIMAH_workplace/brfss/raw_data/data/brfss_full_2022.RDS")


