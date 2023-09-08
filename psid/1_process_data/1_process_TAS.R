# clean script to process PSID data
library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
setwd("~/Google Drive/SIMAH Sheffield")

# read in the data 
data <- read.dbf("SIMAH_workplace/PSID/J320383/J320383.dbf")
source("SIMAH_code/PSID/1_process_data/PSID_processing_functions.R")

data$familyID <- data$ER30001
data$ID <- data$ER30002
data$uniqueID <- (data$familyID*1000) + data$ID

race <- process_TAS_race(data)

education <- process_TAS_education(data) %>% distinct()

TAS <- merge(education, race, by=c("uniqueID"))

TAS$year <- as.integer(TAS$year)
TAS$uniqueID <- as.integer(TAS$uniqueID)

# save the TAS 
write.csv(TAS, "SIMAH_workplace/PSID/TAS_2011_2019.csv", row.names=F)

# now join up with the processed PSID to see if any additional information can be learned about people 
df <- read.csv("SIMAH_workplace/PSID/alldata_new_1999_2019.csv")

TAS <- TAS %>% dplyr::select(uniqueID, TAS_race) %>% distinct()

# join up with TAS 
df <- left_join(df, TAS)

df <- df %>% 
  group_by(uniqueID) %>% 
  fill(individualrace, .direction=c("downup")) %>% 
  fill(TAS_race, .direction=c("downup")) %>% 
  mutate(race_new = ifelse(is.na(TAS_race), individualrace, TAS_race))


test <- df %>% filter(uniqueID==53042)

# ensure that each person has a unique value for race and ethnicity over time
race_eth_function <- function(data){
  races <- unique(data$race_new)
  # recode according to hierarchy - black, hispanic, native american, asian/pi, other
  newrace <- ifelse("black" %in% races, "black",
                            ifelse("hispanic" %in% races, "hispanic", 
                                   ifelse("Native" %in% races, "Native",
                                          ifelse("Asian/PI" %in% races, "Asian/PI",
                                                 ifelse("other" %in% races, "other",
                                                        ifelse("white" %in% races, "white", NA))))))
  data$race_new_unique <- newrace
  return(data)
}

uniquerace <- df %>% dplyr::select(uniqueID, race_new) %>% 
  distinct() %>% 
  group_by(uniqueID) %>% 
  do(race_eth_function(.))

uniquerace <- uniquerace %>% dplyr::select(uniqueID, race_new_unique) %>% 
  distinct()

df <- left_join(df, uniquerace)


# write new version of df containing TAS race and education 
write.csv(df, "SIMAH_workplace/PSID/alldata_new_1999_2019.csv", row.names=F)



