# process transition probabilities 
library(tidyverse)
WorkingDirectory <- "C:/Users/cmp21seb/Documents/SIMAH/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/education_calibration/newagecat30")
TPs <- read_rds(paste0(DataDirectory, "/transitionsList-1.RDS"))

for(i in 1:length(TPs)){
  TPs[[i]]$samplenum <- i
  TPs[[i]]$probability <- c(0, diff(TPs[[i]]$cumsum))
}

TPs <- do.call(rbind, TPs)

TPs_new_1 <- TPs %>% 
  mutate(cat = gsub("1999-2019_","", cat),
         cat = gsub("_STATEFROM", "", cat),
         agecat = case_when(grepl("18", cat) ~ "18",
                            grepl("19", cat) ~ "19",
                            grepl("20", cat) ~ "20",
                            grepl("21", cat) ~ "21",
                            grepl("22-24", cat) ~ "22-24",
                            grepl("25-29", cat) ~ "25-29",
                            grepl("30+", cat) ~ "30+"),
         sex = case_when(grepl("m", cat) ~ "Men",
                         grepl("f", cat) ~ "Women"),
         race = case_when(grepl("black", cat) ~ "Black",
                          grepl("white", cat) ~ "White",
                          grepl("hispanic", cat) ~ "Hispanic",
                          grepl("other", cat) ~ "Others"),
         cat = substr(cat, 7,20),
         StateFrom = parse_number(cat),
         probability = ifelse(probability<=0, cumsum, probability)) %>% ungroup() %>% 
  dplyr::select(samplenum, StateFrom, StateTo,agecat, sex, race, probability)

###############################################################################
TPs_10 <- read_rds(paste0(DataDirectory, "/transitionsList-10.RDS"))

for(i in 1:length(TPs_10)){
  TPs_10[[i]]$samplenum <- i
  TPs_10[[i]]$probability <- c(0, diff(TPs_10[[i]]$cumsum))
}

TPs_10 <- do.call(rbind, TPs_10)

TPs_new_10 <- TPs_10 %>% 
  mutate(cat = gsub("1999-2019_","", cat),
         cat = gsub("_STATEFROM", "", cat),
         agecat = case_when(grepl("18", cat) ~ "18",
                            grepl("19", cat) ~ "19",
                            grepl("20", cat) ~ "20",
                            grepl("21", cat) ~ "21",
                            grepl("22-24", cat) ~ "22-24",
                            grepl("25-29", cat) ~ "25-29",
                            grepl("30+", cat) ~ "30+"),
         sex = case_when(grepl("m", cat) ~ "Men",
                         grepl("f", cat) ~ "Women"),
         race = case_when(grepl("black", cat) ~ "Black",
                          grepl("white", cat) ~ "White",
                          grepl("hispanic", cat) ~ "Hispanic",
                          grepl("other", cat) ~ "Others"),
         cat = substr(cat, 7,20),
         StateFrom = parse_number(cat),
         probability = ifelse(probability<=0, cumsum, probability)) %>% ungroup() %>% 
  dplyr::select(samplenum, StateFrom, StateTo,agecat, sex, race, probability)

saveRDS(TPs_new_1, paste0(DataDirectory, "/transitionsList-1_converted.RDS"))
saveRDS(TPs_new_10, paste0(DataDirectory, "/transitionsList-10_converted.RDS"))