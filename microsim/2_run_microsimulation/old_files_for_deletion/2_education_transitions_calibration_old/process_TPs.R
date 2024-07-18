# process transition probabilities 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/education_calibration/newagecat30")
TPs <- read_rds(paste0(DataDirectory, "/transitionsList-9.RDS"))

for(i in 1:length(TPs)){
  TPs[[i]]$samplenum <- i
  TPs[[i]]$probability <- c(0, diff(TPs[[i]]$cumsum))
}

TPs <- do.call(rbind, TPs)

TPs_new <- TPs %>% 
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
         probability = ifelse(probability<0, 0, probability)) %>% ungroup() %>% 
  dplyr::select(samplenum, StateFrom, StateTo,agecat, sex, race, probability)
