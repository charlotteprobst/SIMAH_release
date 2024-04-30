TPs <- readRDS(paste0(OutputDirectory, "/transitionsList-1-1.RDS"))

for(i in 1:length(TPs)){
  TPs[[i]]$samplenum <- i
  TPs[[i]]$probability <- c(0, diff(TPs[[i]]$cumsum))
  TPs[[i]]$probability = ifelse(TPs[[i]]$probability<=0, TPs[[i]]$cumsum, TPs[[i]]$probability)
}

TPs <- do.call(rbind, TPs)

TPs_new <- TPs %>% 
  mutate(agecat = case_when(grepl("18-20", cat) ~ "18-20",
                            grepl("21-25", cat) ~ "21-25",
                            grepl("26-29", cat) ~ "26-29",
                            grepl("30-39", cat) ~ "30-39",
                            grepl("40-49", cat) ~ "40-49",
                            grepl("50-64", cat) ~ "50-64",
                            grepl("65+", cat) ~ "65+"),
         sex = case_when(grepl("_m_", cat) ~ "Men",
                         grepl("_f_", cat) ~ "Women"),
         race = case_when(grepl("BLA", cat) ~ "Black",
                          grepl("WHI", cat) ~ "White",
                          grepl("SPA", cat) ~ "Hispanic",
                          grepl("OTH", cat) ~ "Others"),
         education = case_when(grepl("LEHS", cat) ~ "LEHS",
                               grepl("SomeC", cat) ~ "SomeC",
                               grepl("College", cat) ~ "College"),
         StateFrom = case_when(grepl("Non-drinker", cat) ~ "Non-drinker",
                               grepl("Low risk", cat) ~ "Low risk",
                               grepl("Medium risk", cat) ~ "Medium risk",
                               grepl("High risk", cat) ~ "High risk")) %>% 
  ungroup() %>% 
  dplyr::select(samplenum, sex, agecat, race, education, StateFrom, StateTo, probability)

testTPs <- TPs_new %>% 
  filter(agecat=="65+" & sex=="Men" & education=="College" & race=="White") %>%
  filter(samplenum==6)
  
