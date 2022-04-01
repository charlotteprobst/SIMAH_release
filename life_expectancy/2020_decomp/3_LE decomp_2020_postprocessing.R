# Further process LE decomposition results for 2018 to 2020 
# to report results 
# Project: SIMAH

# libraries required:
library("tidyverse")
library("dplyr")
library("RColorBrewer")

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")

dMort <- read.csv("SIMAH_workplace/mortality/3_out data/US_mortality_rates_0020.csv")

dResults_contrib <- read.csv(
  "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/dResults_contrib_2018_2020ACS.csv")
dResults_contrib <- read.csv(
  "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/dResults_contrib_2018_2020race_ACS.csv")
# Graph results by sex and SES
dResults_contrib <- as.data.frame(dResults_contrib[, -1])

v.cod <- names(dResults_contrib)[grepl("mort", names(dResults_contrib))]
v.other <- v.cod[!v.cod %in% "COVIDmort"]
v.ncd <- 
v.despair <- c("OPDPOImort", "ALCPOImort", "SIJmort", "LIVERmort") 
v.alc <- c("ALCPOImort", "LIVERmort", "AUDmort") 
# Summarize outcomes and calculate proportion 
dResults_contrib <- 
  dResults_contrib %>% mutate(ALLmort = rowSums(dResults_contrib[,v.cod]), 
                              ALLOTHmort = rowSums(dResults_contrib[,v.other]),
                              DESPAIRmort = rowSums(dResults_contrib[,v.despair]), 
                              ALCmort = rowSums(dResults_contrib[,v.alc])) %>% 
  mutate(propCOV =  (COVIDmort/ALLmort)*100, 
         propDespair =   (DESPAIRmort/ALLmort)*100,
         propAlc =   (ALCmort/ALLmort)*100) %>% 
  mutate(across(c(v.cod, "propCOV", "propDespair", "propAlc",
                  "ALLmort", "ALLOTHmort", "DESPAIRmort", "ALCmort"), round, 2))
  
write.csv(dResults_contrib, 
          "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/results_decomp_2020_post.csv")

dMort <- dMort[dMort$year > 2018,] 
# delete all rate variables
dMort <- dMort[,which(!grepl(pattern = "rate",x = names(dMort)))]
#aggregate by age
dMort <- aggregate(.~ year + edclass + race + sex, data =  dMort, FUN=sum)

dMort <- 
  dMort %>% mutate(ALLmort = rowSums(dMort[,v.cod]), 
                              ALLOTHmort = rowSums(dMort[,v.other]),
                              DESPAIRmort = rowSums(dMort[,v.despair]), 
                              ALCmort = rowSums(dMort[,v.alc])) %>% 
  mutate(propCOV =  (COVIDmort/ALLmort)*100, 
         propDespair =   (DESPAIRmort/ALLmort)*100,
         propAlc =   (ALCmort/ALLmort)*100) %>% 
  mutate(across(c(v.cod, "propCOV", "propDespair", "propAlc",
                  "ALLmort", "ALLOTHmort", "DESPAIRmort", "ALCmort"), round, 2))

## grepl to replace mort with contrib --> merge data sets
write.csv(dMort, 
          "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/results_mort.csv")
