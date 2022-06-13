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

v.infect <- c("FLUmort", "OTHINFmort") 

v.ncd <- c("HEARTmort", "CANCERmort", "STROKEmort", "RESPmort", 
           "ALZHmort", "DMmort", "KIDNEYmort", "OTHNCDmort")
v.alc <- c("ALCPOImort", "LIVERmort", "AUDmort") 
v.injur <- c("MVACCmort", "OPDPOImort", "UIJmort", "SIJmort", "OTHJmort") 

#overlap with other categories
v.despair <- c("OPDPOImort", "ALCPOImort", "SIJmort", "LIVERmort") 

# Summarize outcomes and calculate proportion 
dResults_contrib <- 
  dResults_contrib %>% mutate(ALLmort = rowSums(dResults_contrib[,v.cod]), 
                              ALLOTHmort = rowSums(dResults_contrib[,v.other]),
                              DESPAIRmort = rowSums(dResults_contrib[,v.despair]), 
                              INFECTmort = rowSums(dResults_contrib[,v.infect]),
                              NCDmort = rowSums(dResults_contrib[,v.ncd]),
                              ALCmort = rowSums(dResults_contrib[,v.alc]),
                              INJURmort = rowSums(dResults_contrib[,v.injur])) %>% 
  mutate(propCOVID =  (COVIDmort/ALLmort)*100, 
         propDespair =   (DESPAIRmort/ALLmort)*100,
         propINFECT =   (INFECTmort/ALLmort)*100, 
         propNCD =   (NCDmort/ALLmort)*100, 
         propALC =   (ALCmort/ALLmort)*100,
         propINJUR =   (INJURmort/ALLmort)*100,
         propREST =   (RESTmort/ALLmort)*100) %>% 
  mutate(across(c(v.cod, 
                  "propCOVID", "propDespair", "propINFECT","propNCD",
                  "propALC", "propINJUR", "propREST",
                  "ALLmort", "ALLOTHmort", "DESPAIRmort", 
                  "COVIDmort", "INFECTmort", "NCDmort", 
                  "ALCmort", "INJURmort", "RESTmort"), round, 2))
  
write.csv(dResults_contrib, 
          "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/results_decomp_2020_post.csv")

v.vars <- c("sex", "edclass", "race", "start_year", "end_year", "LE1", "LE2",
            "COVIDmort", "INFECTmort","ALCmort", "NCDmort", "INJURmort", "RESTmort", 
             "propCOVID", "propINFECT", "propALC", "propNCD", "propINJUR", "propREST")
dResults_contrib <- select(dResults_contrib, v.vars)
dResults_contrib <- filter(dResults_contrib, start_year == 2019)
dGatherMort <- gather(data = dResults_contrib, key = "mort", value = "value", 
                         -sex , -edclass, -race, -start_year, -end_year, -LE1, -LE2)
v.sel <- "mort"
#v.sel <- "prop"

if (v.sel == "mort") {
  dGatherMort <- filter(dGatherMort, grepl('mort', mort))
} else {
  dGatherMort <- filter(dGatherMort, grepl('prop', mort))  
}

# have to be factor variables
dGatherMort$sex <- as.factor(dGatherMort$sex)
levels(dGatherMort$sex) <- list(Men = "1", Women = "2")

dGatherMort$race <- factor(dGatherMort$race, levels = c("White", "Black", "Hispanic"))

dGatherMort$edclass <- as.factor(dGatherMort$edclass)
levels(dGatherMort$edclass) <- list("Low" = "LEHS", "Middle" = "SomeC", "High" = "College")
#dGatherMort$SES <- factor(dGatherMort$SES, levels = c("High", "Middle", "Low"))

if (v.sel == "mort") {
  dGatherMort$mort = gsub(pattern = "mort", replacement = "", x = dGatherMort$mort)
} else {
  dGatherMort$mort = gsub(pattern = "prop", replacement = "", x = dGatherMort$mort)
}
dGatherMort$mort <- as.factor(dGatherMort$mort)
levels(dGatherMort$mort) <- list("Covid 19" = "COVID",
                                 "Infectious diseases" = "INFECT",
                                 "Alcohol-related causes" = "ALC", 
                                 "Non-communicable diseases" = "NCD",
                                 "Injuries" = "INJUR",
                                 "Rest" = "REST")
dGatherMort <- rename(dGatherMort, "Sex" = "sex", 
                         "Race" = "race", 
                         "Cause_of_death" = "mort", 
                         "Contribution"= "value", 
                         "Education" = "edclass")
color.vec <- c(rev(brewer.pal(5,"YlGnBu")),
               c("grey"))
ggplot(data = dGatherMort, aes(x = Education, y = Contribution, fill = Cause_of_death)) +
  geom_bar(position = position_stack(reverse = T), stat = "identity") +
  scale_fill_manual("Cause of death", values = color.vec)+ 
  facet_grid(rows = vars(Sex, Race)) + 
  coord_flip() +
  theme_bw() +
  if(v.sel == "mort") {
    ylab("Contribution to changes in life expectancy in years")
  } else {
    ylab("Percent contribution to total changes in life expectancy")  
    }
if(v.sel == "mort") {
  ggsave("SIMAH_workplace/life_expectancy/3_graphs/2020_decomp/2_LE decomp_prelim_absolute.jpeg", dpi=600, width=30, height=15, units="cm")
  write.csv(dGatherMort, "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/2_LE decomp_prelim_absolute.csv")
} else {
  ggsave("SIMAH_workplace/life_expectancy/3_graphs/2020_decomp/2_LE decomp_prelim_prop.jpeg", dpi=600, width=30, height=15, units="cm")  
  write.csv(dGatherMort, "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/2_LE decomp_prelim_prop.csv")
}


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
