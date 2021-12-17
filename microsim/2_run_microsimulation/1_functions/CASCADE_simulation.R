######FUNCTION FOR RUNNING MICROSIMULATION 
run_microsim <- function(seed,samplenum,basepop, outwardmigrants, inwardmigrants, deathrates, apply_death_rates,
                         updatingeducation, education_setup, transitionroles,
                         calculate_migration_rates, outward_migration, inward_migration, 
                         brfss,Rates,AlctransitionProbability,
                         transitions, PopPerYear, minyear, maxyear){
seed <- 1
samplenum <- 1
set.seed(seed)
Summary <- list()
DeathSummary <- list()
Cirrhosis <- list()
alcohol <- list()
# lhsSample <- lhsSample[[1]]
names <- names(lhsSample)
lhsSample <- as.numeric(lhsSample)
names(lhsSample) <- names
for(y in minyear:maxyear){ 
####add the migrants for the year
  
if(y>=1985){
  basepop <- inward_migration(basepop,Rates,y, brfss)
  # SummaryMissing[[paste(y)]] <- list[[2]]
  basepop <- outward_migration(basepop,Rates,y)
}
print(length(unique(basepop$microsim.init.id))==nrow(basepop))

if(y>=1984){
basepop <- apply_death_rates(basepop, deathrates, y)
}
  
# update alcohol and BMI 
  if(y>1984){
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/update_alcohol_BMI.R") 
  }
  
# apply cirrhosis risk 
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/apply_cirrhosis_risk.R")
  
alcohol[[paste(y)]] <- basepop %>% group_by(microsim.init.sex) %>% filter(microsim.init.alc.gpd!=0) %>% 
  mutate(agegroup = cut(microsim.init.age, 
                        breaks=c(0,24,34,44,54,64,74,100),
                        labels=c("18-24","25-34","35-44","45-54","55-64","65-74","75+")),
         birthyear = y-microsim.init.age,
         cohort = cut(birthyear,
                      breaks=c(0,1920,1925,1930,1935,1940,1945,1950,1955,
                               1960,1965,1970,1975,1980,1985,1990,2005),
                      labels=c("1900-1920","1921-1925","1926-1930","1931-1935","1936-1940",
                               "1941-1945","1946-1950","1951-1955","1956-1960","1961-1965",
                               "1966-1970","1971-1975","1976-1980","1981-1985","1986-1990",
                               "1991-2000"))) %>% 
  ungroup() %>% group_by(microsim.init.sex,agegroup,cohort) %>% filter(microsim.init.alc.gpd!=0) %>% 
  summarise(meanGPD = mean(microsim.init.alc.gpd)) %>% mutate(year=y)

#delete anyone over 80
###then age everyone by 1 year and update age category
PopPerYear[[paste(y)]] <- basepop
  
basepop <- basepop %>% mutate(microsim.init.age = microsim.init.age+1,
                              agecat = cut(microsim.init.age,
                                           breaks=c(0,19,24,34,44,54,64,74,100),
                                           labels=c("15-19","20-24","25-34","35-44","45-54","55-64",
                                                    "65-74","75-79")))
basepop <- subset(basepop, microsim.init.age<=80)
basepop <- basepop %>% dplyr::select(-c(RRHeavyUse:cirrhosis))

}
# SummaryMissing <- do.call(rbind,SummaryMissing)
# 
# write.csv(SummaryMissing,paste0("SIMAH_workplace/microsim/2_output_data/SummaryMissing", SelectedState, ".csv"))

for(i in names(PopPerYear)){
Summary[[paste(i)]] <- PopPerYear[[paste(i)]] %>% mutate(agecat = cut(microsim.init.age,
                                                                      breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,100),
                                                                      labels=c("18-24","25-29","30-34","35-39","40-44","45-49",
                                                                               "50-54","55-59","60-64","65-69","70-74","75-79")),
                                                         agecat = cut(microsim.init.age, breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,79),
                                                             labels=c("18","19-24","25-29","30-34","35-39",
                                                                      "40-44","45-49","50-54","55-59",
                                                                      "60-64","65-69","70-74","75-79"))) %>%
  group_by(microsim.init.sex, microsim.init.age, microsim.init.education, microsim.init.race) %>% tally() %>%
  mutate(year=i, seed=seed)
}

Summary <- do.call(rbind,Summary)
Cirrhosis <- do.call(rbind, Cirrhosis)
Cirrhosis <- Cirrhosis %>% mutate(seed = seed, 
         samplenum = samplenum,
         birthyear = Year-microsim.init.age,
         cohort = cut(birthyear,
                      breaks=c(0,1920,1925,1930,1935,1940,1945,1950,1955,
                               1960,1965,1970,1975,1980,1985,1990,2005),
                      labels=c("1900-1920","1921-1925","1926-1930","1931-1935","1936-1940",
                               "1941-1945","1946-1950","1951-1955","1956-1960","1961-1965",
                               "1966-1970","1971-1975","1976-1980","1981-1985","1986-1990",
                               "1991-2000")),
         pathway = ifelse(pmax(RRMetabolic, RRHeavyUse, RRHep)==RRHeavyUse, "Heavy use",
                           ifelse(pmax(RRMetabolic, RRHeavyUse, RRHep)==RRMetabolic, "Metabolic",
                                  "Hepatitis"))) %>% 
  group_by(Year,seed,samplenum, microsim.init.sex,cohort,pathway) %>% tally() 
# Cirrhosis <- Cirrhosis %>% group_by(Year, seed, samplenum, microsim.init.sex,agecat) %>% tally()
alcohol <- do.call(rbind,alcohol)
# DeathSummary <- do.call(rbind, DeathSummary)
# Summary <- list(Summary, DeathSummary)
return(alcohol)
}

