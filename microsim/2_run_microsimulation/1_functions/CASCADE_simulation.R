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
lhsSample <- lhsSample[[1]]
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
# PopPerYear <- do.call(rbind,PopPerYear) %>% mutate(year=as.factor(as.character(year)),
#                                                    samplenum=as.factor(samplenum),
#                                                    microsim.init.sex=as.factor(microsim.init.sex),
#                                                    microsim.init.race=as.factor(microsim.init.race),
#                                                    microsim.init.education=as.factor(microsim.init.education),
#                                                    agecat = ifelse(microsim.init.age<=29, "18-29",
#                                                                    ifelse(microsim.init.age>=30 & microsim.init.age<=49,"30-49",
#                                                                           "50+")),
#                                                    agecat=as.factor(agecat),
#                                                    AlcCAT=as.factor(AlcCAT)) %>% 
#   group_by(year, samplenum, microsim.init.sex,microsim.init.race, microsim.init.education, agecat,
#            AlcCAT, .drop=FALSE) %>% tally()
Summary <- do.call(rbind,Summary)
Cirrhosis <- do.call(rbind, Cirrhosis)
Cirrhosis$seed <- seed
Cirrhosis$samplenum <- samplenum
Cirrhosis <- Cirrhosis %>% group_by(Year, seed, samplenum, microsim.init.sex,agecat) %>% tally()
# DeathSummary <- do.call(rbind, DeathSummary)
# Summary <- list(Summary, DeathSummary)
return(Summary)
}

