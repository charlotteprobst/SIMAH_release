######FUNCTION FOR RUNNING MICROSIMULATION 
run_microsim <- function(seed,samplenum,lhsSample, basepop, deathrates, apply_death_rates,
                         outward_migration, inward_migration, mortality,
                         AssignAcuteHep, AssignChronicHep, CirrhosisHeavyUse, CirrhosisHepatitis, MetabolicPathway,
                         brfss,Rates, minyear, maxyear){
set.seed(seed)
Summary <- list()
DeathSummary <- list()
Cirrhosis <- list()
alcohol <- list()
PopPerYear <- list()
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
    basepop <- basepop %>% 
      mutate(agecat = cut(microsim.init.age,
                          breaks=c(0,19,24,34,44,54,64,74,100),
                          labels=c("15-19","20-24","25-34","35-44","45-54","55-64",
                                   "65-74","75+")))
    newGPD <- updating_alcohol(basepop, brfssorig, y)
    basepop <- left_join(basepop, newGPD, by=c("microsim.init.id"))
    
    basepop$newformerdrinker <- ifelse(basepop$newGPD==0 & basepop$microsim.init.alc.gpd>0.46,1,
                                       ifelse(basepop$newGPD>0.46, 0, basepop$formerdrinker))
    
    basepop$newyearsincedrink <- ifelse(basepop$newformerdrinker==1 & basepop$formerdrinker==0, 0,
                                        ifelse(basepop$newformerdrinker==1 & basepop$formerdrinker==1,
                                               basepop$yearsincedrink+1,
                                               ifelse(basepop$newGPD>0.46, 0, 0)))
    
    basepop <-  basepop %>% 
      mutate(formerdrinker = newformerdrinker,
             yearsincedrink = newyearsincedrink,
             microsim.init.alc.gpd = newGPD,
             microsim.init.drinkingstatus = ifelse(microsim.init.alc.gpd<=0.46, 0, 1)) %>% 
      dplyr::select(-c(newGPD, newformerdrinker, newyearsincedrink))
    
    newBMI <- updating_BMI(basepop, brfssorig, y)
    basepop <- left_join(basepop, newBMI, by=c("microsim.init.id"))
    basepop$microsim.init.BMI <- basepop$newBMI
    basepop$newBMI <- NULL
# source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/update_alcohol_BMI.R") 
  }

# apply cirrhosis risk 
# source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/apply_cirrhosis_risk.R")
basepop$Cirrhosis_risk <- ifelse(basepop$microsim.init.drinkingstatus==1 & basepop$grams_10years>as.numeric(lhsSample[["THRESHOLD"]]) & basepop$microsim.init.sex=="m", 1,
                                 ifelse(basepop$microsim.init.drinkingstatus==1 & basepop$grams_10years>(as.numeric(lhsSample[["THRESHOLD"]])*as.numeric(lhsSample[["THRESHOLD_MODIFIER"]])) &
                                          basepop$microsim.init.sex=="f", 1, 
                                        ifelse(basepop$formerdrinker==1 & basepop$grams_10years>as.numeric(lhsSample[["THRESHOLD"]]) & basepop$microsim.init.sex=="m" & 
                                                 basepop$yearsincedrink<=8, 1,
                                               ifelse(basepop$formerdrinker==1 & basepop$grams_10years>(as.numeric(lhsSample[["THRESHOLD"]])*as.numeric(lhsSample[["THRESHOLD_MODIFIER"]])) &
                                                        basepop$microsim.init.sex=="f" & basepop$yearsincedrink<=8, 1,0))))

basepop <- CirrhosisHeavyUse(basepop, lhsSample,"b")
basepop <- MetabolicPathway(basepop, lhsSample,"b")
basepop <- AssignAcuteHep(basepop, Hep, distribution,y)
basepop <- AssignChronicHep(basepop)

basepop <- CirrhosisHepatitis(basepop,lhsSample)
basepop$RR <- (basepop$RRHeavyUse)+(basepop$RRMetabolic)+(basepop$RRHep)

basepop$RR <- ifelse(basepop$RR>100, 100, basepop$RR)

basepop$agecat <- cut(basepop$microsim.init.age,
                      breaks=c(0,19,24,34,44,54,64,74,100),
                      labels=c("15-19","20-24","25-34","35-44","45-54","55-64","65-74",
                               "75."))
basepop$cat <- paste(basepop$agecat, basepop$microsim.init.sex, sep="_")

if(y==1984){
  rates <- basepop %>% group_by(cat) %>% add_tally() %>% 
    summarise(popcount=n,
              sumrisk = sum(RR),
              .groups='drop') %>% ungroup() %>% distinct()
  rates <- left_join(rates, cirrhosisdeaths1984, by=c("cat"))
  rates$rate <- rates$count/rates$sumrisk
}

basepop <- left_join(basepop, rates, by=c("cat"))
basepop$Risk <- basepop$RR*basepop$rate
basepop <- basepop %>% dplyr::select(-c(popcount))

basepop$probs <- runif(nrow(basepop))
basepop$cirrhosis <- ifelse(basepop$probs<=basepop$Risk, 1,0)

Cirrhosis[[paste(y)]] <- basepop %>% filter(cirrhosis==1) %>% mutate(Year = y)

if(mortality==1){
toremove <- basepop %>% filter(cirrhosis==1) %>% group_by(microsim.init.sex,agecat) %>% add_tally() %>% 
  mutate(toremove=round(n/100)) %>% sample_n(toremove)
ids <- toremove$microsim.init.id
basepop <- basepop %>% filter(!microsim.init.id %in% ids)
}

basepop$grams_10years <- basepop$grams_10years + (basepop$microsim.init.alc.gpd*365)
  
alcohol[[paste(y)]] <- basepop %>% group_by(microsim.init.sex) %>% 
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
  ungroup() %>% group_by(microsim.init.sex,agegroup) %>% filter(Cirrhosis_risk==1) %>% 
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
                                                         agegroup = cut(microsim.init.age, 
                                                                        breaks=c(0,19,24,34,44,54,64,74,100),
                                                                        labels=c("15-19","20-24","25-34","35-44","45-54","55-64","65-74","75.")),
                                                         obese = ifelse(microsim.init.BMI>=30, 1,0)) %>%
  group_by(microsim.init.sex, agegroup) %>% tally(name="populationtotal") %>% 
  # summarise(percentobese = mean(obese),
  #                                                   percentoverthreshold = mean(Cirrhosis_risk)) %>% 
  mutate(year=i, seed=seed, samplenum=samplenum)
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
         agegroup = cut(microsim.init.age, 
                        breaks=c(0,19,24,34,44,54,64,74,100),
                        labels=c("15-19","20-24","25-34","35-44","45-54","55-64","65-74","75.")),
         pathway = ifelse(pmax(RRMetabolic, RRHeavyUse, RRHep)==RRHeavyUse, "Heavy use",
                           ifelse(pmax(RRMetabolic, RRHeavyUse, RRHep)==RRMetabolic, "Metabolic",
                                  "Hepatitis"))) %>% 
  group_by(Year,seed,agegroup,samplenum, microsim.init.sex) %>% tally(name="cirrhosistotal") %>% 
  rename(year=Year)
Summary$year <- as.numeric(Summary$year)
Summary <- left_join(Summary, Cirrhosis)
Summary$cirrhosistotal <- Summary$cirrhosistotal/100
Summary$rateper100000 <- (Summary$cirrhosistotal/Summary$populationtotal)*100000
# write.csv(Summary, paste0("SIMAH_workplace/microsim/2_output_data/validation/outputfiles/", samplenum, "_", seed,".csv"))
# Cirrhosis <- Cirrhosis %>% group_by(Year, seed, samplenum, microsim.init.sex,agecat) %>% tally()
# DeathSummary <- do.call(rbind, DeathSummary)
# Summary <- list(Summary, DeathSummary)
return(Summary)
}

