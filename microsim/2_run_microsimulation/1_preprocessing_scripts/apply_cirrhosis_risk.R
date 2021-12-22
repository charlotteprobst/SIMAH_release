# script to apply cirrhosis risk in the microsimulation
basepop$Cirrhosis_risk <- ifelse(basepop$microsim.init.drinkingstatus==1 & basepop$grams_10years>100000 & basepop$microsim.init.sex=="m", 1,
                                 ifelse(basepop$microsim.init.drinkingstatus==1 & basepop$grams_10years>(100000*0.66) &
                                          basepop$microsim.init.sex=="f", 1, 
                                        ifelse(basepop$formerdrinker==1 & basepop$grams_10years>100000 & basepop$microsim.init.sex=="m" & 
                                                 basepop$yearsincedrink<=8, 1,
                                               ifelse(basepop$formerdrinker==1 & basepop$grams_10years>(100000*0.66) &
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

toremove <- basepop %>% filter(cirrhosis==1) %>% group_by(microsim.init.sex) %>% add_tally() %>% 
  mutate(toremove=n/100) %>% sample_n(toremove)
ids <- toremove$microsim.init.id
basepop <- basepop %>% filter(!microsim.init.id %in% ids)
basepop$grams_10years <- basepop$grams_10years + (basepop$microsim.init.alc.gpd*365)

