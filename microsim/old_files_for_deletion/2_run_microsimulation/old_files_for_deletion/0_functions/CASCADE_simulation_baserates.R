######FUNCTION FOR RUNNING MICROSIMULATION 
run_microsim_baserates <- function(seed,basepop, outwardmigrants, inwardmigrants, deathrates, apply_death_rates,
                        updatingeducation, education_setup, transitionroles,
                        calculate_migration_rates, outward_migration, inward_migration, 
                        brfss,
                        transitions, PopPerYear, minyear, maxyear){
set.seed(seed)
Rates <- list()
for(y in minyear:maxyear){ 
####add the migrants for the year
if(y>=1985){
Rates[[paste(y)]] <- calculate_migration_rates(basepop, outwardmigrants, inwardmigrants, y)
year_rates <- Rates[[paste(y)]]
basepop <- outward_migration(basepop,year_rates,y)
basepop <- inward_migration(basepop,year_rates,y, brfss)
}

if(y>=1984){
  basepop <- apply_death_rates(basepop, deathrates, y)
}

#delete anyone over 80
PopPerYear[[paste(y)]] <- basepop
###then age everyone by 1 year and update age category
basepop <- basepop %>% mutate(microsim.init.age = microsim.init.age+1,
                              agecat = cut(microsim.init.age,
                                           breaks=c(0,19,24,34,44,54,64,74,100),
                                           labels=c("15-19","20-24","25-34","35-44","45-54","55-64",
                                                    "65-74","75-79")))
basepop <- subset(basepop, microsim.init.age<=80)


}
return(Rates)
}

