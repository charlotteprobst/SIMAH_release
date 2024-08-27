#' Runs microsimulation - alternative version with different ordering
#'
#' This function runs the microsimulation
#' @param
#' @keywords microsimulation
#' @import data.table
#' @export
#' @examples
#' run_microsim_alt
run_microsim_alt <- function(seed,samplenum,basepop,brfss,
                         death_counts,
                         updatingeducation, education_transitions,
                         COVID_specific_tps,
                         migration_rates,
                         updatingalcohol, alcohol_transitions,
                         catcontmodel, drinkingdistributions,
                         base_counts, diseases, mortality_parameters, sesinteraction,
                         policy=0, policy_model, year_policy, scenario, 
                         participation, part_elasticity, cons_elasticity, cons_elasticity_se, r_sim_obs,
                         inflation_factors,
                         age_inflated,
                         update_base_rate,
                         minyear=2000, maxyear=2005, output="mortality"){
set.seed(seed)
# Summary <- list()
DeathSummary <- list()
DiseaseSummary <- list()
PopPerYear <- list()
CatSummary <- list()
meandrinking <- list()
targets <- generate_targets_alcohol(brfss)
targets$proptarget <- ifelse(targets$year==2000, NA, targets$proptarget)
DM_men <- "off"
# names <- names(lhs)
# lhs <- as.numeric(lhs)
# names(lhs) <- names
for(y in minyear:maxyear){
print(y)

# apply policy effects
  if(policy==1 & year_policy>maxyear & y==minyear){
    print("policy is not within model time frame")
  }
  
  if(policy==1 & y==year_policy & scenario > 0){
    
    if(policy_model %like% "tax|price"){
      
      if(participation == 1){
        prob_alcohol_transitions <- prob_alcohol_transition(basepop, alcohol_transitions)
        }
    
      basepop <- apply_tax_policy(basepop, policy_model, scenario, 
                                  participation, part_elasticity, prob_alcohol_transitions, 
                                  cons_elasticity, cons_elasticity_se, r_sim_obs)  
    }  
    
    if(policy_model %like% "sales"){
      print("Sunday sales ban policy currently not implemented")
    }  
    
    # update alcohol categories
    basepop <- update_alcohol_cat(basepop)
    
  }

# calculate implausibility in each year - break if implausibility is over threshold
  if(output=="alcoholcat"){
  CatSummary[[paste(y)]] <- basepop %>%
    mutate(samplenum=samplenum,
           seed=seed,
           setting=as.character(setting),
           scenario=as.character(round(scenario*100,1)),
           agecat = cut(age,
                        breaks=c(0,24,64,100),
                        labels=c("18-24","25-64","65+")),
           education=ifelse(agecat=="18-24" & education=="College", "SomeC", education),
           year=y) %>%
    group_by(year, samplenum, seed, scenario, setting, sex, education,
             alc_cat, .drop=FALSE) %>% tally() %>%
    ungroup() %>%
    group_by(year, samplenum, seed, scenario, setting, sex, education) %>%
    mutate(propsimulation=n/sum(n)) %>%
    dplyr::select(-n) %>%
    mutate_at(vars(setting, sex, education, alc_cat), as.character)

  # CatSummary[[paste(y)]] <- left_join(CatSummary[[paste(y)]],targets, by=c("year","sex","race",
  #                                                                         "agecat","education","alc_cat"))
  # CatSummary[[paste(y)]] <- left_join(CatSummary[[paste(y)]],variance, by=c("year","sex","race",
  #                                                                           "agecat","education","alc_cat"))

  # CatSummary[[paste(y)]]$implausibility <- abs(CatSummary[[paste(y)]]$propsimulation-CatSummary[[paste(y)]]$proptarget)/sqrt(CatSummary[[paste(y)]]$se^2)
  }

if(output=="alcoholcont"){
  meandrinking[[paste(y)]] <- basepop %>%
    mutate(samplenum=samplenum,
           seed=seed,
           setting=as.character(setting),
           scenario=as.character(round(scenario*100,1)),
           year=y) %>%
    filter(alc_gpd>0) %>%
    mutate(agecat=cut(age,
                      breaks=c(0,24,64,100),
                      labels=c("18-24","25-64","65+"))) %>%
    group_by(year, samplenum, seed, scenario, setting, sex, education) %>%
    summarise(meansimulation = mean(alc_gpd))
}

# save a population summary
PopPerYear[[paste(y)]] <- basepop %>% mutate(year=y, seed=seed, samplenum=samplenum, scenario=scenario)

# apply death rates - all other causes
basepop <- apply_death_counts(basepop, death_counts, y, diseases)
# DeathSummary[[paste(y)]] <- basepop %>% filter(dead==1) %>% dplyr::select(agecat, race, sex, education,
#                                               dead, cause) %>% mutate(year=y, seed=seed)
# # remove individuals due to death and remove columns no longer needed
# basepop <- basepop %>% filter(dead==0) %>% dplyr::select(-c(dead, cause, overallrate))

# If diseases are specified in the disease vector, simulate mortality from those specific diseases
if(!is.null(diseases)){
# print("simulating disease mortality")
# disease <- unique(diseases)
if("HLVDC" %in% diseases==TRUE){
  basepop <- CirrhosisHepatitis(basepop,mortality_parameters)
}
if("LVDC" %in% diseases==TRUE){
  if(sesinteraction==1){
    basepop <- CirrhosisAllInteraction(basepop,mortality_parameters)
  }else if(sesinteraction==0){
    basepop <- CirrhosisAll(basepop,mortality_parameters)
  }
}
if("AUD" %in% diseases==TRUE){
  if(sesinteraction==1){
    basepop <- AUDInteraction(basepop,mortality_parameters)
  }else if(sesinteraction==0){
    basepop <- AUD(basepop,mortality_parameters)
  }
}
if("IJ" %in% diseases==TRUE){
  basepop <- SUICIDE(basepop,mortality_parameters)
}
if("DM" %in% diseases==TRUE){
  if(DM_men=="off"){
    basepop <- DM_menoff(basepop,mortality_parameters)
  }else if(DM_men=="on"){
    basepop <- DM(basepop,mortality_parameters)
  }
}
if("IHD" %in% diseases==TRUE){
  if(sesinteraction==1){
    basepop <- IHDInteraction(basepop,mortality_parameters)
  }else if(sesinteraction==0){
    basepop <- IHD(basepop,mortality_parameters)
  }
}
if("ISTR" %in% diseases==TRUE){
  basepop <- ISTR(basepop, mortality_parameters)
}
if("HYPHD" %in% diseases==TRUE){
  basepop <- HYPHD(basepop, mortality_parameters)
}
if("MVACC" %in% diseases==TRUE){
  basepop <- MVACC(basepop, mortality_parameters)
}
if("UIJ" %in% diseases==TRUE){
  basepop <- UIJ(basepop, mortality_parameters)
}

# calculate base rates if year = 2000)
if(y == 2000){
  rates <- calculate_base_rate(basepop,base_counts,diseases)
}

# update the base rate based on mortality_parameters
if(update_base_rate==1){
  rates <- update_base_rate(rates, mortality_parameters, y)
}

basepop <- left_join(basepop, rates, by=c("cat"))

# now simulate mortality
basepop <- simulate_mortality(basepop, diseases)

# now loop through and summarise the N from each cause of death
summary_list <- list()
for (disease in diseases) {
  # Generate and add the summary to the list with automatic naming
  summary_list[[paste0(disease)]] <- basepop %>%
    mutate(ageCAT = cut(age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           inflation_factor = ifelse(ageCAT %in% age_inflated[[1]], inflation_factors[1],
                                     ifelse(ageCAT %in% age_inflated[[2]], inflation_factors[2], NA))) %>%
    group_by(ageCAT, sex, race, education) %>%
    summarise(!!paste0("mort_", disease) := sum(!!sym(paste0("mort_", disease))/inflation_factor),
              !!paste0("yll_", disease) := sum(!!sym(paste0("yll_", disease))/inflation_factor)) %>%
    rename(agecat=ageCAT)

}

DiseaseSummary[[paste(y)]] <- basepop %>%
  mutate(agecat = cut(age,
               breaks=c(0,24,34,44,54,64,74,79),
               labels=c("18-24","25-34","35-44", "45-54",
                        "55-64","65-74","75-79"))) %>%
  group_by(agecat, sex, race, education) %>% tally() %>%
  mutate(year=y)

DiseaseSummary[[paste(y)]]$max_risk <- unique(basepop$max_risk)

# now join together to make a diseases dataframe for that year
for(disease in diseases){
  DiseaseSummary[[paste(y)]] <-
    left_join(DiseaseSummary[[paste(y)]], summary_list[[paste0(disease)]], by=c("agecat","sex",
                                                                                "race","education"))
}

# now sample the correct proportion of those to be removed (due to inflated mortality rate)
for (disease in diseases) {
  # function that removes individuals due to their original RR for that disease
  basepop <- remove_individuals(basepop, disease, age_inflated, inflation_factors)
}

basepop <- basepop %>% dplyr::select(-c(cat,prob,max_risk))

}

# transition education for individuals aged 34 and under
if(updatingeducation==1){
  # print("updating education")
  totransition <- basepop %>% filter(age<=34)
  tostay <- basepop %>% filter(age>34)
  if(y<=2019){
  totransition <- setup_education(totransition,y)
  }else {
  totransition <- setup_education_covid(totransition,y)
  }

  if(COVID_specific_tps==1){
    if(y<=2019){
      print("applying pre-covid tps")
      # apply transitions using pre covid TPs
      totransition <- totransition %>% group_by(cat) %>%
        do(transition_ed(., education_transitions))
    }else {
      print("applying covid tps")
      # apply transitions using covid TPs
      totransition <- totransition %>% group_by(cat) %>%
        do(transition_ed(., education_transitions_covid))
    }
  } else {
    print("applying pre-covid tps")
    # apply tps for pre-covid throughout
    totransition <- totransition %>% group_by(cat) %>%
      do(transition_ed(., education_transitions))
  }
  totransition$education_detailed <- totransition$newED
  totransition$education <- ifelse(totransition$education_detailed=="LEHS","LEHS",
                                                 ifelse(totransition$education_detailed=="SomeC1","SomeC",
                                                        ifelse(totransition$education_detailed=="SomeC2","SomeC",
                                                               ifelse(totransition$education_detailed=="SomeC3","SomeC",
                                                                      ifelse(totransition$education_detailed=="College","College",NA)
                                                                      ))))
  totransition <- totransition %>% ungroup() %>% dplyr::select(-c(prob, state, year, newED,cat))
  basepop <- rbind(totransition, tostay)
}

# if(updatingeducation==1 & y>=2019){
# }

# update alcohol use categories
if(updatingalcohol==1){
  basepop <- transition_alcohol_ordinal_regression(basepop,alcohol_transitions,y)
#   # allocate a numeric gpd for individuals based on model - only individuals that have changed categories
  if(is.null(catcontmodel)==FALSE){
  basepop <- allocate_gramsperday_sampled(basepop,y,catcontmodel)
  # allocate former drinker status - for now this is not tracked over time
  basepop <- update_former_drinker(basepop)
  }else if(is.null(catcontmodel)==TRUE){
  basepop$totransitioncont <- NULL
  }
}

#delete anyone over 79
###then age everyone by 1 year and update age category
basepop <- basepop %>% mutate(age = age+1,
                              agecat = cut(age,
                                           breaks=c(0,19,24,34,44,54,64,74,100),
                                           labels=c("15-19","20-24","25-34","35-44","45-54","55-64",
                                                    "65-74","75-79")))
basepop <- subset(basepop, age<=79)

# add and remove migrants
if(y<2019){
basepop <- inward_births_rate(basepop, migration_rates, y, brfss)
basepop <- inward_migration_rate(basepop, migration_rates, y, brfss)
basepop <- outward_migration_rate(basepop,migration_rates,y)
}

}
# save output - depending on which was selected
#### use a vector to contain the outputs we are interested in TODO
# indicator of how aggregated the results should be? - in the vector of outputs

if(output=="population"){
  Summary <- basepop %>%
    group_by(sex, agecat, race, education) %>%
    summarize(count = n()) %>%
    mutate(percentage = round(count / sum(count) * 100, 1))
}else if(output=="mortality" & !is.null(diseases)){
  Summary <- postprocess_mortality(DiseaseSummary,diseases, death_counts) %>%
    mutate(seed = seed, samplenum = samplenum, scenario = scenario)
}else if(output=="mortality" & is.null(diseases)){
    Summary <- 0
  }else if(output=="demographics"){
    # add seed to the output file here TODO
  for(i in 1:length(PopPerYear)){
    PopPerYear[[i]]$agecat <- cut(PopPerYear[[i]]$age,
                                                  breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,100),
                                                  labels=c("18","19-24","25-29","30-34","35-39","40-44",
                                                           "45-49","50-54","55-59","60-64","65-69",
                                                           "70-74","75-79"))
    PopPerYear[[i]] <- as.data.table(PopPerYear[[i]])
    PopPerYear[[i]] <- PopPerYear[[i]][, .(n = .N), by = .(year, samplenum, seed, scenario, sex, race, education, age, agecat)]
  }
    Summary <- do.call(rbind,PopPerYear)
}else if(output=="alcoholcat"){
  # CatSummary <- do.call(rbind,PopPerYear) %>%
  #   mutate(agecat = cut(age,
  #                       breaks=c(0,24,64,100),
  #                       labels=c("18-24","25-64","65+")) %>%
  #   group_by(year, samplenum, seed, sex,race,age, education,
  #            alc_cat, .drop=FALSE) %>% tally() %>%
  #     ungroup() %>%
  #     group_by(year, sex,race,agecat, education) %>%
  #     mutate(propsimulation=n/sum(n)) %>%
  #     dplyr::select(-n) %>%
  #     mutate_at(vars(sex, race, agecat, education, alc_cat), as.character)
Summary <- do.call(rbind,CatSummary) %>%
  mutate(seed=seed, samplenum=samplenum, scenario=scenario)
#implausibility <- max(CatSummary$implausibility, na.rm=T)
}else if(output=="alcoholcont"){
  Summary <- do.call(rbind, meandrinking)
}

# formerdrinkers <- list()
# for(i in 1:length(PopPerYear)){
#   formerdrinkers[[i]] <- PopPerYear[[i]] %>% group_by(formerdrinker) %>% tally() %>%
#     ungroup() %>% mutate(prop=n/sum(n))
# }
# migration_rates <- do.call(rbind,migration_rates)
# birth_rates <- do.call(rbind,birth_rates)
return(Summary)
}
