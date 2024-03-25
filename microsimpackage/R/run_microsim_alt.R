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
                         migration_rates,
                         updatingalcohol, alcohol_transitions,
                         catcontmodel, drinkingdistributions,
                         base_counts, diseases, lhs, sesinteraction,
                         policy=0, percentreduction=0.1, year_policy, inflation_factors,
                         age_inflated,
                         update_base_rate,
                         minyear, maxyear, output){
set.seed(seed)
Summary <- list()
DeathSummary <- list()
DiseaseSummary <- list()
PopPerYear <- list()
# birth_rates <- list()
# migration_rates <- list()
names <- names(lhs)
lhs <- as.numeric(lhs)
names(lhs) <- names
for(y in minyear:maxyear){
print(y)

# apply policy effects
if(policy==1 & year_policy>maxyear & y==minyear){
  print("policy is not within model time frame")
}

if(policy==1 & y ==year_policy){
  # apply policy effect according to percent reduction and then update alcohol categories
  basepop$microsim.init.alc.gpd <- basepop$microsim.init.alc.gpd - (basepop$microsim.init.alc.gpd*percentreduction)
  basepop <- update_alcohol_cat(basepop)
}

# save a population summary
PopPerYear[[paste(y)]] <- basepop %>% mutate(year=y, seed=seed, samplenum=samplenum)

# apply death rates - all other causes
basepop <- apply_death_counts(basepop, death_counts, y, diseases)
# DeathSummary[[paste(y)]] <- basepop %>% filter(dead==1) %>% dplyr::select(agecat, microsim.init.race, microsim.init.sex, microsim.init.education,
#                                               dead, cause) %>% mutate(year=y, seed=seed)
# # remove individuals due to death and remove columns no longer needed
# basepop <- basepop %>% filter(dead==0) %>% dplyr::select(-c(dead, cause, overallrate))

# If diseases are specified in the disease vector, simulate mortality from those specific diseases
if(!is.null(diseases)){
print("simulating disease mortality")
# disease <- unique(diseases)
if("HLVDC" %in% diseases==TRUE){
  basepop <- CirrhosisHepatitis(basepop,lhs)
}
if("LVDC" %in% diseases==TRUE){
  if(sesinteraction==1){
    basepop <- CirrhosisAllInteraction(basepop,lhs)
  }else if(sesinteraction==0){
    basepop <- CirrhosisAll(basepop,lhs)
  }
}
if("AUD" %in% diseases==TRUE){
  if(sesinteraction==1){
    basepop <- AUDInteraction(basepop,lhs)
  }else if(sesinteraction==0){
    basepop <- AUD(basepop,lhs)
  }
}
if("IJ" %in% diseases==TRUE){
  basepop <- SUICIDE(basepop, lhs)
}
if("DM" %in% diseases==TRUE){
  if(DM_men=="off"){
    basepop <- DM_menoff(basepop,lhs)
  }else if(DM_men=="on"){
    basepop <- DM(basepop,lhs)
  }
}
if("IHD" %in% diseases==TRUE){
  if(sesinteraction==1){
    basepop <- IHDInteraction(basepop,lhs)
  }else if(sesinteraction==0){
    basepop <- IHD(basepop,lhs)
  }
}
if("ISTR" %in% diseases==TRUE){
  basepop <- ISTR(basepop, lhs)
}
if("HYPHD" %in% diseases==TRUE){
  basepop <- HYPHD(basepop, lhs)
}
if("MVACC" %in% diseases==TRUE){
  basepop <- MVACC(basepop, lhs)
}
if("UIJ" %in% diseases==TRUE){
  basepop <- UIJ(basepop, lhs)
}

# calculate base rates if year = 2000)
if(y == 2000){
  rates <- calculate_base_rate(basepop,base_counts,diseases)
}

# update the base rate based on lhs file
if(update_base_rate==1){
  rates <- update_base_rate(rates, lhs, y)
}

basepop <- left_join(basepop, rates, by=c("cat"))

# now simulate mortality
basepop <- simulate_mortality(basepop, diseases)

# now loop through and summarise the N from each cause of death
summary_list <- list()
for (disease in diseases) {
  # Generate and add the summary to the list with automatic naming
  summary_list[[paste0(disease)]] <- basepop %>%
    mutate(ageCAT = cut(microsim.init.age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           inflation_factor = ifelse(ageCAT %in% age_inflated[[1]], inflation_factors[1],
                                     ifelse(ageCAT %in% age_inflated[[2]], inflation_factors[2], NA))) %>%
    group_by(ageCAT, microsim.init.sex, microsim.init.race, microsim.init.education) %>%
    summarise(!!paste0("mort_", disease) := sum(!!sym(paste0("mort_", disease))/inflation_factor),
              !!paste0("yll_", disease) := sum(!!sym(paste0("yll_", disease))/inflation_factor)) %>%
    rename(agecat=ageCAT)

}

DiseaseSummary[[paste(y)]] <- basepop %>%
  mutate(agecat = cut(microsim.init.age,
               breaks=c(0,24,34,44,54,64,74,79),
               labels=c("18-24","25-34","35-44", "45-54",
                        "55-64","65-74","75-79"))) %>%
  group_by(agecat, microsim.init.sex, microsim.init.race, microsim.init.education) %>% tally() %>%
  mutate(year=y)

# now join together to make a diseases dataframe for that year
for(disease in diseases){
  DiseaseSummary[[paste(y)]] <-
    left_join(DiseaseSummary[[paste(y)]], summary_list[[paste0(disease)]], by=c("agecat","microsim.init.sex",
                                                                                "microsim.init.race","microsim.init.education"))
}

# now sample the correct proportion of those to be removed (due to inflated mortality rate)
for (disease in diseases) {
  # function that removes individuals due to their original RR for that disease
  basepop <- remove_individuals(basepop, disease, age_inflated, inflation_factors)
}

basepop <- basepop %>% dplyr::select(-c(cat,prob))

}

# transition education for individuals aged 34 and under
if(updatingeducation==1){
  print("updating education")
  totransition <- basepop %>% filter(microsim.init.age<=34)
  tostay <- basepop %>% filter(microsim.init.age>34)
  totransition <- setup_education(totransition,y)
  totransition <- totransition %>% group_by(cat) %>% do(transition_ed(., education_transitions))
  totransition$microsimnewED <- totransition$newED
  totransition$microsim.init.education <- ifelse(totransition$microsimnewED=="LEHS","LEHS",
                                                 ifelse(totransition$microsimnewED=="SomeC1","SomeC",
                                                        ifelse(totransition$microsimnewED=="SomeC2","SomeC",
                                                               ifelse(totransition$microsimnewED=="SomeC3","SomeC",
                                                                      ifelse(totransition$microsimnewED=="College","College",NA)
                                                                      ))))
  totransition <- totransition %>% ungroup() %>% dplyr::select(-c(prob, state, year, newED,cat))
  basepop <- rbind(totransition, tostay)
}

# update alcohol use categories
if(updatingalcohol==1){
  print("updating alcohol use")
  # if(y %in% transitionyears==TRUE){
  basepop <- basepop %>% ungroup() %>% mutate(
    agecat = cut(microsim.init.age,
                 breaks=c(0,20,25,29,39,49,64,100),
                 labels=c("18-20","21-25","26-29","30-39","40-49","50-64","65+")),
    cat = paste(agecat, microsim.init.sex,
                                      microsim.init.race, microsim.init.education,
                                      AlcCAT, sep="_"),
                                prob = runif(nrow(.)))
  basepop <- basepop %>% group_by(cat) %>% do(transition_alcohol(., alcohol_transitions))
  basepop <- basepop %>%
    mutate(totransition = ifelse(AlcCAT == newALC, 0, ifelse(AlcCAT != newALC, 1, NA)),
           AlcCAT = newALC) %>% ungroup() %>% dplyr::select(-c(cat, prob, newALC))
  # }
  # allocate a numeric gpd for individuals based on model
  # allocate every year even when transitions are only every two years?
  basepop <- allocate_gramsperday(basepop, y, catcontmodel, DataDirectory)

  basepop <- update_former_drinker(basepop)
  # print(summary(basepop$formerdrinker))
  basepop$microsim.init.alc.gpd <- basepop$newgpd
  basepop$newgpd <- NULL
  basepop$totransition <-  NULL
  basepop$prop_former_drinker <- NULL
  basepop$n <- NULL
}

#delete anyone over 79
###then age everyone by 1 year and update age category
basepop <- basepop %>% mutate(microsim.init.age = microsim.init.age+1,
                              agecat = cut(microsim.init.age,
                                           breaks=c(0,19,24,34,44,54,64,74,100),
                                           labels=c("15-19","20-24","25-34","35-44","45-54","55-64",
                                                    "65-74","75-79")))
basepop <- subset(basepop, microsim.init.age<=79)

# add and remove migrants
if(y<2019){
model <- "SIMAH"
basepop <- inward_births_rate(basepop, migration_rates, y, brfss, model)
basepop <- inward_migration_rate(basepop, migration_rates, y, brfss)
basepop <- outward_migration_rate(basepop,migration_rates,y)
}

}
# save output - depending on which was selected
#### use a vector to contain the outputs we are interested in TODO
# indicator of how aggregated the results should be? - in the vector of outputs

if(output=="population"){
  Summary <- basepop %>%
    group_by(microsim.init.sex, agecat, microsim.init.race, microsimnewED) %>%
    summarize(count = n()) %>%
    mutate(percentage = round(count / sum(count) * 100, 1))
}else if(output=="mortality" & !is.null(diseases)){
  Summary <- postprocess_mortality(DiseaseSummary,diseases, death_counts) %>%
    mutate(seed = seed, samplenum = samplenum)
}else if(output=="mortality" & is.null(diseases)){
    Summary <- 0
  }else if(output=="demographics"){
    # add seed to the output file here TODO
  for(i in 1:length(PopPerYear)){
    PopPerYear[[i]]$agecat <- cut(PopPerYear[[i]]$microsim.init.age,
                                                  breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,100),
                                                  labels=c("18","19-24","25-29","30-34","35-39","40-44",
                                                           "45-49","50-54","55-59","60-64","65-69",
                                                           "70-74","75-79"))
    PopPerYear[[i]] <- as.data.table(PopPerYear[[i]])
    PopPerYear[[i]] <- PopPerYear[[i]][, .(n = .N), by = .(year, samplenum, seed, microsim.init.sex, microsim.init.race, microsim.init.education, microsim.init.age, agecat)]
  }
    Summary <- do.call(rbind,PopPerYear)
}else if(output=="alcohol"){
  CatSummary <- do.call(rbind,PopPerYear) %>% mutate(year=as.factor(as.character(year)),
                                                  samplenum=as.factor(samplenum),
                                                  microsim.init.sex=as.factor(microsim.init.sex),
                                                  microsim.init.race=as.factor(microsim.init.race),
                                                  microsim.init.education=as.factor(microsim.init.education),
                                                  agecat = cut(microsim.init.age,
                                                                    breaks=c(0,24,34,44,54,64,100),
                                                                    labels=c("18-24","25-34","35-44","45-54",
                                                                             "55-64","65+")),
                                                  agecat=as.factor(agecat),
                                                  AlcCAT=as.factor(AlcCAT)) %>%
    group_by(year, samplenum, seed, microsim.init.sex,microsim.init.race,agecat, microsim.init.education,
             AlcCAT, .drop=FALSE) %>% tally()
  MeanSummary <- do.call(rbind,PopPerYear) %>% mutate(year=as.factor(as.character(year)),
                                                      samplenum=as.factor(samplenum),
                                                      microsim.init.sex=as.factor(microsim.init.sex),
                                                      microsim.init.race=as.factor(microsim.init.race),
                                                      microsim.init.education=as.factor(microsim.init.education),
                                                      agecat=as.factor(agecat)) %>%
    group_by(year, samplenum, microsim.init.sex, microsim.init.education, .drop=FALSE) %>%
    filter(microsim.init.alc.gpd!=0) %>%
    summarise(meangpd = mean(microsim.init.alc.gpd))
  # add former drinkers and lifetime abstainers to this summary TODO
  Summary <- CatSummary
  # Summary <- list(CatSummary, MeanSummary)
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
