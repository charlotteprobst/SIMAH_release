# Calculate LE decomposition for 2018 to 2020 
# by SES and race and ethnicity 
# Mortality data NCHS
# Population data: ACS 
# Project: SIMAH


# libraries required:
library("dplyr")
library("DemoDecomp")
library("tidyverse")

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")
setwd("~/Google Drive/SIMAH Sheffield/")

## Load the functions that go with this code
source("SIMAH_code/life_expectancy/2b_decomp_functions.R")

#############################################################################################################
#before starting with the decomposition, we have to get the mortality data into the right format
# switch between types of ACS population counts 
# (0) = raw ACS (ACS experimental weights), (1) = modelled ACS 
ACS_type <- 0

#load aggregated mortality data:
dMort <- read.csv("SIMAH_workplace/mortality/3_out data/US_mortality_rates_0020.csv")
#load population data (raw vs modelled)
if(ACS_type==0){
dPop <- read.csv("SIMAH_workplace/demography/ACS_popcounts_2000_2020.csv")
}else if(ACS_type==1){
  dPop <- read.csv("SIMAH_workplace/demography/ACS_popcounts_2000_2020_predicted2020.csv")  
  dPop_weights_raw <- readRDS("SIMAH_workplace/ACS/rep_weights_2020.RDS")
}
dPop <- filter(dPop, state == "USA")
dPop <- filter(dPop, year > 2017)
dPop <- select (dPop,-c(state))
dMort <- filter(dMort, year > 2017)
dMort <- inner_join(dMort, dPop)

# variable type should be factor and not character
dMort$race <- as.factor(dMort$race)
dMort$edclass <- as.factor(dMort$edclass)

## Aggregate: summarize the data to collapse one demographic dimension 
## Specify all factor variables you want to keep (and omit the one 
## you want to collapse)
dMort_raw <- dMort
dMort <- aggregate(.~ year + edclass + race + sex + age_gp, data =  dMort, FUN=sum)
dMort_d <- aggregate(.~ year + edclass + sex + race + age_gp, data =  dMort_raw, FUN=sum)

## We have to calculate the rates again
v.totals <- names(dMort)[grepl("mort", names(dMort))]
v.rates <- names(dMort)[grepl("rate", names(dMort))]
v.rates <- paste0(c("", rep("mx_", length(v.rates)-1)),v.rates)

#select variables
sel.vars <- c("year", "edclass", "sex", "age_gp", v.rates) 

####### DECOMP BY SEX AND SES  ########
# first set up 2020 weights 
# first aggregate by only year sex age_gp and edclass 

if(ACS_type==0){
dMort2020 <- dMort %>% filter(year==2020) %>% dplyr::select(-c(race,TPop))
mortlist <- list()

# join with TPop for different weights and calculate mortality from different causes
for(i in 1:length(dPop_weights_raw)){
  dPop_weights[[i]] <- dPop_weights_raw[[i]] %>% 
    group_by(year, sex, age_gp, edclass) %>% summarise(TPop=sum(TPop)) %>% 
    mutate(age_gp=as.integer(age_gp))
}

mortlist <- list()

for(i in 1:length(dPop_weights)){
mortlist[[i]] <- left_join(dMort2020, dPop_weights[[i]]) %>% 
  pivot_longer(cols=c(Tmort:RESTmort)) %>% 
  mutate(rate = value/TPop,
         name = gsub("mort","rate",name),
         name = ifelse(name=="Trate","Trate",
                       paste0("mx_",name))) %>% 
  dplyr::select(-value) %>% 
  pivot_wider(names_from=name, values_from=rate)
}
}

# Calculate the rates for all relevant causes of death
for (i in 1:length(v.totals)){
  dMort[, v.rates[i]] <- (dMort[, v.totals[i]]/dMort$TPop)
} 

# Generate a variable to loop over
dMort$group <- apply(dMort[ , c("sex", "edclass") ] , 1 , paste , collapse = "_" )

# now you can loop over the unique values of this new variable
v.group <- unique(dMort$group)

v.year1 <- c(2018, 2019)
v.year2 <- c(2019, 2020)
for (j in (1:length(v.year1))){
  year1 <- v.year1[j]
  year2 <- v.year2[j]
  for(i in 1:length(v.group)) {
    US_y1 <- filter(dMort, year==year1 &  group == v.group[i]) ## this one would then be i
    US_y2 <- filter(dMort, year==year2 &  group == v.group[i])
    
    US_y1 <- US_y1[, sel.vars] #to delete several columns at once. comment: This kept Trate. in our out?
    US_y2 <- US_y2[, sel.vars]
    
    US_y1$ax = c(3.5,rep(2.5,11), 6.99) # midpoint of age range. Check age groups again. 
    US_y2$ax = c(3.5,rep(2.5,11), 6.99)
    
    # to start the first step of the decomposition
    US_y1_vector <- unlist(select(US_y1,starts_with("mx_")))
    US_y2_vector <- unlist(select(US_y2,starts_with("mx_")))
    
    decomp_results <-  horiuchi(func = life_table_causes,
                                pars1 = US_y1_vector,
                                pars2 = US_y2_vector,
                                age_vector = pull(US_y1,age_gp),
                                ax_vector = (pull(US_y1,ax) + pull(US_y2,ax))/2,
                                N = 100)
    decomp_results_mat <- matrix(decomp_results,ncol = length(v.totals[-1])) # -1 to remove Tmort
    
    #Sum over age - this gives the contribution of each cause in terms of years of LE. Note that this should sum to difference LE between the beg and start periods
    cause_contributions <- apply(decomp_results_mat,2,sum)
    
    cause_contributions <-  array(cause_contributions, dim = c(1, length(cause_contributions)))
    
    temp_contrib <- as.data.frame(cause_contributions) 
    colnames(temp_contrib) <- v.totals[-1]

    temp_contrib$group <- v.group[i]
    temp_contrib$start_year <- v.year1[j]
    temp_contrib$end_year <- v.year2[j]
   
    temp_contrib$LE1 <-  life_table_causes(nmx_by_cause_vector = US_y1_vector, age_vector = pull(US_y1,age_gp),
                                     ax_vector = pull(US_y1, ax)) + min(pull(US_y1, age_gp))
    temp_contrib$LE2 <-  life_table_causes(nmx_by_cause_vector = US_y2_vector, age_vector = pull(US_y2,age_gp),
                                     ax_vector = pull(US_y2, ax)) + min(pull(US_y2, age_gp))
    
    if (i == 1 & j == 1) {
      
      dResults_contrib <- temp_contrib
      
      
    } else {
      
      dResults_contrib <- rbind(temp_contrib, dResults_contrib)
      
    }
  } 
  
}

dResults_contrib <- separate(dResults_contrib, col = group, into = c("sex","edclass"), sep = "_")
dResults_contrib$edclass <- factor(dResults_contrib$edclass, 
                                   levels = c( "LEHS", "SomeC", "College"))
dResults_contrib$sex <- as.factor(dResults_contrib$sex)
dResults_contrib <- dResults_contrib[order(dResults_contrib$start_year, dResults_contrib$sex, dResults_contrib$edclass), ]

if(ACS_type==0){
write.csv(dResults_contrib,paste0("SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/dResults_contrib_", v.year1[1], "_", max(v.year2), "ACS.csv") )
write.csv(dMort, "SIMAH_workplace/life_expectancy/2_out_data/dMort_0020.csv")
}else if(ACS_type==1){
  write.csv(dResults_contrib,paste0("SIMAH_workplace/life_expectancy/2_out_data/dResults_ACSmodel_contrib_", v.year1[1], "_", max(v.year2), "ACS.csv") )
  write.csv(dMort, "SIMAH_workplace/life_expectancy/2_out_data/dMort_0020_ACSmodel.csv")
}

if(ACS_type==0){
# now loop over and get the results for 2020 weights
for(i in 1:length(mortlist)){
  mortlist[[i]]$group <- apply(mortlist[[i]][ , c("sex", "edclass") ] , 1 , paste , collapse = "_" )
  mortlist[[i]]$weight <- i
}

dResults_contrib_list <- list()

# only get the results for 2019 - 2020 (results above will be the same for 2018-2019)
# for(k in 1:length(mortlist)){
# for(k in 1:3){
for(k in 1:length(mortlist)){
  year1 <- 2019
  year2 <- 2020
  for(i in 1:length(v.group)) {
    US_y1 <- filter(dMort, year==year1 &  group == v.group[i]) ## this one would then be i
    US_y2 <- filter(mortlist[[k]], year==year2 &  group == v.group[i])
    
    US_y1 <- US_y1[, sel.vars] #to delete several columns at once. comment: This kept Trate. in our out?
    US_y2 <- US_y2[, sel.vars]
    
    US_y1$ax = c(3.5,rep(2.5,11), 6.99) # midpoint of age range. Check age groups again. 
    US_y2$ax = c(3.5,rep(2.5,11), 6.99)
    
    # to start the first step of the decomposition
    US_y1_vector <- unlist(select(US_y1,starts_with("mx_")))
    US_y2_vector <- unlist(select(US_y2,starts_with("mx_")))
    
    decomp_results <-  horiuchi(func = life_table_causes,
                                pars1 = US_y1_vector,
                                pars2 = US_y2_vector,
                                age_vector = pull(US_y1,age_gp),
                                ax_vector = (pull(US_y1,ax) + pull(US_y2,ax))/2,
                                N = 100)
    decomp_results_mat <- matrix(decomp_results,ncol = length(v.totals[-1])) # -1 to remove Tmort
    
    #Sum over age - this gives the contribution of each cause in terms of years of LE. Note that this should sum to difference LE between the beg and start periods
    cause_contributions <- apply(decomp_results_mat,2,sum)
    
    cause_contributions <-  array(cause_contributions, dim = c(1, length(cause_contributions)))
    
    temp_contrib <- as.data.frame(cause_contributions) 
    colnames(temp_contrib) <- v.totals[-1]
    
    temp_contrib$group <- v.group[i]
    temp_contrib$start_year <- v.year1[j]
    temp_contrib$end_year <- v.year2[j]
    
    temp_contrib$LE1 <-  life_table_causes(nmx_by_cause_vector = US_y1_vector, age_vector = pull(US_y1,age_gp),
                                           ax_vector = pull(US_y1, ax)) + min(pull(US_y1, age_gp))
    temp_contrib$LE2 <-  life_table_causes(nmx_by_cause_vector = US_y2_vector, age_vector = pull(US_y2,age_gp),
                                           ax_vector = pull(US_y2, ax)) + min(pull(US_y2, age_gp))
    
    if (i == 1 & j == 1) {
      
      dResults_contrib_list[[paste(k)]] <- temp_contrib
      
      
    } else {
      
      dResults_contrib_list[[paste(k)]] <- rbind(temp_contrib, dResults_contrib_list[[paste(k)]])
      
    }
  } 
  
}

for(i in 1:length(dResults_contrib_list)){
  dResults_contrib_list[[i]] <- separate(dResults_contrib_list[[i]], col = group, into = c("sex","edclass"), sep = "_")
  dResults_contrib_list[[i]]$edclass <- factor(dResults_contrib_list[[i]]$edclass, 
                                                      levels = c( "LEHS", "SomeC", "College"))
  dResults_contrib_list[[i]]$sex <- as.factor(dResults_contrib_list[[i]]$sex)
  dResults_contrib_list[[i]] <- dResults_contrib_list[[i]][order(dResults_contrib_list[[i]]$start_year, dResults_contrib_list[[i]]$sex, dResults_contrib_list[[i]]$edclass), ]
  dResults_contrib_list[[i]]$weight <- i
  dResults_contrib_list[[i]]$group <- NULL
}

dResults_weights <- do.call(rbind, dResults_contrib_list)

write.csv(dResults_weights,paste0("SIMAH_workplace/life_expectancy/2_out_data/dResults_contrib_", v.year1[1], "_", max(v.year2), "ACS_2020weights.csv") )
write.csv(mortlist, "SIMAH_workplace/life_expectancy/2_out_data/dMort_0020_2020weights.csv")

####### DECOMP BY SEX, SES AND RACE  ########
# calculate rates by race and ethnicity for 2020 weights 
dMort2020 <- dMort_d %>% filter(year==2020) %>% dplyr::select(-c(TPop))
mortlist <- list()
dPop_weights <- list()
# join with TPop for different weights and calculate mortality from different causes
for(i in 1:length(dPop_weights_raw)){
  dPop_weights[[i]] <- dPop_weights_raw[[i]] %>% 
    group_by(year, sex, race, age_gp, edclass) %>% summarise(TPop=sum(TPop)) %>% 
    mutate(age_gp=as.integer(age_gp))
}

mortlist <- list()

for(i in 1:length(dPop_weights)){
  mortlist[[i]] <- left_join(dMort2020, dPop_weights[[i]]) %>% 
    pivot_longer(cols=c(Tmort:RESTmort)) %>% 
    mutate(rate = value/TPop,
           name = gsub("mort","rate",name),
           name = ifelse(name=="Trate","Trate",
                         paste0("mx_",name))) %>% 
    dplyr::select(-value) %>% 
    pivot_wider(names_from=name, values_from=rate) %>% 
    mutate(group = paste(sex, edclass, race, sep="_"))
}
}

# Calculate the rates for all relevant causes of death
for (i in 1:length(v.totals)){
  dMort_d[, v.rates[i]] <- (dMort_d[, v.totals[i]]/dMort_d$TPop)
} 

# Generate a variable to loop over
dMort_d$group <- apply(dMort_d[ , c("sex", "edclass", "race") ] , 1 , paste , collapse = "_" )

# now you can loop over the unique values of this new variable
v.group <- unique(dMort_d$group)

v.year1 <- c(2018, 2019)
v.year2 <- c(2019, 2020)
for (j in (1:length(v.year1))){
  year1 <- v.year1[j]
  year2 <- v.year2[j]
  for(i in 1:length(v.group)) {
    US_y1 <- filter(dMort_d, year==year1 &  group == v.group[i]) ## this one would then be i
    US_y2 <- filter(dMort_d, year==year2 &  group == v.group[i])
    
    US_y1 <- US_y1[, sel.vars] #to delete several columns at once. comment: This kept Trate. in our out?
    US_y2 <- US_y2[, sel.vars]
    
    US_y1$ax = c(3.5,rep(2.5,11), 6.99) # midpoint of age range. Check age groups again. 
    US_y2$ax = c(3.5,rep(2.5,11), 6.99)
    
    # to start the first step of the decomposition
    US_y1_vector <- unlist(select(US_y1,starts_with("mx_")))
    US_y2_vector <- unlist(select(US_y2,starts_with("mx_")))
    
    decomp_results <-  horiuchi(func = life_table_causes,
                                pars1 = US_y1_vector,
                                pars2 = US_y2_vector,
                                age_vector = pull(US_y1,age_gp),
                                ax_vector = (pull(US_y1,ax) + pull(US_y2,ax))/2,
                                N = 100)
    decomp_results_mat <- matrix(decomp_results,ncol = length(v.totals[-1])) # -1 to remove Tmort
    
    #Sum over age - this gives the contribution of each cause in terms of years of LE. Note that this should sum to difference LE between the beg and start periods
    cause_contributions <- apply(decomp_results_mat,2,sum)
    
    cause_contributions <-  array(cause_contributions, dim = c(1, length(cause_contributions)))
    
    temp_contrib <- as.data.frame(cause_contributions) 
    colnames(temp_contrib) <- v.totals[-1]
    
    temp_contrib$group <- v.group[i]
    temp_contrib$start_year <- v.year1[j]
    temp_contrib$end_year <- v.year2[j]
    
    temp_contrib$LE1 <-  life_table_causes(nmx_by_cause_vector = US_y1_vector, age_vector = pull(US_y1,age_gp),
                                           ax_vector = pull(US_y1, ax)) + min(pull(US_y1, age_gp))
    temp_contrib$LE2 <-  life_table_causes(nmx_by_cause_vector = US_y2_vector, age_vector = pull(US_y2,age_gp),
                                           ax_vector = pull(US_y2, ax)) + min(pull(US_y2, age_gp))
    
    if (i == 1 & j == 1) {
      
      dResults_contrib <- temp_contrib
      
      
    } else {
      
      dResults_contrib <- rbind(temp_contrib, dResults_contrib)
      
    }
  } 
  
}

dResults_contrib <- separate(dResults_contrib, col = group, into = c("sex","edclass", "race"), sep = "_")
dResults_contrib$edclass <- factor(dResults_contrib$edclass, 
                                   levels = c( "LEHS", "SomeC", "College"))
dResults_contrib$sex <- as.factor(dResults_contrib$sex)
dResults_contrib <- dResults_contrib[order(dResults_contrib$start_year, dResults_contrib$sex, dResults_contrib$edclass, dResults_contrib$race), ]
if(ACS_type==0){
write.csv(dResults_contrib,paste0("SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/dResults_contrib_", v.year1[1], "_", max(v.year2), "race_ACS.csv") )
write.csv(dMort_d, "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/dMort_race_0020.csv")
}else if(ACS_type==1){
  write.csv(dResults_contrib,paste0("SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/dResults_ACSmodel_contrib_", v.year1[1], "_", max(v.year2), "race_ACS.csv") )
  write.csv(dMort_d, "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/dMort_race_0020_ACSmodel.csv")  
}

if(ACS_type==0){
dResults_contrib_list <- list()
# now loop over and get 2020 weights version for race split 
# for(k in 1:3){
  for(k in 1:length(mortlist)){
  year1 <- 2019
  year2 <- 2020
  for(i in 1:length(v.group)) {
    US_y1 <- filter(dMort_d, year==year1 &  group == v.group[i]) ## this one would then be i
    US_y2 <- filter(mortlist[[k]], year==year2 &  group == v.group[i])
    
    US_y1 <- US_y1[, sel.vars] #to delete several columns at once. comment: This kept Trate. in our out?
    US_y2 <- US_y2[, sel.vars]
    
    US_y1$ax = c(3.5,rep(2.5,11), 6.99) # midpoint of age range. Check age groups again. 
    US_y2$ax = c(3.5,rep(2.5,11), 6.99)
    
    # to start the first step of the decomposition
    US_y1_vector <- unlist(select(US_y1,starts_with("mx_")))
    US_y2_vector <- unlist(select(US_y2,starts_with("mx_")))
    
    decomp_results <-  horiuchi(func = life_table_causes,
                                pars1 = US_y1_vector,
                                pars2 = US_y2_vector,
                                age_vector = pull(US_y1,age_gp),
                                ax_vector = (pull(US_y1,ax) + pull(US_y2,ax))/2,
                                N = 100)
    decomp_results_mat <- matrix(decomp_results,ncol = length(v.totals[-1])) # -1 to remove Tmort
    
    #Sum over age - this gives the contribution of each cause in terms of years of LE. Note that this should sum to difference LE between the beg and start periods
    cause_contributions <- apply(decomp_results_mat,2,sum)
    
    cause_contributions <-  array(cause_contributions, dim = c(1, length(cause_contributions)))
    
    temp_contrib <- as.data.frame(cause_contributions) 
    colnames(temp_contrib) <- v.totals[-1]
    
    temp_contrib$group <- v.group[i]
    temp_contrib$start_year <- v.year1[j]
    temp_contrib$end_year <- v.year2[j]
    
    temp_contrib$LE1 <-  life_table_causes(nmx_by_cause_vector = US_y1_vector, age_vector = pull(US_y1,age_gp),
                                           ax_vector = pull(US_y1, ax)) + min(pull(US_y1, age_gp))
    temp_contrib$LE2 <-  life_table_causes(nmx_by_cause_vector = US_y2_vector, age_vector = pull(US_y2,age_gp),
                                           ax_vector = pull(US_y2, ax)) + min(pull(US_y2, age_gp))
    
    if (i == 1 & j == 1) {
      
      dResults_contrib_list[[paste(k)]] <- temp_contrib
      
      
    } else {
      
      dResults_contrib_list[[paste(k)]] <- rbind(temp_contrib, dResults_contrib_list[[paste(k)]])
      
    }
  } 
  
}

for(i in 1:length(dResults_contrib_list)){
  dResults_contrib_list[[i]] <- separate(dResults_contrib_list[[i]], col = group, into = c("sex","edclass","race"), sep = "_")
  dResults_contrib_list[[i]]$edclass <- factor(dResults_contrib_list[[i]]$edclass, 
                                               levels = c( "LEHS", "SomeC", "College"))
  dResults_contrib_list[[i]]$sex <- as.factor(dResults_contrib_list[[i]]$sex)
  dResults_contrib_list[[i]] <- dResults_contrib_list[[i]][order(dResults_contrib_list[[i]]$start_year, dResults_contrib_list[[i]]$sex, dResults_contrib_list[[i]]$edclass), ]
  dResults_contrib_list[[i]]$weight <- i
  dResults_contrib_list[[i]]$group <- NULL
}

dResults_weights <- do.call(rbind, dResults_contrib_list)
write.csv(dResults_weights,paste0("SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/dResults_contrib_", v.year1[1], "_", max(v.year2), "race_ACSweights.csv") )
}