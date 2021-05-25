# create preliminary file for LE decomposition with aggregated US mortality data (2000 & 2015)

# libraries required:
library("tidyverse")
library("DemoDecomp")
library("dplyr")
library("reshape")
library("data.table")

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")
#setwd("~/Documents/Promotion/Mortality US")

## Load the functions that go with this code
source("LE/2_do/2b_decomp_functions.R")

#############################################################################################################
#before starting with the decomposition, we have to get the mortality data into the right format

#load aggregated mortality data:
dMort <- read.csv("Mortality/3_out data/allethn_rates_0018_final.csv")
#dCensus <- read.csv("Demography/population_counts_census_ACS.csv")

#dMort <- select(dMort, -"TPop")
#dMort <- merge(dMort, dCensus, by = c("year", "edclass", "sex", "age_gp", "race"))

# variable type should be factor and not character
class(dMort$race)
dMort$race <- as.factor(dMort$race)
class(dMort$edclass)
dMort$edclass <- as.factor(dMort$edclass)

## Aggregate: summarize the data to collapse one demographic dimension 
## Specify all factor variables you want to keep (and omit the one 
## you want to collapse)
dMort <- aggregate(.~ year + edclass + sex + age_gp, data =  dMort, FUN=sum)
dMort_t <- aggregate(.~ year + age_gp, data =  dMort, FUN=sum)


## We have to calculate the rates again because we cannot simply sum them up
## define vectors for the total death counts and rates of interest.
## IMPORTANT: the order of the totals and the rates must match!! 
v.totals <- c("Tmort", "LVDCmort", "DMmort", "IHDmort", "STRmort", 
              "HYPHDmort", "AUDmort", "UIJmort", "MVACCmort", "IJmort", "RESTmort")

## these are the original rate variable names. You could also introduce the "mx_" 
## nomenclature here 
v.rates <- c("Trate", "mx_LVDCrate", "mx_DMrate", "mx_IHDrate", "mx_STRrate", 
             "mx_HYPHDrate", "mx_AUDrate", "mx_UIJrate", "mx_MVACCrate", 
             "mx_IJrate", "mx_RESTrate") 

#select mortality rates of interest (CD_NRrate, CD_LRrate, CD_HRrate, NCD_NRrate, NCD_LRrate, NCD_HRrate, 
# NCD_AAF1rate, Inj_NRrate, Inj_LRrate, Inj_AAF1rate, HS_NRrate, HS_LRrate, HS_AAF1rate) 
#US_2000$TPop <- NULL (to delete just one column)
sel.vars <- c("year", "edclass", "sex", "age_gp", v.rates) 

# Calculate the rates for all relevant causes of death
for (i in 1:length(v.totals)){
  dMort[, v.rates[i]] <- (dMort[, v.totals[i]]/dMort$TPop)
} 

# Generate a variable to loop over
dMort$group <- apply(dMort[ , c("sex", "edclass") ] , 1 , paste , collapse = "_" )

# now you can loop over the unique values of this new variable
v.group <- unique(dMort$group)

v.year1 <- c(2000:2017)
v.year2 <- c(2001:2018)
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

#write.csv(dResults_contrib,paste0("LE/3_out data/dResults_contrib_", v.year1[1], "_", v.year2[1], "_Census.csv")  )
write.csv(dResults_contrib,paste0("LE/3_out data/dResults_contrib_", v.year1[1], "_", max(v.year2), "CPS.csv") )
