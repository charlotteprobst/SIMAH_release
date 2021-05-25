# create preliminary file for LE decomposition with aggregated US mortality data (2000 & 2015)

# libraries required:
library("tidyverse")
library("DemoDecomp")
library("dplyr")
library("reshape")
library("data.table")

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")
setwd("~/Documents/Promotion/Mortality US")

## Load the functions that go with this code
source("LE/2_do/1b_decomp_functions.R")

#############################################################################################################
#before starting with the decomposition, we have to get the mortality data into the right format

#load aggregated mortality data:
dMort <- read.csv("Mortality/3_out data/final_allethn_rates_00&15.csv")

# variable type should be factor and not character
class(dMort$race)
dMort$race <- as.factor(dMort$race)
class(dMort$edclass)
dMort$edclass <- as.factor(dMort$edclass)

## Aggregate: summarize the data to collapse one demographic dimension 
## Specify all factor variables you want to keep (and omit the one 
## you want to collapse)
dMort <- aggregate(.~ year + edclass + sex + age_gp, data =  dMort, FUN=sum)

## We have to calculate the rates again because we cannot simply sum them up
## define vectors for the total death counts and rates of interest.
## IMPORTANT: the order of the totals and the rates must match!! 
v.totals <- c("Tmort", "CD_NRmort", "CD_LRmort", "CD_HRmort", "NCD_NRmort", "NCD_LRmort", 
              "NCD_HRmort", "NCD_AAF1mort", "Inj_NRmort", "Inj_LRmort", 
              "Inj_AAF1mort", "HS_NRmort", "HS_LRmort", "HS_AAF1mort")

## these are the original rate variable names. You could also introduce the "mx_" 
## nomenclature here 
v.rates <- c("Trate", "mx_CD_NRrate", "mx_CD_LRrate", "mx_CD_HRrate", "mx_NCD_NRrate", 
             "mx_NCD_LRrate", "mx_NCD_HRrate", "mx_NCD_AAF1rate",  "mx_Inj_NRrate", 
             "mx_Inj_LRrate", "mx_Inj_AAF1rate", "mx_HS_NRrate", "mx_HS_LRrate", "mx_HS_AAF1rate") 

#select mortality rates of interest (CD_NRrate, CD_LRrate, CD_HRrate, NCD_NRrate, NCD_LRrate, NCD_HRrate, 
# NCD_AAF1rate, Inj_NRrate, Inj_LRrate, Inj_AAF1rate, HS_NRrate, HS_LRrate, HS_AAF1rate) 
#US_2000$TPop <- NULL (to delete just one column)
sel.vars <- c("year", "edclass", "sex", "age_gp", 
              "mx_CD_NRrate", "mx_CD_LRrate", "mx_CD_HRrate", "mx_NCD_NRrate", "mx_NCD_LRrate", "mx_NCD_HRrate", 
              "mx_NCD_AAF1rate", "mx_Inj_NRrate", "mx_Inj_LRrate", "mx_Inj_AAF1rate", "mx_HS_NRrate", 
              "mx_HS_LRrate", "mx_HS_AAF1rate") 

# Calculate the rates for all relevant causes of death
for (i in 1:length(v.totals)){
  dMort[, v.rates[i]] <- (dMort[, v.totals[i]]/dMort$TPop)*100000
} 
  
# Generate a variable to loop over
dMort$group <- paste0(dMort$sex, dMort$edclass)

# now you can loop over the unique values of this new variable
v.group <- unique(dMort$group)

for(i in 1:length(v.group)) {
  US_2000 <- filter(dMort, year=="2000" &  group == v.group[i]) ## this one would then be i
  US_2015 <- filter(dMort, year=="2015" &  group == v.group[i])
  
  US_2000 <- US_2000[, sel.vars] #to delete several columns at once. comment: This kept Trate. in our out?
  US_2015 <- US_2015[, sel.vars]
  
  US_2000$ax_2000 = 2.5
  US_2015$ax_2015 = 2.5
  
  # to start the first step of the decomposition
  US_2000_vector <- unlist(select(US_2000,starts_with("mx_")))
  US_2015_vector <- unlist(select(US_2015,starts_with("mx_")))
  
  decomp_results <-  horiuchi(func = life_table_causes,
                              pars1 = US_2000_vector,
                              pars2 = US_2015_vector,
                              age_vector = pull(US_2000,age_gp),
                              ax_vector = (pull(US_2000,ax_2000) + pull(US_2015,ax_2015))/2,
                              N = 100)
  #out_names <- c("CD_NR","CD_LR","CD_HR","NCD_NR","NCD_LR","NCD_HR","NCD_AAF1", "Inj_NR", "Inj_LR", "Inj_AAF1", "HS_NR", "HS_LR", "HS_AAF1")
  decomp_results_mat <- matrix(decomp_results,ncol = length(v.totals[-1])) # -1 to remove Tmort
  
  #Sum over age - this gives the contribution of each cause in terms of years of LE. Note that this should sum to difference LE between the beg and start periods
  cause_contributions <- apply(decomp_results_mat,2,sum)
  
  cause_contributions <-  array(cause_contributions, dim = c(1, length(cause_contributions)))
  
  #Can express as percent of total LE change
  cause_contributions_share <- cause_contributions / sum(cause_contributions)
  cause_contributions_share
  
  temp_contrib <- as.data.frame(cause_contributions) 
  colnames(temp_contrib) <- v.totals[-1]
  
  temp_share <- data.frame(cause_contributions_share)
  colnames(temp_share) <- v.totals[-1]
  
  temp_contrib$group <- v.group[i]
  temp_share$group <- v.group[i]
  
  if (i == 1) {

    dResults_contrib <- temp_contrib
    dResults_share <- temp_share
    
  } else {
    
    dResults_contrib <- rbind(temp_contrib, dResults_contrib)
    dResults_share <- rbind(temp_share, dResults_share)
  }
  
}
setDT(dResults_contrib, keep.rownames = TRUE)

write.csv(dResults_contrib,"LE/3_out data/dResults_contrib.csv" )
write.csv(dResults_share,"LE/3_out data/dResults_share.csv" )
