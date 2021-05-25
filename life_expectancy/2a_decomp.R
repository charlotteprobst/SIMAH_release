# create preliminary file for LE decomposition with aggregated US mortality data (2000-2018)

# libraries required:
library("tidyverse")
library("DemoDecomp")
library("dplyr")

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")

## Load the functions that go with this code
source("LE/2_do/1b_decomp_functions.R")


#############################################################################################################
#before starting with the decomposition, we have to geth the mortality data into the right format

#load aggregated mortality data:
dMort <- read.csv("Mortality/3_out data/final_allethn_rates_0018.csv")

## Aggregate: summarize the data to collapse one demographic dimension 
## Specify all factor variables you want to keep (and omitt the one 
## you want to collapse)
dMort <- aggregate(.~ year + edclass + sex + age_gp, data =  dMort, FUN=sum)

## We have to calculate the rates again because we cannot simply sum them up
## define vectors for the total death counts and rates of interest.
## IMPORTANT: the order of the totals and the rates must match!! 
v.totals <- c("Tmort", "CD_NRmort", "CD_LRmort", "CD_HRmort", "NCD_NRmort", "NCD_LRmort", 
              "NCD_HRmort", "NCD_AAF1mort", "Inj_NRmort", "Inj_LRmort", 
              "Inj_AAF1mort", "HS_NRmort", "HS_LRmort", "HS_AAF1mort")
## these are the original rate variabel names. You could also introcude the "mx_" 
## nomenclature here 
v.rates <- c("Trate", "CD_NRrate", "CD_LRrate", "CD_HRrate", "NCD_NRrate", 
             "NCD_LRrate", "NCD_HRrate", "NCD_AAF1rate",  "Inj_NRrate", 
             "Inj_LRrate", "Inj_AAF1rate", "HS_NRrate", "HS_LRrate", "HS_AAF1rate") 

# Calculate the rates for all relevant causes of death

for (i in 1:length(v.totals)){
  dMort[, v.rates[i]] <- (dMort[, v.totals[i]]/dMort$TPop)*100000
} 
  

## MESSAGE TO MIRIAM: the loop could start here. 
# Generate a variable to loop over. eg.:
dMort$group <- paste0(dMort$sex, dMort$edclass)
# now you can loop over the unique values of this new variable
v.group <- unique(dMort$group)
US_2000 <- filter(dMort, year=="2000" &  group == v.group[2]) ## this one would then be i
US_2015 <- filter(dMort, year=="2015" &  group == v.group[2])


#filter years (+  sex, educational attainment and specific rates of death) of interest
US_2000 <- filter(dMort, year=="2000" &  sex=="2" & edclass=="LEHS")
US_2015 <- filter(dMort, year=="2015" &  sex=="2" & edclass=="LEHS")


#select mortality rates of interest (CD_NRrate, CD_LRrate, CD_HRrate, NCD_NRrate, NCD_LRrate, NCD_HRrate, 
# NCD_AAF1rate, Inj_NRrate, Inj_LRrate, Inj_AAF1rate, HS_NRrate, HS_LRrate, HS_AAF1rate) 
#US_2000$TPop <- NULL (to delete just one column)
sel.vars <- c("year", "edclass", "sex", "age_gp", 
              "CD_NRrate", "CD_LRrate", "CD_HRrate", "NCD_NRrate", "NCD_LRrate", "NCD_HRrate", 
              "NCD_AAF1rate", "Inj_NRrate", "Inj_LRrate", "Inj_AAF1rate", "HS_NRrate", "HS_LRrate", "HS_AAF1rate") 

US_2000 <- US_2000[, sel.vars] #to delete several columns at once. comment: This kept Trate. in our out?
US_2015 <- US_2015[, sel.vars]

#add column with ax_value of 2.5 ()
ax_2000 <- 2.5
US_2000$ax_2000=ax_2000
ax_2015 <- 2.5
US_2015$ax_2015=ax_2015

#give columns of COD groups same beginning name "mx_"
US_2000 <- US_2000 %>% 
  rename(
    mx_CD_NRrate = CD_NRrate,
    mx_CD_LRrate = CD_LRrate,
    mx_CD_HRrate = CD_HRrate,
    mx_NCD_NRrate = NCD_NRrate,
    mx_NCD_LRrate = NCD_LRrate,
    mx_NCD_HRrate = NCD_HRrate,
    mx_NCD_AAF1rate = NCD_AAF1rate,
    mx_Inj_NRrate = Inj_NRrate,
    mx_Inj_LRrate = Inj_LRrate,
    mx_Inj_AAF1rate = Inj_AAF1rate,
    mx_HS_NRrate = HS_NRrate,
    mx_HS_LRrate = HS_LRrate,
    mx_HS_AAF1rate = HS_AAF1rate
  )

write.csv(US_2000, "LE/3_out data/US_2000.csv")

US_2015 <- US_2015 %>% 
  rename(
    mx_CD_NRrate = CD_NRrate,
    mx_CD_LRrate = CD_LRrate,
    mx_CD_HRrate = CD_HRrate,
    mx_NCD_NRrate = NCD_NRrate,
    mx_NCD_LRrate = NCD_LRrate,
    mx_NCD_HRrate = NCD_HRrate,
    mx_NCD_AAF1rate = NCD_AAF1rate,
    mx_Inj_NRrate = Inj_NRrate,
    mx_Inj_LRrate = Inj_LRrate,
    mx_Inj_AAF1rate = Inj_AAF1rate,
    mx_HS_NRrate = HS_NRrate,
    mx_HS_LRrate = HS_LRrate,
    mx_HS_AAF1rate = HS_AAF1rate
  )

#################################################################################################
# to start the first step of the decomposition
US_2000_vector <- unlist(select(US_2000,starts_with("mx_")))
US_2015_vector <- unlist(select(US_2015,starts_with("mx")))


#DECOMPOSITION
# input into Horiuchi function
# func = the function that takes the input vector and converts it into the things to be decomposed (LE function)
# "pars" inputs are vectors that we want to evaluate the contribution of (age-cause-specific rates)
# pars1: intial contribution at time 1
# pars2: final contribution at time 2
#LE function requires that we also give it a vector of ages and ax-values 
# tells function it doesn't need to decompose ages and ax
#N = the number of intervals to break the difference between the initial and final point into (set at 100)

## Beginning of the loop

decomp_results <- horiuchi(func = life_table_causes,
                           pars1 = US_2000_vector,
                           pars2 = US_2015_vector,
                           age_vector = pull(US_2000,age_gp),
                           ax_vector = (pull(US_2000,ax_2000) + pull(US_2015,ax_2015))/2,
                           N = 100)
decomp_results

# output is a vector that is as long as the input vectors where each value is the contribution of that specific
#(cont'd) age-cause-specific rate to overall change in LE (5 years)
# reshape into age-cause-specific matrix
# show overall cause-specific contributions by summing over age

#Matrix-a-tize it
out_names <- c("CD_NR","CD_LR","CD_HR","NCD_NR","NCD_LR","NCD_HR","NCD_AAF1", "Inj_NR", "Inj_LR", "Inj_AAF1", "HS_NR", "HS_LR", "HS_AAF1")
decomp_results_mat <- matrix(decomp_results,ncol = length(out_names))
colnames(decomp_results_mat) <- out_names

#Sum over age - this gives the contribution of each cause in terms of years of LE. Note that this should sum to difference LE between the beg and start periods
cause_contributions <- apply(decomp_results_mat,2,sum)

#Can express as percent of total LE change
cause_contributions_share <- cause_contributions / sum(cause_contributions)
cause_contributions_share

dResultsGroup <- data.frame(rbind(cause_contributions, cause_contributions_share))

dResultsGroup$group <- v.group[2]

