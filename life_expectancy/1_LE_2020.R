# libraries required:
library("tidyverse")
library("DemoDecomp")
library("dplyr")
library("reshape")
library("data.table")

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")

## Load the functions that go with this code
source("SIMAH_code/life_expectancy/2b_decomp_functions.R")

#############################################################################################################
#before calculating the life expectancy, we have to get the mortality data into the right format

#load aggregated mortality data:
dMort <- read.csv("SIMAH_workplace/mortality/3_out data/allethn_sumCOD_0020_LE_decomp.csv")
dPop <- read.csv("SIMAH_workplace/demography/ACS_popcounts_2000_2020.csv")
dPop <- filter(dPop, state == "USA")
dPop <- select (dPop,-c(state))

dMort <- inner_join(dMort, dPop)


# variable type should be factor and not character
class(dMort$race)
dMort$race <- as.factor(dMort$race)
class(dMort$edclass)
dMort$edclass <- as.factor(dMort$edclass)

## To aggregate some cause of death categories
dMort$RESTmort <- dMort$RESTmort + dMort$CANmort
dMort$RESTmort <- dMort$RESTmort + dMort$PANCmort
dMort$RESTmort <- dMort$RESTmort + dMort$HSTRmort

dMort$IHDmort <- dMort$IHDmort + dMort$ISTRmort

#  Delete variables that are no longer needed 
dMort <- dMort %>% select(-c(CANmort, PANCmort, ISTRmort, HSTRmort))

## Aggregate: summarize the data to collapse one demographic dimension 
## Specify all factor variables you want to keep (and omit the one 
## you want to collapse)
dMort <- aggregate(.~ year + edclass + sex + age_gp, data =  dMort, FUN=sum)
dMort_t <- aggregate(.~ year + sex + age_gp, data =  dMort, FUN=sum)



## We have to calculate the rates again because we cannot simply sum them up
## define vectors for the total death counts and rates of interest.
## IMPORTANT: the order of the totals and the rates must match!! 
## We have to calculate the rates again 
v.totals <- c("Tmort", "LVDCmort", "DMmort", "IHDmort",
              "HYPHDmort", "AUDmort", "UIJmort", "MVACCmort", "IJmort",  "RESTmort")

## Introduce the "mx_" nomenclature  
v.rates <- c("Trate", "mx_LVDCrate", "mx_DMrate", "mx_IHDrate", 
             "mx_HYPHDrate", "mx_AUDrate", "mx_UIJrate", "mx_MVACCrate", 
             "mx_IJrate",  "mx_RESTrate")

###############################################################################
# LE by sex
###############################################################################

sel.vars <- c("year", "sex", "age_gp", v.rates) 

# Calculate the rates for all relevant causes of death
for (i in 1:length(v.totals)){
      dMort_t[, v.rates[i]] <- (dMort_t[, v.totals[i]]/dMort_t$TPop)
} 


# Generate a variable to loop over
dMort_t$group <- dMort_t$sex

# now you can loop over the unique values of this new variable
v.group <- unique(dMort_t$group)

v.year1 <- c(2000:2020)
for (j in (1:length(v.year1))){
      year1 <- v.year1[j]
      for(i in 1:length(v.group)) {
            US_y1 <- filter(dMort_t, year==year1 &  group == v.group[i]) ## this one would then be i
            
            US_y1 <- US_y1[, sel.vars] #to delete several columns at once. comment: This kept Trate. in our out?
            
            US_y1$ax = c(3.5,rep(2.5,11), 6.99) # midpoint of age range. Check age groups again. 
            
            US_y1_vector <- unlist(select(US_y1,starts_with("mx_")))
            
            le_results <-  life_table_causes(nmx_by_cause_vector = US_y1_vector, age_vector = pull(US_y1,age_gp),
                                             ax_vector = pull(US_y1,ax))
            le_results <- as.data.frame(c(le_results + min(pull(US_y1, age_gp)))) # I am adding the age at baseline here
            le_results$year <- v.year1[j]
            le_results$group <- v.group[i]
            
            if (j == 1 & i == 1) {
                  
                  dle_results <- le_results
                  
            } else {
                  
                  dle_results <- rbind(dle_results, le_results)
            }
            
      } 
      
}

names(dle_results) <-  c("Life_expectancy", "Year", "Sex")
write.csv(dle_results, "SIMAH_workplace/life_expectancy/2_out_data/LifeExpectancy_sex_ACS_2020.csv")


###############################################################################
# LE by sex and SES
###############################################################################

sel.vars <- c("year", "edclass", "sex", "age_gp", v.rates) 

# Calculate the rates for all relevant causes of death
for (i in 1:length(v.totals)){
      dMort[, v.rates[i]] <- (dMort[, v.totals[i]]/dMort$TPop)
} 

# Generate a variable to loop over
dMort$group <- apply(dMort[ , c("sex", "edclass") ] , 1 , paste , collapse = "_" )

# now you can loop over the unique values of this new variable
v.group <- unique(dMort$group)

v.year1 <- c(2000:2020)
for (j in (1:length(v.year1))){
      year1 <- v.year1[j]
      for(i in 1:length(v.group)) {
            US_y1 <- filter(dMort, year==year1 &  group == v.group[i]) ## this one would then be i

            US_y1 <- US_y1[, sel.vars] #to delete several columns at once. comment: This kept Trate. in our out?

            US_y1$ax = c(3.5,rep(2.5,11), 6.99) # midpoint of age range. Check age groups again. 

            US_y1_vector <- unlist(select(US_y1,starts_with("mx_")))
            
            le_results <-  life_table_causes(nmx_by_cause_vector = US_y1_vector, age_vector = pull(US_y1,age_gp),
                                             ax_vector = pull(US_y1,ax))
            le_results <- as.data.frame(c(le_results + min(pull(US_y1, age_gp)))) # I am adding the age at baseline here
            le_results$year <- v.year1[j]
            le_results$group <- v.group[i]
            
            if (j == 1 & i == 1) {
                  
                  dle_results <- le_results
                  
            } else {
                  
                  dle_results <- rbind(dle_results, le_results)
            }
      
      } 
      
}


dle_results <- separate(dle_results, col = group, into = c("sex","edclass"), sep = "_")
dle_results$edclass <- factor(dle_results$edclass, 
                                   levels = c( "LEHS", "SomeC", "College"))
dle_results$sex <- as.factor(dle_results$sex)
dle_results <- dle_results[order(dle_results$year, dle_results$sex, dle_results$edclass), ]
names(dle_results) <-  c("Life_expectancy", "Year", "Sex", "SES" )


write.csv(dle_results, "SIMAH_workplace/life_expectancy/2_out_data/LifeExpectancy_sex_SES_ACS_2020.csv")


