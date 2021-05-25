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
source("LE/2_do/1b_decomp_functions.R")

#############################################################################################################
#before starting with the decomposition, we have to get the mortality data into the right format

#load aggregated mortality data:
dMort <- read.csv("Mortality/3_out data/allethn_rates_0018_final.csv")

# variable type should be factor and not character
class(dMort$race)
dMort$race <- as.factor(dMort$race)
class(dMort$edclass)
dMort$edclass <- factor(dMort$edclass, 
                                 levels = c( "LEHS", "SomeC", "College"))

## Aggregate: summarize the data to collapse one demographic dimension 
## Specify all factor variables you want to keep (and omit the one 
## you want to collapse)
dMort <- aggregate(.~ year + edclass + sex + age_gp, data =  dMort, FUN=sum) # age_gp age standardized would be better
#dMort <- aggregate(.~ year + edclass + sex , data =  dMort, FUN=sum) # age_gp age standardized would be better
totals <- aggregate(.~ year , data =  dMort, FUN=sum) # age_gp age standardized would be better
write.csv(totals, "Mortality/3_out data/totals.csv")

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
sel.vars <- c("year", "edclass", "sex", "age_gp", "TPop", v.totals) ## "age_gp" age standardized would be better
dMort <- dMort[, sel.vars]
# Calculate the rates for all relevant causes of death
for (i in 1:length(v.totals)){
      dMort[, v.rates[i]] <- (dMort[, v.totals[i]]/dMort$TPop)*100000
} 

dMort_as <- aggregate(.~ year + edclass + sex , data =  dMort, FUN=sum)
dMort$group <- paste0(dMort$sex, dMort$edclass)
dMort_as$group <- paste0(dMort_as$sex, dMort_as$edclass)
standard <- dMort[which(dMort$year == 2017), c("group", "TPop")]
standard$group <- as.factor(standard$group)
# age weighted but not standardized to a particular year 
for  (i in 1:length(v.totals)){
   for (j in unique(dMort$group)) {
      for (k in unique(dMort$year)) {
               dMort_as[which(dMort_as$group == j & dMort_as$year == k), 
                        v.rates[i]] <- 
         weighted.mean(x = dMort[which(dMort$group == j  & dMort$year == k), v.rates[i]],
                       w = standard$TPop[which(standard$group == j)]) 
      }
  
   }
} 

write.csv(dMort_as, "Mortality/3_out data/allethn_rates_as.csv")

label_sex <- c("2" = "Females", "1" = "Males")
label_ses <- c(College = "Bachelor +",
               LEHS = "High school and less", 
               SomeC = "Some College")
dMort_as$edclass <- factor(dMort_as$edclass, levels = c("LEHS", "SomeC", "College"))
for (i in v.rates) {
   ggplot(data=dMort_as, aes(x=year, y=dMort_as[,i] , group= edclass, color=edclass)) +
      geom_line()+
      theme_bw()+
      scale_size_area(max_size=10) +
      facet_grid(cols = vars(sex), labeller = labeller(sex = label_sex, edclass = label_ses)) +
      scale_color_manual(values=c("#145869",  "#C63A08", "red")) +
      xlab("Year") +
      ggtitle(i) +
      ylab("Age-standardized mortality rate") +
      theme(strip.background =element_rect(fill="#5B5D5E")) +
      theme(strip.text = element_text(colour = 'white'), 
            text=element_text(size=13)) 
   ggsave(paste0("Mortality/4_graphs/", i, "_ses.jpeg"), device = "jpeg", dpi = 600,
          width = 16, height = 8, units = "cm")    
}
### Race ethnicity

dMort <- read.csv("Mortality/3_out data/allethn_rates_0018_final.csv")

# variable type should be factor and not character
class(dMort$race)
dMort$race <- as.factor(dMort$race)
class(dMort$edclass)
dMort$edclass <- as.factor(dMort$edclass)

## Aggregate: summarize the data to collapse one demographic dimension 
## Specify all factor variables you want to keep (and omit the one 
## you want to collapse)
dMort <- aggregate(.~ year + race + sex + age_gp, data =  dMort, FUN=sum) # age_gp age standardized would be better
#dMort <- aggregate(.~ year + edclass + sex , data =  dMort, FUN=sum) # age_gp age standardized would be better

## We have to calculate the rates again because we cannot simply sum them up
## define vectors for the total death counts and rates of interest.
## IMPORTANT: the order of the totals and the rates must match!! 
v.totals <- c("Tmort", "UIJmort", "IJmort", "LVDCmort", "DMmort", "IHDmort",  
              "HSTRmort", "HYPHDmort", "RESTmort")

## these are the original rate variable names. You could also introduce the "mx_" 
## nomenclature here 
v.rates <- c("Trate", "mx_UIJrate", "mx_IJrate", "mx_LVDCrate", "mx_DMrate", 
             "mx_IHDrate", "mx_HSTRrate", "mx_HYPHDrate", "mx_RESTrate") 

#select mortality rates of interest (CD_NRrate, CD_LRrate, CD_HRrate, NCD_NRrate, NCD_LRrate, NCD_HRrate, 
# NCD_AAF1rate, Inj_NRrate, Inj_LRrate, Inj_AAF1rate, HS_NRrate, HS_LRrate, HS_AAF1rate) 
#US_2000$TPop <- NULL (to delete just one column)
sel.vars <- c("year", "race", "sex", "age_gp", "TPop", v.totals) ## "age_gp" age standardized would be better
dMort <- dMort[, sel.vars]
# Calculate the rates for all relevant causes of death
for (i in 1:length(v.totals)){
   dMort[, v.rates[i]] <- (dMort[, v.totals[i]]/dMort$TPop)*100000
} 

dMort_as <- aggregate(.~ year + race + sex , data =  dMort, FUN=sum)
dMort$group <- paste0(dMort$sex, dMort$race)
dMort_as$group <- paste0(dMort_as$sex, dMort_as$race)
standard <- dMort[which(dMort$year == 2017), c("group", "TPop")]
standard$group <- as.factor(standard$group)
# age weighted but not standardized to a particular year 
for  (i in 1:length(v.totals)){
   for (j in unique(dMort$group)) {
      for (k in unique(dMort$year)) {
         dMort_as[which(dMort_as$group == j & dMort_as$year == k), 
                  v.rates[i]] <- 
            weighted.mean(x = dMort[which(dMort$group == j  & dMort$year == k), v.rates[i]],
                          w = standard$TPop[which(standard$group == j)]) 
      }
      
   }
} 

write.csv(dMort_as, "Mortality/3_out data/allethn_rates_as_race.csv")

label_sex <- c("2" = "Females", "1" = "Males")
dMort_as$race <- factor(dMort_as$race, levels = c("White", "Black", "Hispanic", "Other"))
for (i in v.rates) {
   ggplot(data=dMort_as, aes(x=year, y=dMort_as[,i] , group= race, color=race)) +
      geom_line()+
      theme_bw()+
      scale_size_area(max_size=10) +
      facet_grid(cols = vars(sex), labeller = labeller(sex = label_sex, edclass = label_ses)) +
      scale_color_manual(values=c("#145869",  "#C63A08", "red", "black")) +
      xlab("Year") +
      ggtitle(i) +
      ylab("Age-standardized mortality rate") +
      theme(strip.background =element_rect(fill="#5B5D5E")) +
      theme(strip.text = element_text(colour = 'white'), 
            text=element_text(size=13)) 
   ggsave(paste0("Mortality/4_graphs/", i, "_race.jpeg"), device = "jpeg", dpi = 600,
          width = 16, height = 8, units = "cm")    
}


