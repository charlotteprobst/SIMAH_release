# Calculate LE decomposition by SES with aggregated US mortality data (2000-2008)
# Mortality data NCHS
# Population data: March CPS
# Project: SIMAH


# libraries required:
library("tidyverse")
library("DemoDecomp")
library("dplyr")
library("reshape")
library("data.table")

# remove e notation for small numbers
options(scipen=999)


## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")
# CB working directory
setwd("~/Google Drive/SIMAH Sheffield/")
#setwd("~/Documents/Promotion/Mortality US")

## Load the functions that go with this code
source("SIMAH_code/life_expectancy/2b_decomp_functions.R")

#############################################################################################################
#before starting with the decomposition, we have to get the mortality data into the right format

# switch between 0 and 1 for ACS experimental weights (1) versus our modelled ACS 2020 population (0)
ACS_type <- 1

#load aggregated mortality data:
dMort <- read.csv("SIMAH_workplace/mortality/3_out data/allethn_sumCOD_0020_LE_decomp.csv")

if(ACS_type==0){
dPop <- read.csv("SIMAH_workplace/demography/ACS_popcounts_2000_2020.csv")
<<<<<<< Updated upstream
# read in alternative population counts for 2020 from experimental ACS 
dPop_weights <- readRDS("SIMAH_workplace/ACS/rep_weights_2020.RDS")
=======
}else if(ACS_type==1){
# version with modelled pop for 2020
dPop <- read.csv("SIMAH_workplace/demography/ACS_popcounts_2000_2020_predicted2020.csv")
}


>>>>>>> Stashed changes
dPop <- filter(dPop, state == "USA")
dPop <- select (dPop,-c(state))

dMort <- inner_join(dMort, dPop)

# variable type should be factor and not character
dMort$race <- as.factor(dMort$race)
dMort$edclass <- as.factor(dMort$edclass)

## To aggregate some cause of death categories
dMort$RESTmort <- dMort$RESTmort + dMort$CANmort
dMort$RESTmort <- dMort$RESTmort + dMort$PANCmort
dMort$RESTmort <- dMort$RESTmort + dMort$HSTRmort

dMort$IHDmort <- dMort$IHDmort + dMort$ISTRmort

#  Delete variables that are no longer needed 
dMort <- dMort %>% select (-c(CANmort, PANCmort, ISTRmort, HSTRmort))

## Aggregate: summarize the data to collapse one demographic dimension 
## Specify all factor variables you want to keep (and omit the one 
## you want to collapse)
dMort <- aggregate(.~ year + edclass + sex + age_gp, data =  dMort, FUN=sum)
dMort_t <- aggregate(.~ year + age_gp, data =  dMort, FUN=sum)


## We have to calculate the rates again 
v.totals <- c("Tmort", "COVmort", "LVDCmort", "DMmort", "IHDmort",
              "HYPHDmort", "AUDmort", "UIJmort", "MVACCmort", "IJmort",  "RESTmort")

v.totals <- names(dMort)[6:15]

## these are the original rate variable names. You could also introduce the "mx_" 
## nomenclature here 
v.rates <- c("Trate", "mx_COVrate", "mx_LVDCrate", "mx_DMrate", "mx_IHDrate", 
             "mx_HYPHDrate", "mx_AUDrate", "mx_UIJrate", "mx_MVACCrate", 
             "mx_IJrate",  "mx_RESTrate") 

v.rates <- c("Trate","mx_LVDCrate", "mx_DMrate", "mx_IHDrate", 
             "mx_HYPHDrate", "mx_AUDrate", "mx_UIJrate", "mx_MVACCrate", 
             "mx_IJrate",  "mx_RESTrate") 


#select mortality rates of interest
sel.vars <- c("year", "edclass", "sex", "age_gp", v.rates) 

# Calculate the proportion in each SES group by sex and year. 
dMort_pop <- aggregate(.~ year + sex + edclass, data =  dMort, FUN=sum)
dMort_pop <- dMort_pop  %>% select(year, sex, edclass, TPop) %>% 
  group_by(year, sex, .drop=FALSE) %>% mutate(Proportion=(TPop/sum(TPop)) * 100) %>%
  ungroup() %>% arrange(., sex, year, edclass)
color.vec <- c("#69AA9E", "#447a9e",  "#d72c40") # high  middle low
setnames(dMort_pop, old = c('edclass', 'sex', 'year'), new = c('SES','Sex', 'Year'))
dMort_pop$SES <- recode(dMort_pop$SES, "College" = "High", "SomeC" = "Middle",  "LEHS" = "Low")
dMort_pop$Sex <- as.factor(dMort_pop$Sex)
dMort_pop$Sex <- recode(dMort_pop$Sex, "1" = "Men", "2" = "Women")
#write.csv(dMort_pop, "SIMAH_workplace/life_expectancy/2_out_data/proportion_SES.csv")

ggplot(dMort_pop, aes(x = Year, y = Proportion,  group = SES)) +
  facet_grid(cols = vars(Sex)) +
  geom_line(aes(colour = SES), size = .9, alpha = .7) +    
  xlab("Year") +
  theme_light()+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), text=element_text(size = 16)) +
  theme(legend.position = "right") +
  scale_color_manual(name = "SES", breaks = c("High", "Middle", "Low"), values = color.vec, 
                     labels = c("High", "Middle", "Low")) +   
  geom_point(size = 1, aes(color = SES)) 
#ggsave("SIMAH_workplace/life_expectancy/3_graphs/SES_proportion_over_time.jpg", dpi=600, width=18, height=13, units="cm")

# creating 80 different versions of the dMort file for 2020

# first aggregate by only year sex age_gp and edclass 
<<<<<<< Updated upstream
for(i in 1:length(dPop_weights)){
  dPop_weights[[i]] <- dPop_weights[[i]] %>% 
    group_by(year, sex, age_gp, edclass) %>% summarise(TPop2=sum(TPop)) %>% 
=======

if(ACS_type==0){
dMort2020 <- dMort %>% filter(year==2020) %>% dplyr::select(-c(race,TPop))
mortlist <- list()

# join with TPop for different weights and calculate mortality from different causes
for(i in 1:length(dPop_weights_raw)){
  dPop_weights[[i]] <- dPop_weights_raw[[i]] %>% 
    group_by(year, sex, age_gp, edclass) %>% summarise(TPop=sum(TPop)) %>% 
>>>>>>> Stashed changes
    mutate(age_gp=as.integer(age_gp))
}

dMort2020 <- dMort %>% filter(year==2020)

mortlist <- list()
for(i in 1:length(dPop_weights)){
  mortlist[[i]] <- left_join(dMort2020, dPop_weights[[i]]) %>% 
    dplyr::select(-TPop) %>% dplyr::rename(TPop=TPop2)
}
}


# Calculate the rates for all relevant causes of death
for (i in 1:length(v.totals)){
  dMort[, v.rates[i]] <- (dMort[, v.totals[i]]/dMort$TPop)
} 

# Calculate the rates for all relevant causes of death - 
# for different 2020 versions 
for(i in 1:length(mortlist)){
  mortlist[[i]] <- mortlist[[i]] %>% 
    pivot_longer(cols=c(Tmort:RESTmort)) %>% 
    mutate(rate = value/TPop,
           name = gsub("mort","rate",name),
           name = ifelse(name=="Trate","Trate",
                         paste0("mx_",name))) %>% 
    dplyr::select(-value) %>% 
    pivot_wider(names_from=name, values_from=rate)
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
write.csv(dResults_contrib,paste0("SIMAH_workplace/life_expectancy/2_out_data/dResults_contrib_", v.year1[1], "_", max(v.year2), "ACS.csv") )
write.csv(dMort, "SIMAH_workplace/life_expectancy/2_out_data/dMort_0020.csv")
}else if(ACS_type==1){
  write.csv(dResults_contrib,paste0("SIMAH_workplace/life_expectancy/2_out_data/dResults_ACSmodel_contrib_", v.year1[1], "_", max(v.year2), "ACS.csv") )
  write.csv(dMort, "SIMAH_workplace/life_expectancy/2_out_data/dMort_0020_ACSmodel.csv")
}

<<<<<<< Updated upstream
# new version for looping over different population versions for 2020
=======
if(ACS_type==0){
# now loop over and get the results for 2020 weights
>>>>>>> Stashed changes
for(i in 1:length(mortlist)){
mortlist[[i]]$group <- apply(mortlist[[i]][ , c("sex", "edclass") ] , 1 , paste , collapse = "_" )
mortlist[[i]]$weight <- i
}

test <- do.call(rbind,mortlist)

dResults_contrib_list <- list()

# only get the results for 2019 - 2020 (results above will be the same for 2018-2019)
  # for(k in 1:length(mortlist)){
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
  dResults_contrib_list[[paste(i)]] <- separate(dResults_contrib_list[[paste(i)]], col = group, into = c("sex","edclass"), sep = "_")
  dResults_contrib_list[[paste(i)]]$edclass <- factor(dResults_contrib_list[[paste(i)]]$edclass, 
                                     levels = c( "LEHS", "SomeC", "College"))
  dResults_contrib_list[[paste(i)]]$sex <- as.factor(dResults_contrib_list[[paste(i)]]$sex)
  dResults_contrib_list[[paste(i)]] <- dResults_contrib_list[order(dResults_contrib_list[[paste(i)]]$start_year, dResults_contrib_list[[paste(i)]]$sex, dResults_contrib_list[[paste(i)]]$edclass), ]
}

for(i in 1:length(dResults_contrib_list)){
  dResults_contrib_list[[paste(i)]]$weight <- i
  dResults_contrib_list[[paste(i)]]$group <- NULL
}

for(i in 2:length(dResults_contrib_list)){
  dResults_contrib_list[[paste(i)]]$sex <- dResults_contrib_list[[1]]$sex
  dResults_contrib_list[[paste(i)]]$edclass <- dResults_contrib_list[[1]]$edclass
}
}

dResults_weights <- do.call(rbind,dResults_contrib_list)

write.csv(dResults_weights,paste0("SIMAH_workplace/life_expectancy/2_out_data/dResults_contrib_", v.year1[1], "_", max(v.year2), "ACS_2020weights.csv") )
write.csv(mortlist, "SIMAH_workplace/life_expectancy/2_out_data/dMort_0020_2020weights.csv")

<<<<<<< Updated upstream
dResults_weights <- read.csv(paste0("SIMAH_workplace/life_expectancy/2_out_data/dResults_contrib_", v.year1[1], "_", max(v.year2), "ACS_2020weights.csv") )
=======
if(ACS_type==0){
write.csv(dResults_contrib,paste0("SIMAH_workplace/life_expectancy/2_out_data/dResults_contrib_", v.year1[1], "_", max(v.year2), "race_ACS.csv") )
write.csv(dMort_d, "SIMAH_workplace/life_expectancy/2_out_data/dMort_race_0020.csv")
}else if(ACS_type==1){
write.csv(dResults_contrib,paste0("SIMAH_workplace/life_expectancy/2_out_data/dResults_ACSmodel_contrib_", v.year1[1], "_", max(v.year2), "race_ACS.csv") )
write.csv(dMort_d, "SIMAH_workplace/life_expectancy/2_out_data/dMort_race_0020_ACSmodel.csv")  
}

if(ACS_type==0){
>>>>>>> Stashed changes

checking <- dResults_weights %>% group_by(sex,edclass) %>% 
  summarise(meanLE1 = round(mean(LE1),digits=2),
            minLE1 = round(min(LE1),digits=2),
            maxLE1 = round(max(LE1),digits=2),
            meanLE2 = round(mean(LE2),digits=2),
            minLE2 = round(min(LE2),digits=2),
            maxLE2 = round(max(LE2),digits=2))

<<<<<<< Updated upstream
=======
dResults_weights <- do.call(rbind, dResults_contrib_list)
write.csv(dResults_weights,paste0("SIMAH_workplace/life_expectancy/2_out_data/dResults_contrib_", v.year1[1], "_", max(v.year2), "race_ACSweights.csv") )
}
>>>>>>> Stashed changes
