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


## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")

## Load the functions that go with this code
source("SIMAH_code/life_expectancy/2b_decomp_functions.R")

#############################################################################################################
#before starting with the decomposition, we have to get the mortality data into the right format

#load aggregated mortality data:
dMort <- read.csv("SIMAH_workplace/mortality/3_out data/allethn_rates_0019_LE_decomp.csv")

# variable type should be factor and not character
class(dMort$race)
dMort$race <- as.factor(dMort$race)
class(dMort$edclass)
dMort$edclass <- as.factor(dMort$edclass)


## To aggregate some cause of death categories
dMort$IHDmort <- dMort$IHDmort + dMort$ISTRmort

#  Delete variables that are no longer needed 
dMort <- dMort %>% select (-c(ISTRmort,ISTRrate))

## Aggregate: summarize the data to collapse one demographic dimension 
## Specify all factor variables you want to keep (and omit the one 
## you want to collapse)
dMort <- aggregate(.~ year + edclass + sex + age_gp, data =  dMort, FUN=sum)
dMort_t <- aggregate(.~ year + age_gp, data =  dMort, FUN=sum)


## We have to calculate the rates again 
v.totals <- c("Tmort", "LVDCmort", "PANCmort", "DMmort", "IHDmort", "HSTRmort",
              "HYPHDmort", "AUDmort", "UIJmort", "MVACCmort", "IJmort", 
              "CANmort", "LRImort", "RESTmort")

#define variable names
v.rates <- c("Trate", "mx_LVDCrate", "mx_PANCrate", "mx_DMrate", "mx_IHDrate", 
             "mx_HSTRrate", "mx_HYPHDrate", "mx_AUDrate", 
             "mx_UIJrate", "mx_MVACCrate", "mx_IJrate", 
             "mx_CANrate", "mx_LRIrate", "mx_RESTrate") 


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
write.csv(dMort_pop, "SIMAH_workplace/life_expectancy/2_out_data/proportion_SES.csv")

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
ggsave("SIMAH_workplace/life_expectancy/3_graphs/SES_proportion_over_time.jpg", dpi=600, width=18, height=13, units="cm")


# Calculate the rates for all relevant causes of death
for (i in 1:length(v.totals)){
  dMort[, v.rates[i]] <- (dMort[, v.totals[i]]/dMort$TPop)
} 


# Get mortality data aggregated over age groups
dMort_agg <- aggregate(.~ year + edclass + sex, data =  dMort, FUN=sum)
for (i in 1:length(v.totals)){
  dMort_agg[, v.rates[i]] <- (dMort_agg[, v.totals[i]]/dMort_agg$TPop)
}
write.csv(dMort_agg, "SIMAH_workplace/life_expectancy/2_out_data/dMort_0018_agg.csv")

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

write.csv(dResults_contrib,paste0("SIMAH_workplace/life_expectancy/2_out_data/dResults_contrib_", v.year1[1], "_", max(v.year2), "CPS_v4.csv") )
write.csv(dMort, "SIMAH_workplace/life_expectancy/2_out_data/dMort_0018.csv")

 