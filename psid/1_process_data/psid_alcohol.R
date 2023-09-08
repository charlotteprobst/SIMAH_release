# SIMAH project 2021 - processing alcohol use in PSID data 
library(foreign)
library(dplyr)
library(tidyr)
setwd("~/Google Drive/SIMAH Sheffield")


PSID <- read.csv("SIMAH_workplace/education_transitions/alldata_2019.csv")


# read in the alcohol variables
data <- read.dbf("SIMAH_workplace/PSiD/J300708/J300708.dbf")


recode_PSID_vars <- function(data, varlist, variable, years){
  newdata <- data %>% 
    mutate(origINTNO = ER30001,
           ID = ER30002,
           uniqueID = (origINTNO*1000) + ID,
           sex = recode(as.factor(ER32000), "1"="male", "2"="female")) %>% 
    dplyr::select(uniqueID, sex, c(varlist))
  names(newdata)[3:length(newdata)] <- years
  if(min(years)==1999){
  newdata <- newdata %>% pivot_longer(cols='1999':'2019', names_to="year", values_to=variable) %>% 
    mutate(year=as.numeric(as.character(year)))
  }else{
    newdata <- newdata %>% pivot_longer(cols='2005':'2019', names_to="year", values_to=variable) %>% 
      mutate(year=as.numeric(as.character(year)))
  }
  return(newdata)
  
}

years <- c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)

# variables for "ever drink" for household head
varlist <- c("ER15550","ER19715","ER23130","ER27105","ER38316","ER44289","ER49627","ER55375","ER62497","ER68562", "ER74570")

everdrinkhead <- recode_PSID_vars(data, varlist, "everdrinkhead", years)

# variables for "ever drink" for spouse
varlist <- c("ER15658","ER19823","ER23257","ER27228","ER39413","ER45386","ER50745","ER56491","ER63613","ER69689", "ER75697")

everdrinkspouse <- recode_PSID_vars(data, varlist, "everdrinkspouse",years)

# join up to the individual level data
PSID <- left_join(PSID, everdrinkhead)
PSID <- left_join(PSID, everdrinkspouse)

PSID <- PSID %>% 
  mutate(drinkingstatus = ifelse(relationshiptohead=="head", everdrinkhead,
                            ifelse(relationshiptohead=="wife/partner",everdrinkspouse, NA)),
         drinkingstatus = ifelse(drinkingstatus==1, 1,
                            ifelse(drinkingstatus==5, 0, NA))) %>% 
  dplyr::select(-c(everdrinkhead,everdrinkspouse))

# how much missing data per year for drinking status 
summary <- PSID %>% group_by(year, sex,drinkingstatus) %>% tally() %>% ungroup() %>% 
  group_by(year, sex) %>% mutate(percent=n/sum(n)) %>% filter(is.na(drinkingstatus))

# now do drinking quantity 
# variables for "drinking quantity" for household head
varlist <- c("ER15551","ER19716","ER23131","ER27107","ER38318","ER44291","ER49629","ER55377","ER62499","ER68564", "ER74572")

quanthead <- recode_PSID_vars(data, varlist, "quanthead",years)

varlist <- c("ER15659","ER19824","ER23258","ER27230","ER39415","ER45388","ER50747","ER56493","ER63615","ER69691", "ER75699")

quantspouse <- recode_PSID_vars(data, varlist, "quantspouse",years)

PSID <- left_join(PSID, quanthead)
PSID <- left_join(PSID, quantspouse)

PSID <- PSID %>% 
  mutate(quantity = ifelse(relationshiptohead=="head", quanthead,
                                 ifelse(relationshiptohead=="wife/partner",quantspouse, NA)),
         quantity = ifelse(year<=2003 & quantity ==0, 0,
                           ifelse(year<=2003 & quantity ==1, 0.5,
                                  ifelse(year<=2003 & quantity==2, 1,
                                         ifelse(year<=2003 & quantity==3, 2, 
                                                ifelse(year<=2003 & quantity==4, runif(., min=5, max=20),
                                                       ifelse(year>=2005 & quantity>=98, NA, quantity))))))) %>% 
  dplyr::select(-c(quanthead,quantspouse))


# now do drinking frequency 
# variables for "drinking frequency" for household head
varlist <- c("ER27106","ER38317","ER44290","ER49628","ER55376","ER62498","ER68563","ER74571")
years <- c(2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)

freqhead <- recode_PSID_vars(data, varlist, "freqhead",years)

varlist <- c("ER27229","ER39414","ER45387","ER50746","ER56492","ER63614","ER69690","ER75698")

freqspouse <- recode_PSID_vars(data, varlist, "freqspouse",years)

PSID <- left_join(PSID, freqhead)
PSID <- left_join(PSID, freqspouse)

PSID <- PSID %>% 
  mutate(frequency = ifelse(relationshiptohead=="head", freqhead,
                           ifelse(relationshiptohead=="wife/partner",freqspouse, NA)),
         frequency = ifelse(frequency==1, 1, 
                            ifelse(frequency==2, 1.5, 
                                   ifelse(frequency==3, 3.5,
                                          ifelse(frequency==4, 5,
                                                 ifelse(frequency==5, 12,
                                                        ifelse(frequency==6, 30,
                                                               ifelse(frequency==8, NA,
                                                                      ifelse(frequency==0, 0, frequency))))))))) %>% 
  dplyr::select(-c(freqhead,freqspouse)) %>% 
  mutate(gpd = ifelse(year<=2003, quantity*14,(quantity*frequency*14)/30))

# reweight the data 
library(splitstackshape)

PSID <- PSID %>% drop_na(weight)

reweighted <- expandRows(PSID, "weight")

# compare to BRFSS data 
brfssorig <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_states_upshifted.RDS") %>% 
  filter(age_var<=80) %>% filter(State=="USA")
  
summarybrfss <- brfssorig %>%  filter(gramsperday!=0) %>% 
  group_by(YEAR, sex_recode) %>% summarise(meangpd = mean(gramsperday)) %>% 
  rename(year=YEAR, sex=sex_recode) %>% 
  mutate(sex = ifelse(sex=="Male","male","female"),
         type="BRFSS")

# calculate mean grams per day in each year 
summary <- reweighted %>% group_by(year, sex) %>% filter(gpd!=0) %>% 
  summarise(meangpd = mean(gpd),
            type="PSID")

summary <- rbind(summary, summarybrfss)

ggplot(data=summary, aes(x=year, y=meangpd, colour=type)) + geom_line() + ylim(0,NA) + theme_bw() +
  facet_grid(rows=vars(sex)) + ylab("mean gpd (in drinkers)") +
  theme(legend.title=element_blank(),
        strip.background = element_rect(fill="white"))

ggsave("SIMAH_workplace/PSID/compare_gpd_brfss.png",dpi=300, width=33, height=19, units="cm")

summary <- reweighted %>% group_by(year, sex) %>% 
  summarise(prevalence = mean(drinkingstatus, na.rm=T)) %>% 
  mutate(type="PSID")

summarybrfss <- brfssorig %>% 
  group_by(YEAR, sex_recode) %>% summarise(prevalence = mean(drinkingstatus)) %>% 
  rename(year=YEAR, sex=sex_recode) %>% 
  mutate(sex = ifelse(sex=="Male","male","female"),
         type="BRFSS")

summary <- rbind(summary, summarybrfss) %>% mutate(prevalence=round(prevalence,digits=2))
scaleFUN <- function(x) sprintf("%.2f", x)

ggplot(data=summary, aes(x=year, y=prevalence, colour=type)) + geom_line() + ylim(0,NA) + theme_bw() +
  facet_grid(rows=vars(sex)) + ylab("mean prevalence") +
  theme(legend.title=element_blank(),
        strip.background = element_rect(fill="white")) + 
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA))

ggsave("SIMAH_workplace/PSID/compare_prevalence_brfss.png",dpi=300, width=33, height=19, units="cm")

ggplot(data=summary, aes(x=year, y=prevalence, colour=sex)) + geom_line() + ylim(0,NA) + theme_bw()

# by alcohol categories 
reweighted <- reweighted %>% 
  mutate(AlcCAT = ifelse(gpd==0, "abstainer",
                       ifelse(sex=="male" & gpd>0 & 
                                gpd<=40, "Low risk",
                              ifelse(sex=="female" & gpd>0 &
                                       gpd<=20, "Low risk",
                                     ifelse(sex=="male" & gpd>40 &
                                              gpd<=60, "Medium risk",
                                            ifelse(sex=="female" & gpd>20 & 
                                                     gpd<=40, "Medium risk",
                                                   ifelse(sex=="male" & gpd>60,
                                                          "High risk",
                                                          ifelse(sex=="female" & gpd>40,
                                                                 "High risk", NA))))))))

summary <- reweighted %>% drop_na(AlcCAT) %>% group_by(year, sex, AlcCAT) %>% tally() %>% ungroup() %>% 
  group_by(year, sex) %>% mutate(percent=n/sum(n), type="PSID")

summarybrfss <- brfssorig %>% 
  mutate(AlcCAT = ifelse(gramsperday==0, "abstainer",
                         ifelse(sex_recode=="Male" & gramsperday>0 & 
                                  gramsperday<=40, "Low risk",
                                ifelse(sex_recode=="Female" & gramsperday>0 &
                                         gramsperday<=20, "Low risk",
                                       ifelse(sex_recode=="Male" & gramsperday>40 &
                                                gramsperday<=60, "Medium risk",
                                              ifelse(sex_recode=="Female" & gramsperday>20 & 
                                                       gramsperday<=40, "Medium risk",
                                                     ifelse(sex_recode=="Male" & gramsperday>60,
                                                            "High risk",
                                                            ifelse(sex_recode=="Female" & gramsperday>40,
                                                                   "High risk", NA)))))))) %>% 
  
  group_by(YEAR, sex_recode, AlcCAT) %>% tally() %>% ungroup() %>% group_by(YEAR, sex_recode) %>% 
  mutate(percent=n/sum(n)) %>% 
  rename(year=YEAR, sex=sex_recode) %>% 
  mutate(sex = ifelse(sex=="Male","male","female"),
         type="BRFSS")

summary <- rbind(summary, summarybrfss) %>% 
  mutate(AlcCAT = factor(AlcCAT, levels=c("abstainer","Low risk","Medium risk","High risk")))

ggplot(data=summary, aes(x=year, y=percent, colour=type)) + geom_line() + ylim(0,NA) + theme_bw() +
  facet_grid(cols=vars(AlcCAT), rows=vars(sex)) + 
  scale_y_continuous(labels=scaleFUN)


ggsave("SIMAH_workplace/PSID/compare_categories_brfss.png",dpi=300, width=33, height=19, units="cm")

           


                                                              