# SIMAH June 2021
# Calculating population totals from ACS for US and States 
# Split by sex, age group, racee/ethnicity and education 

#  SIMAH project 2022 
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
suppressPackageStartupMessages(library("dplyr"))
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)
library(R.utils)
library(ipumsr)
options(scipen=999)

####EDIT ONLY BELOW HERE ### 
###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(paste(WorkingDirectory))

# first read in the data 2000 to 2020
#
# gunzip("SIMAH_workplace/ACS/usa_00033.dat.gz", remove=FALSE)

# now read in the data for 2021 
ddi <- read_ipums_ddi("SIMAH_workplace/ACS/usa_00033.xml")

data <- read_ipums_micro(ddi)
library(labelled)
data <- remove_attributes(data, "var_desc")
data <- remove_attributes(data, "label")
data <- remove_attributes(data, "labels")
data <- remove_attributes(data, "lbl")
data <- remove_attributes(data, "int+lbl")
data <- zap_ipums_attributes(data)


summaryUSA <- data %>% 
  filter(AGE>=18) %>% 
  mutate(age_gp = cut(AGE, breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,79,10000),
                      labels=c("18","25","30","35","40","45","50","55","60","65",
                               "70","75","80")),
                      race = ifelse(RACE==1, "White",
                                    ifelse(RACE==2,"Black",
                                           "Other")),
                      race = ifelse(HISPAN==0, race,
                                    "Hispanic"),
                      race = as.factor(race),
                      edclass = ifelse(EDUC<=6, "LEHS",
                                    ifelse(EDUC>6 & EDUC<=9, "SomeC","College")),
                      sex = SEX) %>% 
  group_by(sex, race, age_gp, edclass) %>%
  summarise(TPop=sum(PERWT)) %>% 
  mutate(year = 2021, state="USA")

# join to existing ACS 
ACS_20002020 <- read.csv("SIMAH_workplace/ACS/ACS_popcounts_2000_2020.csv") %>% filter(state=="USA")

ACS_20002020 <- rbind(ACS_20002020, summaryUSA) %>% drop_na()

write.csv(ACS_20002020, "SIMAH_workplace/ACS/ACS_popcounts_2000_2021.csv", row.names=F)


# 
ggplot(data=subset(ACS_20002020,sex==2), aes(x=year, y=TPop, colour=age_gp)) +
  geom_line() + facet_grid(cols=vars(race),rows=vars(edclass)) + theme_bw() + 
  ggtitle("Women") + xlim(2010, 2021)




# 
# summaryStates <- data %>% group_by(YEAR, STATE, SEX, RACE, AGECAT, EDUC) %>% 
#   summarise(n=sum(FINALWEIGHT)) %>% drop_na()
# 
# summary <- rbind(summaryUSA, summaryStates)
# 
# write.csv(summary, "ACS_popcounts.csv", row.names=FALSE)

# unweighted ACS population counts 

# read in the data 
ddi <- read_ipums_ddi("1_adjust_ACS/usa_00030.xml")

library(R.utils)
# gunzip("1_adjust_ACS/usa_00030.dat.gz")
data <- read_ipums_micro(ddi)
library(labelled)
data <- remove_attributes(data, "var_desc")
data <- remove_attributes(data, "label")
data <- remove_attributes(data, "labels")


summary(data$AGE)

summary <- data %>% group_by(YEAR) %>% 
  summarise(min = min(AGE), max = max(AGE))
  

data <- data %>% 
  mutate(SEX = ifelse(SEX==1, "M","F"),
         RACE = ifelse(RACE==1, "White",
                       ifelse(RACE==2,"Black",
                              "Other")),
         RACE = ifelse(HISPAN==0, RACE,
                       "Hispanic"),
         EDUC = ifelse(EDUC<=6, "LEHS",
                       ifelse(EDUC>6 & EDUC<=9, "SomeC","College")),
         AGECAT=cut(AGE,
                    breaks=c(0,17,24,29,34,39,44,49,54,59,64,69,74,79,1000),
                    labels=c("0-17","18-24","25-29","30-34","35-39","40-44",
                    "45-49","50-54","55-59","60-64","65-69","70-74","75-79",
                    "80+")),
                    STATE = recode(STATEFIP, "06"="California", "08"="Colorado", "12"="Florida", "18"="Indiana",
                                   "21"="Kentucky","22"="Louisiana","25"="Massachusetts","26"="Michigan",
                                   "27"="Minnesota","29"="Missouri", "36"="New York", "41"="Oregon",
                                   "42"="Pennsylvania", "47"="Tennessee", "48"="Texas",
                                   .default="NA"))

summaryUSA <- data %>% group_by(YEAR, SEX, RACE, AGE, EDUC) %>% 
  summarise(n=sum(PERWT)) %>% mutate(STATE="USA")

summaryStates <- data %>% group_by(YEAR, STATE, SEX, RACE, AGE, EDUC) %>% 
  summarise(n=sum(PERWT)) %>% drop_na()

summary <- rbind(summaryUSA, summaryStates)
write.csv(summary, "2_calculate_population_totals/ACS_popcounts_unweighted_indage.csv", row.names=FALSE)

# now calculate summary for age categories
summaryUSA <- data %>% group_by(YEAR, SEX, RACE, AGE, EDUC) %>% 
  summarise(n=sum(PERWT)) %>% mutate(STATE="USA")

ggplot(data=subset(summaryUSA,SEX=="F"), aes(x=YEAR, y=n, colour=AGECAT)) + geom_line() +
  facet_grid(cols=vars(EDUC), rows=vars(RACE),scales="free")

summaryStates <- data %>% group_by(YEAR, STATE, SEX, RACE, AGECAT, EDUC) %>% 
  summarise(n=sum(PERWT)) %>% drop_na() %>% filter(STATE!="NA")

summary <- rbind(summaryUSA, summaryStates) %>% 
  rename(year=YEAR, sex=SEX, age_gp=AGECAT, edclass=EDUC, TPop=n, state=STATE,
         race=RACE) %>% 
  filter(age_gp!="0-17") %>% 
  mutate(age_gp=substr(age_gp,1,2),
         sex=ifelse(sex=="F",2,1)) %>% 
  dplyr::select(state,year,sex,age_gp,race,edclass,TPop)
write.csv(summary, "2_calculate_population_totals/ACS_popcounts_2000_2020.csv", row.names=FALSE)

summaryUSA <- data %>% 
  mutate(AGE = ifelse(AGE>=90, 90, AGE)) %>%
  mutate_at(vars(YEAR, SEX, RACE, AGE, EDUC), as.factor) %>% 
  group_by(YEAR, SEX, RACE, AGE, EDUC, .drop=FALSE) %>% 
  summarise(n=sum(PERWT)) %>% mutate(STATE="USA") %>%
  # complete(YEAR, SEX, RACE, AGE, EDUC, fill = list(n = 0)) %>% 
  mutate(SEX = ifelse(SEX=="F",2,1),
         AGE = as.numeric(as.character(AGE))) %>% 
  rename(year=YEAR, sex=SEX, race=RACE, age=AGE, edclass=EDUC,
         state=STATE, TPop=n) %>% 
  dplyr::select(state,year,sex,age,race, edclass,TPop) %>% 
  mutate(year=as.numeric(as.character(year)))

training <- summaryUSA %>% filter(year>=2006)

grouped_lm <- function(subdata){
  model <- lm(TPop ~ year, data=subdata)
  topredict <- data.frame(year=2000:2005)
  topredict$TPop <- predict(model, topredict)
  finaldata <- data.frame(state="USA", year=topredict$year,
                          sex=subdata$sex[1:6], age=subdata$age[1:6],
                          race=subdata$race[1:6], edclass=subdata$edclass[1:6],
                          TPop = topredict$TPop)
  finaldata$TPop <- round(finaldata$TPop, digits=0)
  return(finaldata)
}

imputed <- training %>% 
  group_by(sex, age, race, edclass) %>% 
  do(grouped_lm(.))

joined <- rbind(training, imputed)

sum <- joined %>% 
  group_by(year, age) %>% 
  summarise(sum = round(sum(TPop),0)) %>% 
  pivot_wider(names_from=year, values_from=sum)

library(ggplot2)
ggplot(data=subset(joined, age=="90" & edclass=="LEHS"), aes(x=year, y=TPop)) + 
  geom_line() +
  facet_grid(cols=vars(sex), rows=vars(race))

# work out difference between 2005 and 2006 
difference <- summaryUSA %>% filter(year>=2005 & year<=2006) %>% 
  pivot_wider(names_from=year, values_from=TPop) %>% 
  mutate(`2005`=ifelse(`2005`==0, 1, `2005`),
         pct_change = `2006`/`2005`) %>% 
  dplyr::select(state, sex, age, race, edclass, pct_change)

summaryUSA <- left_join(summaryUSA, difference) %>% 
  mutate(TPop_new = ifelse(year<=2005, TPop*pct_change, TPop))



summaryUSA %>% 
  group_by(year) %>% 
  summarise(sum = sum(TPop))



summaryUSA <- summaryUSA %>% dplyr::select(-pct_change, TPop) %>% 
  mutate(TPop=round(TPop_new, digits=0)) %>% dplyr::select(-TPop_new)

write.csv(joined, "2_calculate_population_totals/ACS_popcounts_2000_2020_indage_adjusted_90.csv", row.names=FALSE)

