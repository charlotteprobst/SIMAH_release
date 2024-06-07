#####script for processing the ACS data to estimate inward and outward migration and new 18 year olds in each year
###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/SIMAH/Microsimulation"
setwd(paste(WorkingDirectory))
library(ipumsr)
library(R.utils)
library(dplyr)
library(labelled)

# gunzip("usa_00039.dat.gz", remove=FALSE)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
# read in the data 
ddi <- read_ipums_ddi("SIMAH_workplace/ACS/usa_00039.xml")
data <- read_ipums_micro(ddi)
data <- remove_labels(data)
data <- remove_attributes(data, "var_desc")

births <- data %>% 
  filter(AGE==18) %>% 
  mutate(SEX=recode(SEX,"1"="m","2"="f"),
         RACE = ifelse(RACE==1, "WHI",
                       ifelse(RACE==2,"BLA",
                              "OTH")),
         RACE = ifelse(HISPAN==0, RACE,
                       "SPA")) %>% 
  group_by(YEAR,AGE,SEX,RACE,EDUC) %>% 
  summarise(EDUC="LEHS",
    MigrationInN=sum(PERWT)) %>% 
  rename(Year=YEAR, agecat=AGE, microsim.init.sex=SEX,microsim.init.race=RACE,
         education=EDUC) %>% 
  mutate(agecat=as.character(agecat))

migrants <- data %>% 
  filter(MIGRATE1==4) %>% 
  filter(AGE!=18) %>% 
  mutate(agecat = cut(AGE,
                      breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,100),
                      labels=c("19-24","25-29","30-34","35-39","40-44",
                               "45-49","50-54","55-59","60-64","65-69",
                               "70-74","75-79")),
    SEX=recode(SEX,"1"="m","2"="f"),
         RACE = ifelse(RACE==1, "WHI",
                       ifelse(RACE==2,"BLA",
                              "OTH")),
         RACE = ifelse(HISPAN==0, RACE,
                       "SPA"),
    EDUC = ifelse(EDUC<=6, "LEHS",
                  ifelse(EDUC==7, "SomeC1",
                         ifelse(EDUC==8, "SomeC2",
                                ifelse(EDUC==9, "SomeC3",
                                       ifelse(EDUC>=10, "College",NA)))))) %>% 
  group_by(YEAR,agecat,SEX,RACE,EDUC) %>% 
  summarise(MigrationInN=sum(PERWT)) %>% 
  rename(Year=YEAR,microsim.init.sex=SEX,microsim.init.race=RACE,education=EDUC)

migrants <- rbind(births,migrants)

# For USA -> no information about outward migration so need to assume 0 as a prior
migrants$MigrationOutN <- 0

write.csv(migrants, "SIMAH_workplace/microsim/census_data/migration_in_USA.csv", row.names=F)
