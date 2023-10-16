#####script for processing the ACS data to estimate inward and outward migration and new 18 year olds in each year
###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/SIMAH/Microsimulation"
setwd(paste(WorkingDirectory))
library(ipumsr)
library(R.utils)
library(dplyr)
library(labelled)
library(ggplot2)
# gunzip("SIMAH_workplace/ACS/usa_00040.dat.gz", remove=FALSE)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
# read in the data 
ddi <- read_ipums_ddi("SIMAH_workplace/ACS/usa_00040.xml")
data <- read_ipums_micro(ddi)
data <- remove_labels(data)
data <- remove_attributes(data, "var_desc")

births <- data %>% 
  filter(AGE==18) %>% 
  filter(SAMPLE!=200004) %>% 
  mutate(SEX=recode(SEX,"1"="m","2"="f"),
         RACE = ifelse(RACE==1, "WHI",
                       ifelse(RACE==2,"BLA",
                              "OTH")),
         RACE = ifelse(HISPAN==0, RACE,
                       "SPA")) %>% 
  group_by(YEAR,AGE,SEX,RACE) %>% 
  summarise(
    MigrationInN=sum(PERWT)) %>% 
  rename(Year=YEAR, agecat=AGE, microsim.init.sex=SEX,microsim.init.race=RACE) %>% 
  mutate(agecat=as.character(agecat)) 

ggplot(data=births, aes(x=Year, y=MigrationInN)) + 
  geom_line() + 
  facet_grid(cols=vars(microsim.init.sex), rows=vars(microsim.init.race))

# now check and impute the values for 2001 - 2005 (dates where group quarters not included)
births$MigrationInN_impute <- ifelse(births$Year>=2001 & births$Year<=2005, NA, births$MigrationInN)

births <- births %>% 
  group_by(agecat, microsim.init.sex, microsim.init.race) %>% 
  mutate(MigrationInN_impute = na.approx(MigrationInN_impute))

ggplot(data=births, aes(x=Year, y=MigrationInN)) + 
  geom_line() + 
  geom_line(aes(x=Year, y=MigrationInN_impute), colour="red") + 
  facet_grid(cols=vars(microsim.init.sex), rows=vars(microsim.init.race))

ggsave("SIMAH_workplace/ACS/compare_imputation_18yearolds.png",
       dpi=300, width=33, height=19, units="cm")

births$MigrationInN <- births$MigrationInN_impute
births$MigrationInN_impute <- NULL

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
  group_by(YEAR,agecat,SEX,RACE) %>% 
  summarise(MigrationInN=sum(PERWT)) %>% 
  rename(Year=YEAR,microsim.init.sex=SEX,microsim.init.race=RACE)

# now check and impute the values for 2001 - 2005 (dates where group quarters not included)
toimpute <- migrants %>% filter(agecat=="19-24" | agecat=="25-29")

# now check and impute the values for 2001 - 2005 (dates where group quarters not included)
toimpute$MigrationInN_impute <- ifelse(toimpute$Year>=2001 & toimpute$Year<=2005, NA, toimpute$MigrationInN)

toimpute <- toimpute %>% 
  group_by(agecat, microsim.init.sex, microsim.init.race) %>% 
  mutate(MigrationInN_impute = na.approx(MigrationInN_impute))

ggplot(data=toimpute, aes(x=Year, y=MigrationInN, colour=agecat)) + 
  geom_line() + 
  geom_line(aes(x=Year, y=MigrationInN_impute, colour=agecat), linetype="dashed") +
  facet_grid(cols=vars(microsim.init.sex), rows=vars(microsim.init.race))

ggsave("SIMAH_workplace/ACS/compare_imputation_19-24yearolds.png",
       dpi=300, width=33, height=19, units="cm")

toimpute$MigrationInN <- NULL
migrants <- left_join(migrants,toimpute)

migrants <- migrants %>% 
  mutate(MigrationInN = ifelse(agecat=="19-24" | agecat=="25-29",MigrationInN_impute,MigrationInN)) %>% 
  dplyr::select(-MigrationInN_impute)

migrants <- rbind(births,migrants)

# For USA -> no information about outward migration so need to assume 0 as a prior
migrants$MigrationOutN <- 0

write.csv(migrants, "SIMAH_workplace/microsim/census_data/migration_in_USA.csv", row.names=F)

