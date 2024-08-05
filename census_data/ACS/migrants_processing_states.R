#####script for processing the ACS data to estimate inward and outward migration and new 18 year olds in each year
###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/SIMAH/Microsimulation"
setwd(paste(WorkingDirectory))
library(ipumsr)
library(R.utils)
library(dplyr)
library(labelled)

# gunzip("usa_00023.dat.gz", remove=FALSE)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
# read in the data 
ddi <- read_ipums_ddi("SIMAH_workplace/microsim/census_data/usa_00023.xml")
data <- read_ipums_micro(ddi)
data <- remove_labels(data)
data <- remove_attributes(data, "var_desc")

source("SIMAH_code/microsim/0_census_data/ACS/migration_cat.R")
source("SIMAH_code/microsim/0_census_data/ACS/migration_place_function.R")

# filter by people that have migrated (either within-state or into the USA)
df <- data %>% filter(MIGRATE5==3 | MIGRATE5==4 |
                        MIGRATE1==3 | MIGRATE1==4) %>% filter(YEAR!=2000)

ddi <- read_ipums_ddi("SIMAH_workplace/microsim/census_data/usa_00028.xml")
data <- read_ipums_micro(ddi)
data <- remove_labels(data)
data <- remove_attributes(data, "var_desc")

df2 <- data %>% filter(MIGRATE5==3 | MIGRATE5==4 |
                         MIGRATE1==3 | MIGRATE1==4)
df$CBSERIAL <- NULL
df <- rbind(df, df2)
rm(df2)

df <- df %>% 
  mutate(SEX = ifelse(SEX==1, "M","F"),
         RACE = ifelse(RACE==1, "WHI",
                      ifelse(RACE==2,"BLA",
                             "OTH")),
         RACE = ifelse(HISPAN==0, RACE,
                       "SPA"),
         EDUC = ifelse(EDUC<=6, "LEHS",
                       ifelse(EDUC>6 & EDUC<=9, "SomeC","College")),
         ForeignImmig = ifelse(YRIMMIG!=0,1,0))

df <- migration_place_function(df)
df$prevplace <- ifelse(is.na(df$prevplace1), df$prevplace5, df$prevplace1)
df <- migration_cat(df)

summary(as.factor(df$currentplace))

# df <- inward_migration(df)

inward_count <- df %>% select(YEAR, PERWT, SEX, AGE, RACE, EDUC, prevplace, currentplace, inwardmigrant) %>% 
  drop_na() %>% group_by(YEAR, SEX, AGE, RACE, EDUC, inwardmigrant) %>% filter(AGE>=18) %>% filter(AGE<=80)%>% 
  summarise(totalIN = ifelse(YEAR>=2001, sum(PERWT),
                           sum(PERWT)/5)) %>% 
  distinct() %>% 
  mutate(STATE = gsub("IN", "", inwardmigrant)) %>% ungroup() %>% select(-c(inwardmigrant))

outward_count <- df %>% select(YEAR, PERWT, SEX, AGE, RACE, EDUC, prevplace, currentplace, outwardmigrant) %>% 
  drop_na() %>% group_by(YEAR, SEX, AGE, RACE, EDUC, outwardmigrant) %>% filter(AGE>=18) %>% filter(AGE<=80) %>% 
  summarise(totalOUT = ifelse(YEAR>=2001, sum(PERWT),
                              sum(PERWT)/5)) %>% 
  distinct() %>% 
  mutate(STATE = gsub("OUT", "", outwardmigrant)) %>% ungroup() %>% select(-c(outwardmigrant))

years <- unique(df$YEAR)
sex <- unique(df$SEX)
age <- 18:80
race <- unique(df$RACE)
educ <- unique(df$EDUC)
state <- unique(outward_count$STATE)

cats <- expand.grid(years, sex, age, race, educ, state)
names(cats) <- c("YEAR","SEX","AGE","RACE","EDUC","STATE")

cats <- left_join(cats, inward_count)
cats <- left_join(cats, outward_count)

cats[is.na(cats)] <- 0

# adding missing years - 1985 - 1989, 1991-1999 and keep rate constant 
missing <- expand.grid(YEAR=c(1985:1989,1991:1999), SEX=sex, AGE=age, RACE=race, 
                       EDUC=educ, STATE=state, totalIN=NA, totalOUT=NA)

cats <- rbind(cats, missing)

cats <- cats %>% group_by(SEX, AGE, RACE, EDUC, STATE) %>% 
  fill(totalIN, .direction="downup") %>% fill(totalOUT, .direction="downup")

cats$NET <- cats$totalIN-cats$totalOUT

outward <- cats %>% filter(NET<0)

write.csv(outward, "SIMAH_workplace/microsim/census_data/outwardmigration_states.csv", row.names=FALSE)

inward <- cats %>% filter(NET>0)
write.csv(inward, "SIMAH_workplace/microsim/census_data/inwardmigration_states.csv", row.names=FALSE)

migrants <- left_join(inward_count, outward_count)


# First iteration just do USA = people that have moved from abroad and are now in the USA 
USA <- df %>% filter(MIGRATE1==4 | MIGRATE5==4) %>% group_by(YEAR, SEX, AGE, RACE, EDUC) %>% 
  summarise(total=ifelse(YEAR>=2001, sum(PERWT),
                         sum(PERWT)/5)) %>% filter(AGE>=18) %>% filter(AGE<=80)

write.csv(USA, "SIMAH_workplace/microsim/census_data/migrationcons.csv", row.names=F)
