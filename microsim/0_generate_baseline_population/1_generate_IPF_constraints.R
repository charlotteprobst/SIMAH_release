# processing ACS target data for education transitions calibration
library(tidyverse)
library(ipumsr)
library(janitor)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

# in order to run this file you will need to download this data from IPUMS https://usa.ipums.org/usa/ with the following sample: 
# Sample	Density	Note
# 2000 5%	5.0%	
# VARIABLES:18(hide)
#   H	SAMPLE	IPUMS sample identifier	--
#   H	SERIAL	Household serial number	--
#   H	HHWT	Household weight	--
#   H	CLUSTER	Household cluster for variance estimation	--
#   H	STATEFIP	State (FIPS code)	--
#   H	STRATA	Household strata for variance estimation	--
#   H	GQ	Group quarters status	--
#   P	PERNUM	Person number in sample unit	--
#   P	PERWT	Person weight	--
#   P	SEX	Sex	--
#   P	AGE	Age	details
# P	RACE (general)	Race [general version]	--
#   P	RACED (detailed)	Race [detailed version]	--
#   P	HISPAN (general)	Hispanic origin [general version]	--
#   P	HISPAND (detailed)	Hispanic origin [detailed version]	--
#   P	EDUC (general)	Educational attainment [general version]	--
#   P	EDUCD (detailed)	Educational attainment [detailed version]	--

ddi <- read_ipums_ddi("SIMAH_workplace/ACS/usa_00044.xml")
data <- read_ipums_micro(ddi)

lookup_table <- data.frame(
  STATEFIP = c(
    "01", "02", "04", "05", "06", "08", "09", "10", "11", "12",
    "13", "15", "16", "17", "18", "19", "20", "21", "22", "23",
    "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
    "34", "35", "36", "37", "38", "39", "40", "41", "42", "44",
    "45", "46", "47", "48", "49", "50", "51", "53", "54", "55",
    "56", "61", "62", "63", "64", "65", "66", "67", "68", "72",
    "97", "99"
  ),
  state = c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "California",
    "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida",
    "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
    "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan",
    "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
    "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina",
    "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
    "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
    "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming",
    "Maine-New Hampshire-Vermont", "Massachusetts-Rhode Island",
    "Minnesota-Iowa-Missouri-Kansas-Nebraska-S.Dakota-N.Dakota",
    "Maryland-Delaware", "Montana-Idaho-Wyoming", "Utah-Nevada",
    "Arizona-New Mexico", "Alaska-Hawaii", "Puerto Rico", "Military/Mil. Reservation",
    "State not identified"
  )
)

lookup_table$STATEFIP <- as.integer(lookup_table$STATEFIP)

targets <- data %>% 
  filter(SAMPLE!=200004) %>% 
  filter(AGE<=79) %>% 
  filter(YEAR==2000) %>% 
  mutate(SEX = ifelse(SEX==1, "M","F"),
         AGECAT = cut(AGE, breaks=c(0,24,29,34,39,44,49,54,59,64,69,79),
                      labels=c("18.24","25.29","30.34","35.39","40.44",
                               "45.49","50.54","55.59","60.64","65.69","70.79")),
         RACE = ifelse(RACE==1, "WHI",
                       ifelse(RACE==2,"BLA","OTH")),
         RACE = ifelse(HISPAN>=1 & HISPAN<=4, "SPA",RACE),
         EDUC =  ifelse(EDUC<=6, "LEHS", ifelse(EDUC>6 & EDUC<=9, "SomeC","College"))) %>% 
  group_by(STATEFIP, YEAR, SEX, AGECAT, RACE, EDUC) %>% 
  summarise(n=sum(PERWT)) %>% 
  mutate(cat = paste0(RACE,SEX,AGECAT,EDUC)) %>% 
  ungroup() %>% 
  dplyr::select(STATEFIP, cat, n) %>% 
  pivot_wider(names_from=cat, values_from=n)

targets <- left_join(targets, lookup_table)

names(targets)

names <- names(targets)
first <- names[2]
last <- tail(names, 2)[1]

targets <- targets %>% 
  dplyr::select(state, first:last) %>% 
  adorn_totals("row") %>% 
  mutate(state = ifelse(state=="Total","USA",state))

# save the target data
write.csv(targets, "SIMAH_workplace/microsim/1_generating_population/constraints_IPF_2023.csv",row.names=F)
