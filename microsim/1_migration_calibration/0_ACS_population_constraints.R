#####script for processing the ACS data to estimate the total population required in the simulation in each year
###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/SIMAH/Microsimulation"
setwd(paste(WorkingDirectory))
library(ipumsr)
library(R.utils)
library(dplyr)
library(labelled)
library(ggplot2)
library(zoo)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
# read in the data # # you will need to download this from IPUMS https://usa.ipums.org/usa/index.shtml
# specifications for the data are below
# 2000 1%	1.0%, 2001 ACS	0.43%	Does not include persons in group quarters, 2002 ACS	0.38%	Does not include persons in group quarters.
# 2003 ACS	0.42%	Does not include persons in group quarters, 2004 ACS	0.42%	Does not include persons in group quarters, 2005 ACS	1.0%	Does not include persons in group quarters.
# 2006 ACS	1.0%,2007 ACS	1.0,2008 ACS	1.0%,2009 ACS	1.0%,2010 ACS	1.0%, 2011 ACS	1.0%	
# 2012 ACS	1.0%, 2013 ACS	1.0%, 2014 ACS	1.0%, 2015 ACS	1.0%, 2016 ACS	1.0%	
# 2017 ACS	1.0%, 2018 ACS	1.0%, 2019 ACS	1.0%, 2020 ACS	1.0%	Uses experimental weights to correct for the effects of the COVID-19 pandemic on the 2020 ACS data collection
# 2021 ACS	1.0%, 2022 ACS	1.0%	
#
# Type	Variable	Label	Case Selection
#   H	SAMPLE	IPUMS sample identifier	--
#   H	SERIAL	Household serial number	--
#   H	CBSERIAL	Original Census Bureau household serial number	--
#   H	HHWT	Household weight	--
#   H	CLUSTER	Household cluster for variance estimation	--
#   H	STATEFIP	State (FIPS code)	--
#   H	STRATA	Household strata for variance estimation	--
#   H	GQ	Group quarters status	--
#   P	PERNUM	Person number in sample unit	--
#   P	PERWT	Person weight	--
#   P	EXPWTP	Experimental person weight	--
#   P	SEX	Sex	--
#   P	AGE	Age	details
# P	RACE (general)	Race [general version]	--
#   P	RACED (detailed)	Race [detailed version]	--
#   P	HIHispanicN (general)	Hispanic origin [general version]	--
#   P	HIHispanicND (detailed)	Hispanic origin [detailed version]	--
#   P	EDUC (general)	Educational attainment [general version]	--
#   P	EDUCD (detailed)	Educational attainment [detailed version]	--
#   P	MIGRATE5 (general)	Migration status, 5 years [general version]	--
#   P	MIGRATE5D (detailed)	Migration status, 5 years [detailed version]	--
#   P	MIGRATE1 (general)	Migration status, 1 year [general version]	--
#   P	MIGRATE1D (detailed)	Migration status, 1 year [detailed version]	--
#   P	MIGPLAC5	State or country of residence 5 years ago	--
#   P	MIGPLAC1	State or country of residence 1 year ago	--

ddi <- read_ipums_ddi("SIMAH_workplace/ACS/usa_00044.xml")
data <- read_ipums_micro(ddi)
data <- remove_labels(data)
data <- remove_attributes(data, "var_desc")

popcounts <- data %>% 
  # remove the ACS sample without group quarters for 2000
  filter(SAMPLE!=200004) %>% 
  mutate(SEX=recode(SEX,"1"="m","2"="f"),
         RACE = ifelse(RACE==1, "White",
                       ifelse(RACE==2,"Black",
                              "Others")),
         RACE = ifelse(HISPAN==0, RACE,
                       "Hispanic"),
         RACE_ALT = ifelse(RACED==100, "White",
                           ifelse(RACED==200, "Black",
                                  ifelse(RACED>=300 & RACED<=700, "Others",
                                         ifelse(RACED==801, "Black",
                                                ifelse(RACED>=802 & RACED<=827, "Others",
                                                       ifelse(RACED>=830 & RACED<=845, "Black",
                                                              ifelse(RACED>=850, "Others", NA))))))),
         RACE_ALT = ifelse(HISPAN==0, RACE_ALT, "Hispanic"),
         EDUC = ifelse(EDUC<=6, "LEHS",
                       ifelse(EDUC>=7 & EDUC<=9, "SomeC",
                                            ifelse(EDUC>=10, "College",NA))),
         agecat = cut(AGE,
                      breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,100),
                      labels=c("18","19-24","25-29","30-34","35-39","40-44",
                               "45-49","50-54","55-59","60-64","65-69",
                               "70-74","75-79")),) %>% 
  group_by(YEAR,agecat,SEX,RACE) %>% 
  summarise(
    TotalPop=sum(PERWT)) %>% 
  rename(Year=YEAR, sex=SEX,race=RACE) %>% 
  mutate(agecat=as.character(agecat)) 

toimpute <- popcounts %>% 
  filter(agecat=="18" | agecat=="19-24" | agecat=="25-29") %>% ungroup() %>% 
  mutate(TotalPop_impute = ifelse(Year>=2001 & Year<=2005, NA, TotalPop)) %>% 
  group_by(agecat, sex, race) %>% 
  mutate(TotalPop_impute = na.approx(TotalPop_impute)) %>% 
  dplyr::select(-TotalPop)

popcounts <- left_join(popcounts, toimpute)
# draw a plot to check
ggplot(data=subset(popcounts,sex=="m"), aes(x=Year, y=TotalPop)) + 
  geom_line() + 
  geom_line(aes(x=Year, y=TotalPop_impute), colour="red") +
  facet_grid(cols=vars(agecat), rows=vars(race)) + 
  geom_vline(aes(xintercept=2006), linetype="dashed")

# ggsave("SIMAH_workplace/ACS/compare_imputation_agecats.png",
#        dpi=300, width=33, height=19, units="cm")

popcounts <- popcounts %>% 
  mutate(TotalPop_final = ifelse(is.na(TotalPop_impute), TotalPop, TotalPop_impute)) %>% 
  dplyr::select(-c(TotalPop,TotalPop_impute)) %>% 
  rename(TotalPop = TotalPop_final)

write.csv(popcounts, "SIMAH_workplace/microsim/population_data/ACS_population_constraints.csv", row.names=F)

summarypop <- popcounts %>% 
  mutate(race = recode(race, "White"="Whitete",
                                     "Black"="Blackck","Hispanic"="Hispanic","Others"="Others"),
         sex = recode(sex, "m"="Men","f"="Women"),
         agecat = case_when(
           agecat == "18" | agecat == "19-24" ~ "18-24",
           agecat == "25-29" | agecat == "30-34" | agecat == "35-39" | agecat == "40-44" ~ "25-44",
           agecat == "45-49" | agecat == "50-54" | agecat == "55-59" | agecat == "60-64" ~ "45-64",
           agecat == "65-69" | agecat == "70-74" | agecat == "75-79" ~ "65-79"
         )) %>% 
  group_by(Year, sex, race, agecat) %>% 
  summarise(TotalPop=sum(TotalPop))

ggplot(data=summarypop, aes(x=Year, y=TotalPop, colour=sex)) + 
  geom_line(linewidth=1) + 
  facet_grid(cols=vars(agecat), rows=vars(race), scales="free") +
  xlim(2015, 2022) + theme_bw() + 
  ylab("Total N US population") + 
  geom_vline(xintercept=2020, linetype="dashed") + 
  ylim(0,NA) + 
  scale_y_continuous(labels=scales::label_number())

ggsave("SIMAH_workplace/ACS/totalpopulation_COVIDperiod.png",
       dpi=300, width=33, height=19, units="cm")
