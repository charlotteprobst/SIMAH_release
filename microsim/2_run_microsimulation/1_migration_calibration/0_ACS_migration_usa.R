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
library(zoo)
# gunzip("SIMAH_workplace/ACS/usa_00040.dat.gz", remove=FALSE)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
# # read in the data 
# # you will need to download this from IPUMS https://usa.ipums.org/usa/index.shtml
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
#   P	HISPAN (general)	Hispanic origin [general version]	--
#   P	HISPAND (detailed)	Hispanic origin [detailed version]	--
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

births <- data %>% 
  filter(AGE==18) %>% 
  # filter(MIGRATE1!=4) %>% 
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

ggsave("SIMAH_workplace/ACS/compare_imputation_18yearolds_incl_2022.png",
       dpi=300, width=33, height=19, units="cm")

births$MigrationInN <- births$MigrationInN_impute
births$MigrationInN_impute <- NULL
# births$BirthsIN <- births$MigrationInN
# births$MigrationInN <- NULL

# exclude migrants when we select the 18-year olds

migrants <- data %>% 
  filter(MIGRATE1==4) %>% 
  filter(SAMPLE!=200007) %>% 
  # filter(AGE!=18) %>% 
  mutate(agecat = cut(AGE,
                      breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,100),
                      labels=c("18","19-24","25-29","30-34","35-39","40-44",
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
toimpute$MigrationInN_impute <- ifelse(toimpute$Year>=2001 & toimpute$Year<=2005, NA, 
                                       ifelse(toimpute$Year>2019, NA, toimpute$MigrationInN))

toimpute <- toimpute %>% 
  group_by(agecat, microsim.init.sex, microsim.init.race) %>% 
  mutate(MigrationInN_impute = ifelse(is.na(MigrationInN_impute), 
                                      na.approx(MigrationInN_impute), MigrationInN_impute),
         MigrationInN_impute = ifelse(Year>2019, MigrationInN, MigrationInN_impute))

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

births$birth <- "births"
migrants$birth <- "migrants"

compare_18 <- rbind(births,migrants) %>% 
  pivot_wider(names_from=birth, values_from=MigrationInN) %>% 
  mutate(BirthsInN = births-migrants,
         BirthsInN = ifelse(is.na(BirthsInN), births, BirthsInN)) %>% 
  dplyr::select(-c(migrants,births)) %>% 
  filter(agecat=="18")

migrants$birth <- NULL

migrants <- rbind(compare_18,migrants)

# For USA -> no information about outward migration so need to assume 0 as a prior
migrants$MigrationOutN <- 0
# migrants$Year <- migrants$Year-1

write.csv(migrants, "SIMAH_workplace/microsim/1_input_data/migration_in_USA.csv", row.names=F)

# summary of COVID years migrants and births
test=subset(migrants,agecat=="18") %>% 
  dplyr::select(Year, microsim.init.sex,microsim.init.race, BirthsInN) %>% drop_na() %>% 
  mutate(BirthsInN_impute = ifelse(Year==2020, NA, BirthsInN),
         BirthsInN_impute = ifelse(is.na(BirthsInN_impute), 
                na.approx(BirthsInN_impute), BirthsInN_impute),
         microsim.init.race = recode(microsim.init.race, "WHI"="White",
                                     "BLA"="Black","SPA"="Hispanic","OTH"="Others"),
         microsim.init.sex = recode(microsim.init.sex, "m"="Men","f"="Women"))

ggplot(data=test, aes(x=Year, y=BirthsInN)) + 
  geom_line(linewidth=1) + 
  geom_line(aes(x=Year, y=BirthsInN_impute), linetype="dashed") + 
  facet_grid(cols=vars(microsim.init.race), rows=vars(microsim.init.sex)) +
  xlim(2015, 2022) + theme_bw() + 
  ylab("Total N 'births' aged 18") + 
  geom_vline(xintercept=2020, linetype="dashed")

ggsave("SIMAH_workplace/ACS/newbirths_COVIDperiod.png",
       dpi=300, width=33, height=19, units="cm")

migrants <- migrants %>% 
  mutate(microsim.init.race = recode(microsim.init.race, "WHI"="White",
                                              "BLA"="Black","SPA"="Hispanic","OTH"="Others"),
         microsim.init.sex = recode(microsim.init.sex, "m"="Men","f"="Women"))

summarymigrants <- migrants %>% group_by(Year, microsim.init.sex,
                                         microsim.init.race) %>% 
  summarise(MigrationInN = sum(MigrationInN,na.rm=T))

ggplot(data=summarymigrants, aes(x=Year, y=MigrationInN)) + 
  geom_line(linewidth=1) + 
  facet_grid(cols=vars(microsim.init.race), rows=vars(microsim.init.sex)) +
  xlim(2015, 2022) + theme_bw() + 
  ylab("Total N migrants entering USA") + 
  geom_vline(xintercept=2020, linetype="dashed")

ggsave("SIMAH_workplace/ACS/newmigrants_COVIDperiod.png",
       dpi=300, width=33, height=19, units="cm")

