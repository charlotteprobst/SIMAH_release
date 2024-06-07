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

popcounts <- data %>% 
  filter(SAMPLE!=200004) %>% 
  mutate(SEX=recode(SEX,"1"="m","2"="f"),
         RACE = ifelse(RACE==1, "WHI",
                       ifelse(RACE==2,"BLA",
                              "OTH")),
         RACE = ifelse(HISPAN==0, RACE,
                       "SPA"),
         agecat = cut(AGE,
                      breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,100),
                      labels=c("18","19-24","25-29","30-34","35-39","40-44",
                               "45-49","50-54","55-59","60-64","65-69",
                               "70-74","75-79")),) %>% 
  group_by(YEAR,agecat,SEX,RACE) %>% 
  summarise(
    TotalPop=sum(PERWT)) %>% 
  rename(Year=YEAR, microsim.init.sex=SEX,microsim.init.race=RACE) %>% 
  mutate(agecat=as.character(agecat)) 

toimpute <- popcounts %>% 
  filter(agecat=="18" | agecat=="19-24" | agecat=="25-29") %>% ungroup() %>% 
  mutate(TotalPop_impute = ifelse(Year>=2001 & Year<=2005, NA, TotalPop)) %>% 
  group_by(agecat, microsim.init.sex, microsim.init.race) %>% 
  mutate(TotalPop_impute = na.approx(TotalPop_impute)) %>% 
  dplyr::select(-TotalPop)

popcounts <- left_join(popcounts, toimpute)
                                  
ggplot(data=subset(popcounts,microsim.init.sex=="m"), aes(x=Year, y=TotalPop)) + 
  geom_line() + 
  geom_line(aes(x=Year, y=TotalPop_impute), colour="red") +
  facet_grid(cols=vars(agecat), rows=vars(microsim.init.race)) + 
  geom_vline(aes(xintercept=2006), linetype="dashed")

ggsave("SIMAH_workplace/ACS/compare_imputation_agecats.png",
       dpi=300, width=33, height=19, units="cm")

popcounts <- popcounts %>% 
  mutate(TotalPop_final = ifelse(is.na(TotalPop_impute), TotalPop, TotalPop_impute)) %>% 
  dplyr::select(-c(TotalPop,TotalPop_impute)) %>% 
  rename(TotalPop = TotalPop_final)

write.csv(popcounts, "SIMAH_workplace/microsim/census_data/ACS_population_constraints.csv", row.names=F)

