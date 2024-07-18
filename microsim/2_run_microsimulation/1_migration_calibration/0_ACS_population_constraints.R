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
ddi <- read_ipums_ddi("SIMAH_workplace/ACS/usa_00044.xml")
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
         EDUC = ifelse(EDUC<=6, "LEHS",
                       ifelse(EDUC>=7 & EDUC<=9, "SomeC",
                                            ifelse(EDUC>=10, "College",NA))),
         agecat = cut(AGE,
                      breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,100),
                      labels=c("18","19-24","25-29","30-34","35-39","40-44",
                               "45-49","50-54","55-59","60-64","65-69",
                               "70-74","75-79")),) %>% 
  group_by(YEAR,agecat,SEX,RACE,EDUC) %>% 
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

write.csv(popcounts, "SIMAH_workplace/microsim/census_data/ACS_population_constraints_educ.csv", row.names=F)

summarypop <- popcounts %>% 
  mutate(microsim.init.race = recode(microsim.init.race, "WHI"="White",
                                     "BLA"="Black","SPA"="Hispanic","OTH"="Others"),
         microsim.init.sex = recode(microsim.init.sex, "m"="Men","f"="Women"),
         agecat = case_when(
           agecat == "18" | agecat == "19-24" ~ "18-24",
           agecat == "25-29" | agecat == "30-34" | agecat == "35-39" | agecat == "40-44" ~ "25-44",
           agecat == "45-49" | agecat == "50-54" | agecat == "55-59" | agecat == "60-64" ~ "45-64",
           agecat == "65-69" | agecat == "70-74" | agecat == "75-79" ~ "65-79"
         )) %>% 
  group_by(Year, microsim.init.sex, microsim.init.race, agecat) %>% 
  summarise(TotalPop=sum(TotalPop))

ggplot(data=summarypop, aes(x=Year, y=TotalPop, colour=microsim.init.sex)) + 
  geom_line(linewidth=1) + 
  facet_grid(cols=vars(agecat), rows=vars(microsim.init.race), scales="free") +
  xlim(2015, 2022) + theme_bw() + 
  ylab("Total N US population") + 
  geom_vline(xintercept=2020, linetype="dashed") + 
  ylim(0,NA) + 
  scale_y_continuous(labels=scales::label_number())

ggsave("SIMAH_workplace/ACS/totalpopulation_COVIDperiod.png",
       dpi=300, width=33, height=19, units="cm")
