####COMPARING MIGRANTS AND 12 YEAR OLDS 
###creating a constraints list for the IPF for migrants AND 12 year olds together
library(dplyr)
library(tidyr)
library(reshape2)
###READ IN ALL THE PROCESSED VERSIONS OF CONSTRAINTS 
###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/SIMAH/Microsimulation"
setwd(paste(WorkingDirectory))
# # or for 18 year olds BRFSS version 
eighteen1990 <- read.csv("SIMAH_workplace/microsim/census_data/18yearoldcons1990.csv") %>% mutate(CensusYear=1990)
eighteen2000 <- read.csv("SIMAH_workplace/microsim/census_data/18yearoldcons2000.csv") %>% mutate(CensusYear=2000)
eighteen2010 <- read.csv("SIMAH_workplace/microsim/census_data/18yearoldcons2010.csv") %>% mutate(CensusYear=2010) %>% dplyr::select(-c(X))
census <- rbind(eighteen1990, eighteen2000, eighteen2010)

rm(eighteen1990, eighteen2000, eighteen2010)

colnames(census) <- c("State", "CensusAge", "Race", "Sex", "Censuscount", "CensusYear")

# # WHEN 18 YEAR OLDS ARE IN
census$entryyear <- ifelse(census$CensusAge==18, census$CensusYear,
                           ifelse(census$CensusAge==17, census$CensusYear+1,
                                  ifelse(census$CensusAge==16, census$CensusYear+2,
                                 ifelse(census$CensusAge==15, census$CensusYear+3,
                                        ifelse(census$CensusAge==14, census$CensusYear+4,
                                               ifelse(census$CensusAge==13, census$CensusYear+5,
                                                      ifelse(census$CensusAge==12, census$CensusYear+6,
                                                             ifelse(census$CensusAge==11, census$CensusYear+7,
                                                                    ifelse(census$CensusAge==10, census$CensusYear+8,
                                                                           ifelse(census$CensusAge==9, census$CensusYear+9,
                                                                                  ifelse(census$CensusAge==19, census$CensusYear-1,
                                                                                         ifelse(census$CensusAge==20, census$CensusYear-2,
                                                                                                ifelse(census$CensusAge==21, census$CensusYear-3,
                                                                                                       ifelse(census$CensusAge==22, census$CensusYear-4,
                                                                                                              ifelse(census$CensusAge==23, census$CensusYear-5,
                                                                                                                     ifelse(census$CensusAge==24, census$CensusYear-6, NA)
                                                                                                              )))))))))))))))
summary(as.factor(census$entryyear))

census <- census %>% dplyr::select(entryyear, State, Sex, Race, Censuscount) %>% rename(Count=Censuscount, Year=entryyear) %>% 
  mutate(State=as.character(State),
         Sex=as.character(Sex),
         Race=as.character(Race),
         EDUC = "LEHS")

census$Age <- 18
census$ACScount <- 0

###these are the individuals that WOULD be born into the model in consecutive years 1981 - BUT could be overlap with migrants 
##read in the migrants file 
migrants <- read.csv("SIMAH_workplace/microsim/census_data/migrationcons.csv") %>% mutate(State="USA") %>% 
  rename(Year=YEAR,
         Sex=SEX,
         Race=RACE,
         Age=AGE,
         ACScount = total) %>% mutate(Count=0) %>% filter(Age>=18 & Age<=80)

setdiff(names(migrants), names(census))
setdiff(names(census), names(migrants))

compare <- rbind(census,migrants)
compare <- compare %>% group_by(Year, State, Sex, Race, Age,EDUC, .drop=FALSE) %>% summarise(Count=sum(Count), ACScount=sum(ACScount)) %>% ungroup()

compare$newcount <- ifelse(compare$Age==18, compare$Count - compare$ACScount,
                           compare$ACScount)
compare$newcount <- abs(compare$newcount)

# save the constraints 
save <- compare %>% mutate(agecat = cut(Age, 
                                        breaks=c(0,18,24,34,44,54,64,100),
                                        labels=c("18","19-24","25-34","35-44","45-54",
                                                 "55-64","65+"))) %>% 
  select(Year, State, Sex, Race,agecat, newcount) %>% 
  mutate(Sex = ifelse(Sex=="female","F","M")) %>% 
  group_by(Year,State,Sex,Race,agecat, .drop=FALSE) %>% 
  summarise(newcount=sum(newcount)) %>% 
  mutate(Cat = paste(Sex,agecat,Race, sep="")) %>% ungroup() %>% select(Year, State,Cat,newcount) %>% 
  pivot_wider(names_from=Cat, values_from=newcount)

years <- 1984:2019
constraints <- list()
for(i in 1:length(years)){
  constraints[[i]] <- data.frame(subset(save, Year==years[i]))
  constraints[[i]]$Year <- NULL
}
names(constraints) <- years

library(readr)

# setwd("~/Desktop/repos/SIMAH/Microsynthesis/migrants/input_data")
# 
# write_rds(constraints, "constraints_migrants_18.RDS")

# save the individual ages for imputation 
# compare <- compare %>% group_by(Year, State, Sex, Race, Age, EDUC) %>% 
#   summarise(Count = sum(newcount))
# write.csv(compare, "individualages.csv", row.names=FALSE)

# read in state level data 
indages <- read.csv("SIMAH_workplace/microsim/1_input_data/individualages_states.csv")

compare$Sex <- recode(compare$Sex, "F"="f","female"="f","male"="m","M"="m")
compare <- compare %>% group_by(Year, State, Sex, Race, Age, EDUC) %>% 
  summarise(Count = sum(newcount))

names(indages)
names(compare)
compare <- rbind(indages,compare)
write.csv(compare, "SIMAH_workplace/microsim/1_input_data/inward_migration.csv", row.names=FALSE)

