# script to process PSID data for Markov modelling 
library(foreign)
library(dplyr)
library(tidyr)
setwd("~/Google Drive/SIMAH Sheffield")

data <- read.dbf("SIMAH_workplace/education_transitions/J292420/J292420.dbf")

data$origINTNO <- data$ER30001
data$ID <- data$ER30002
data$uniqueID <- (data$origINTNO*1000) + data$ID
data$sex <- data$ER32000
data$sex <- recode(as.factor(data$sex), "1"="male", "2"="female")

education <- c("ER33516","ER33616","ER33716","ER33817","ER33917","ER34020","ER34119","ER34230","ER34349","ER34548", "ER34752")

educationvars <- data %>% select(uniqueID, sex, c(education))
years <- c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
names(educationvars)[3:13] <- years
educationvars[educationvars==0] <- NA
educationvars[educationvars==99] <- NA
educationvars[educationvars==98] <- NA
educationvars <- educationvars %>% pivot_longer(cols='1999':'2019', names_to="year", values_to="highestEd")
sum(is.na(educationvars$highestEd))

# data1 <- read.dbf("J285746/J285746.dbf")
agevars <- c("ER33506","ER33606","ER33706","ER33806","ER33906","ER34006","ER34106",
             "ER34206","ER34307","ER34506", "ER34706")

age <- data %>% select(uniqueID, c(agevars))
names(age)[2:12] <- years
age[age==0] <- NA
age[age==9999] <- NA
age <- age %>% pivot_longer(cols='1999':'2019', names_to="year", values_to="BirthYear") %>% 
  group_by(uniqueID) %>% fill(BirthYear) %>% mutate(year = as.numeric(year),
                                                    age = year-BirthYear) %>% 
  select(-c(BirthYear))

# relationtohouseholdercheck 
householdervars <- c("ER33503","ER33603","ER33703","ER33803","ER33903","ER34003","ER34103",
                     "ER34203","ER34303","ER34503", "ER34703")

householder <- data %>% select(uniqueID, c(householdervars))
names(householder)[2:12] <- years
householder[householder==0] <- NA
householder <- householder %>% pivot_longer(cols='1999':'2019', names_to="year", values_to="relationship") %>% 
  group_by(uniqueID) %>% fill(relationship, .direction="downup")
householder$relationshiptohead <- ifelse(householder$relationship==10, "head",
                                         ifelse(householder$relationship==20 | 
                                                  householder$relationship==22, 
                                                "wife/partner",
                                                ifelse(householder$relationship==30 | householder$relationship==37 | householder$relationship==38, "childofhead",
                                                       ifelse(householder$relationship==33 | householder$relationship==35, "childofpartner",
                                                              ifelse(householder$relationship==0, "latino/immigrantsampleunknown",
                                                                     ifelse(householder$relationship==40, "brotherofhead",
                                                                            ifelse(householder$relationship==47 | householder$relationship==48, "brotherofwife",
                                                                                   ifelse(householder$relationship==50, "parentofhead",
                                                                                          ifelse(householder$relationship==57 | householder$relationship==58, "parentofwife",
                                                                                                 ifelse(householder$relationship==60 | householder$relationship==65, "grandson",
                                                                                                        ifelse(householder$relationship==88, "cohabitor",
                                                                                                               ifelse(householder$relationship==83, "childofcohabitor",
                                                                                                                      ifelse(householder$relationship==98, "nonrelative",
                                                                                                                             "otherrelatives")))))))))))))
summary(as.factor(householder$relationshiptohead))
householder <- householder %>%
  select(uniqueID, year, relationshiptohead) %>% mutate(year=as.numeric(year))
educationvars$year <- as.numeric(educationvars$year)
educationvars <- left_join(educationvars, age)
educationvars <- left_join(educationvars, householder)
subset <- educationvars %>% filter(age>=18 & age<=80) %>% group_by(uniqueID) %>% 
  fill(highestEd, .direction=c("downup"))

length(unique(subset$uniqueID))

# recategorise to LEHS, someC and College+
# 12 years education = LEHS, 13-15 = some college and 16 = college + (I think)
# 13 = 1 year college 
# 14 = 2 years college
# 15 = 3 years college 
# 16 = 4 years college (College+)

summary(subset$highestEd)

subset$educationCAT <- ifelse(subset$highestEd<=12, "LEHS",
                              ifelse(subset$highestEd>12 & subset$highestEd<16, "SomeC",
                                     ifelse(subset$highestEd>=16, "College", NA)))
subset$educationCATdetailed <- ifelse(subset$highestEd<=12, "LEHS",
                                      ifelse(subset$highestEd==13, "1 year college",
                                             ifelse(subset$highestEd==14, "2 year college",
                                                    ifelse(subset$highestEd==15, "3 year college",
                                                           ifelse(subset$highestEd>=16, "College", NA)))))

# how many missing data points
sum(is.na(subset$educationCATdetailed))
# 26784 / 87816 data points missing
summary <- subset %>% group_by(year, educationCAT) %>% tally() %>% group_by(educationCAT) %>% summarise(sum(n))

# work out if this is still representative of the 18-35 year olds education from the census data
# apply weights

familyweights <- c("ER16518","ER20394","ER24179","ER28078","ER41069","ER47012","ER52436",
                   "ER58257","ER65492","ER71570","ER77631")

weights <- data %>% select(uniqueID, c(familyweights))

names(weights)[2:12] <- years
weights <- weights %>% pivot_longer(cols='1999':'2019', names_to="year", values_to="weight") %>% 
  mutate(year=as.integer(year))
subset <- left_join(subset,weights)

data$latino <- ifelse(data$ER30001>=3001 & data$ER30001<=3511, "latinosample",
                      ifelse(data$ER30001>=4001 & data$ER30001<=4851, "latinosample",
                             ifelse(data$ER30001>=7001 & data$ER30001<=9308, "latinosample", "not")))

latino <- data.frame(uniqueID=data$uniqueID, latino=data$latino)
subset <- left_join(subset, latino)

parents <- data %>% 
  mutate(uniqueID = (ER30001*1000) + ER30002,
         IDmother = ifelse(ER32010==0, NA,
                           (ER32010*1000) + ER30002),
         birthyearmother = ifelse(ER32011>9990, NA, ER32011),
         IDfather = ifelse(ER32017==0, NA,
                           (ER32017*1000) + ER30002),
         birthyearfather = ifelse(ER32018>9990, NA, ER32018)) %>% 
  select(uniqueID, IDmother, IDfather, birthyearmother, birthyearfather)

raceeth <- read.dbf("SIMAH_workplace/education_transitions/J292599/J292599.dbf")

raceeth$origINTNO <- raceeth$ER30001
raceeth$ID <- raceeth$ER30002
raceeth$uniqueID <- (raceeth$origINTNO*1000) + raceeth$ID
raceeth$sex <- raceeth$ER32000
raceeth$sex <- recode(as.factor(raceeth$sex), "1"="male", "2"="female")

race <- c("V181","V801","V1490","V2202","V2828","V3300","V3720","V4204","V5096","V5662","V6209","V6802",
          "V7447","V8099","V8723","V9408","V11055","V11938","V13565","V14612","V16086",
          "V17483","V18814","V20114","V21420","V23276","ER3944","ER6814","ER9060","ER11848","ER15928",
          "ER19989","ER23426","ER27393","ER40565","ER46543","ER51904","ER57659","ER64810","ER70882", "ER76897")

racevars <- raceeth %>% select(uniqueID, sex, c(race))
years <- c(1968:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
names(racevars)[3:43] <- years
racevars <- racevars %>% pivot_longer(cols='1968':'2019', names_to="year", values_to="racehead") %>% 
  mutate(racehead = ifelse(racehead==1, "white",
                           ifelse(racehead==2, "black",
                                  ifelse(racehead==9, NA,
                                         ifelse(racehead==3 & year<=1984, "hispanic",
                                                ifelse(racehead==9, NA,
                                                       ifelse(racehead==0, NA,
                                                              ifelse(racehead==5 & year>=1990 & year<=2003, "hispanic",
                                                                     ifelse(racehead==3 & year>=1985, "Native",
                                                                            ifelse(racehead==4 & year>=1985, "Asian/PI",
                                                                                   "other")))))))))) %>% group_by(uniqueID) %>% 
  fill(racehead, .direction=c("downup")) %>% distinct()

summary(as.factor(racevars$racehead))

# recode values and fill for each person any missing years 
# 1968 - 1984 
# 1 = white, 2=black, 3=hispanic, 7 = other, 9=NA 
# 1984 - 1989
# 1 = white, 2=black, 3=other, 4=other, 5=other, 7=other 8=other, 9=NA
# 1990 - 2003 
# 1 = white, 2=black, 3=other, 4=other, 5=hispanic, 6=other, 7=other, 9=NA
# 2004 - 2017
# 1 = white, 2=black, 3=other, 4=other, 5=other, 7=other, 0=NA, 9=NA

# now get the race of wife variable
wife <- c("V12293","V13500","V14547","V16021","V17418","V18749","V20049","V21355","V23212",
          "ER3883","ER6753","ER8999","ER11760","ER15836","ER19897","ER23334","ER27297",
          "ER40472","ER46449","ER51810","ER57549","ER64671","ER70744","ER76752")
wiferace <- raceeth %>% select(uniqueID, sex, c(wife))
years <- c(1985:1997,1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
names(wiferace)[3:26] <- years
wiferace <- wiferace %>% pivot_longer(cols='1985':'2019', names_to="year", values_to="racewife") %>% 
  mutate(racewife = ifelse(racewife==1, "white",
                           ifelse(racewife==2, "black",
                                  ifelse(racewife==9, NA,
                                         ifelse(racewife==9, NA,
                                                ifelse(racewife==0, NA,
                                                       ifelse(racewife==5 & year>=1990 & year<=2003, "hispanic",
                                                              ifelse(racewife==3 & year>=1985, "Native",
                                                                     ifelse(racewife==4 & year>=1985, "Asian/PI",
                                                                            "other"))))))))) %>% group_by(uniqueID) %>% 
  fill(racewife, .direction=c("downup")) %>% distinct()

# now get hispanic info for the head and wife
hispanic1 <- c("V11937","V13564","V14611","V16085","V17482","V18813","V20113",
               "V21419","V23275","ER3941","ER6811","ER9057","ER27392","ER40564",
               "ER46542","ER51903","ER57658","ER64809","ER70881","ER76896")
years <- c(1985:1996, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
hispanic <- raceeth %>% select(uniqueID, sex, c(hispanic1))
names(hispanic)[3:22] <- years
hispanic <- hispanic %>% pivot_longer(cols='1985':'2019', names_to="year", values_to="hispanichead") %>% 
  mutate(hispanichead = ifelse(hispanichead==9, NA,
                               ifelse(hispanichead==0, "nothispanic",
                                      "hispanic"))) %>% group_by(uniqueID) %>% 
  fill(hispanichead, .direction=c("downup")) %>% distinct()

# hispanic wife 
hispanic2 <- c("V12292","V13499","V14546","V16020",
               "V17417","V18748","V20048","V21354",
               "V23211","ER3880","ER6750","ER8996",
               "ER27296","ER40471","ER46448","ER51809",
               "ER57548","ER64670","ER70743","ER76751")
hispanicwife <- raceeth %>% select(uniqueID, sex, c(hispanic2))
names(hispanicwife)[3:22] <- years
hispanicwife <- hispanicwife %>% pivot_longer(cols='1985':'2019', names_to="year", values_to="hispanicwife") %>% 
  mutate(hispanicwife = ifelse(hispanicwife==9, NA,
                               ifelse(hispanicwife==0, "nothispanic",
                                      "hispanic"))) %>% group_by(uniqueID) %>% 
  fill(hispanicwife, .direction=c("downup")) %>% distinct()
# combine all race variables together 
raceall <- inner_join(racevars, wiferace)
raceall <- inner_join(raceall, hispanic)
raceall <- inner_join(raceall, hispanicwife)

raceall$year <- as.numeric(raceall$year)

rm(racevars, wiferace, hispanic, hispanicwife,
   age, educationvars, householder, latino, summary,
   weights, agevars, education, familyweights,
   hispanic1, hispanic2, householdervars, race, wife,
   years)

subset <- left_join(subset, raceall)
subset <- subset %>% ungroup() %>% group_by(uniqueID) %>%
  fill(c(racehead, racewife, hispanichead, hispanicwife), .direction=c("downup"))

rm(raceall, raceeth)
# recode race into a single race/ethnicity variable for each individual
subset <- subset %>% group_by(uniqueID) %>% fill(relationshiptohead, .direction=c("downup")) %>% 
  mutate(raceethhead = ifelse(year>=1990 & year<=2003, racehead,
                              ifelse(year>=2004 & racehead=="white" & hispanichead=="nothispanic","white",
                                     ifelse(year>=2004 & racehead=="black" & hispanichead=="nothispanic","black",
                                            ifelse(year>=2004 & racehead=="other" & hispanichead=="nothispanic","other",
                                                   ifelse(year>=2004 & hispanichead=="hispanic","hispanic", 
                                                          ifelse(year>=2004 & racehead=="Asian/PI" & hispanichead=="nothispanic","Asian/PI",
                                                                 ifelse(year>=2004 & racehead=="Native" & hispanichead=="nothispanic","Native",
                                                                        ifelse(year<=1989 & racehead=="white" & hispanichead=="nothispanic", "white",
                                                                               ifelse(year<=1989 & racehead=="black" & hispanichead=="nothispanic","black",
                                                                                      ifelse(year<=1989 & racehead=="other" & hispanichead=="nothispanic","other",
                                                                                             ifelse(year<=1989 & hispanichead=="hispanic","hispanic",
                                                                                                    ifelse(year<=1989 & racehead=="Asian/PI" & hispanichead=="nothispanic","Asian/PI",
                                                                                                           ifelse(year<=1989 & racehead=="Native" & hispanichead=="nothispanic","Native",
                                                                                                                  NA))))))))))))),
         raceethwife = ifelse(year>=1990 & year<=2003, racewife,
                              ifelse(year>=2004 & racewife=="white" & hispanicwife=="nothispanic","white",
                                     ifelse(year>=2004 & racewife=="black" & hispanicwife=="nothispanic","black",
                                            ifelse(year>=2004 & racewife=="other" & hispanicwife=="nothispanic","other",
                                                   ifelse(year>=2004 & hispanicwife=="hispanic","hispanic", 
                                                          ifelse(year>=2004 & racewife=="Native" & hispanicwife=="nothispanic","Native",
                                                                 ifelse(year>=2004 & racewife=="Asian/PI" & hispanicwife=="nothispanic", "Asian/PI",
                                                                        ifelse(year<=1989 & racewife=="white" & hispanicwife=="nothispanic", "white",
                                                                               ifelse(year<=1989 & racewife=="black" & hispanicwife=="nothispanic","black",
                                                                                      ifelse(year<=1989 & racewife=="other" & hispanicwife=="nothispanic","other",
                                                                                             ifelse(year<=1989 & hispanicwife=="hispanic","hispanic",
                                                                                                    ifelse(year<=1989 & racewife=="Native" & hispanicwife=="nothispanic","Native",
                                                                                                           ifelse(year<=1989 & racewife=="Asian/PI" & hispanicwife=="nothispanic","Asian/PI",
                                                                                                                  NA))))))))))))))

subset <- subset %>% group_by(uniqueID) %>% fill(raceethhead, .direction="downup") %>% 
  fill(raceethwife, .direction="downup")

# priority coding of race 
# every possible combination 
source("SIMAH_code/education_transitions/1_data_processing/recode_race_function.R")
subset <- recode_race_function(subset, T)
subset$racefamily <- subset$racenew
subset <- subset %>% group_by(uniqueID) %>% fill(racefamily, raceethhead, raceethwife, .direction="downup") %>% 
  select(uniqueID, sex, age, year, highestEd, educationCAT, educationCATdetailed, weight,
         relationshiptohead, raceethhead, raceethwife, racefamily, latino) %>% data.frame(.)

subset$racefinal <- ifelse(subset$relationshiptohead=="head", subset$raceethhead,
                            ifelse(subset$relationshiptohead=="wife/partner", subset$raceethwife, 
                                   ifelse(subset$relationshiptohead=="cohabitor", subset$raceethwife,
                                          ifelse(subset$relationshiptohead=="childofhead", subset$racefamily,
                                                 ifelse(subset$relationshiptohead=="childofpartner", subset$raceethwife,
                                                        ifelse(subset$relationshiptohead=="grandson", subset$racefamily,
                                                               ifelse(subset$relationshiptohead=="parentofhead", subset$raceethhead,
                                                                      ifelse(subset$relationshiptohead=="parentofwife", subset$raceethwife,
                                                                             subset$racefamily))))))))
summary(as.factor(subset$racefinal))

subset <- left_join(subset, parents)
mother <- subset %>% select(uniqueID, racefinal) %>% distinct() %>% rename(IDmother=uniqueID,
                                                                            racemother=racefinal)
subset <- left_join(subset, mother)

father <- subset %>% select(uniqueID, racefinal) %>% distinct() %>% rename(IDfather=uniqueID, racefather=racefinal)
subset <- left_join(subset, father)

rm(parents, mother, father)

# now recode race based on mother and fathers race 

subset <- recode_race_function(subset, F)

summary(as.factor(subset$racefinal))

subset$racefinal <- ifelse(is.na(subset$racefinal), subset$racenew,
                           subset$racefinal)

subset <- subset %>% group_by(uniqueID) %>% fill(racefinal, .direction=c("downup"))

subset$racefinal <- ifelse(is.na(subset$racefinal) & subset$relationshiptohead=="childofhead",
                           subset$raceethhead,
                           ifelse(is.na(subset$racefinal) & subset$relationshiptohead=="childofpartner",
                                  subset$raceethwife, subset$racefinal))

subset <- subset %>% 
  select(uniqueID, year, sex, age, highestEd, educationCAT, educationCATdetailed, weight, racefinal) %>% 
  fill(weight, .direction=c("downup")) %>% 
  fill(highestEd, .direction=c("downup")) %>% mutate(weight=mean(weight))

summary(subset)
write.csv(subset, "SIMAH_workplace/education_transitions/alldata_2019.csv", row.names=F)






