# script to extract descriptive statistics for education transitions paper

setwd("~/Google Drive/SIMAH Sheffield")
library(dplyr)
library(tidyr)
library(mice)

education <- read.csv("SIMAH_workplace/education_transitions/alldata_2019.csv")
education <- read.csv("SIMAH_workplace/education_transitions/alldata_2019_parentED_final_income.csv")

summary(education)

education$birthyear <- education$year - education$age

education <- education %>% filter(age<=34)

education <- education %>% filter(weight!=0)
length(unique(education$uniqueID))

summary(as.factor(education$year))

# how many people are missing both race AND education
educationmissing <- education[is.na(education$highestEd),]
length(unique(educationmissing$uniqueID))

racemissing <- education[is.na(education$racefinal),]
length(unique(racemissing$uniqueID))

weightmissing <- education[is.na(education$weight),]
length(unique(weightmissing$uniqueID))

education$both <- ifelse(is.na(education$racefinal) & is.na(education$educationCAT), 1,0)
both <- education %>% filter(both==1)
length(unique(both$uniqueID))

firsttime <- education %>% filter(year<=2009)
length(unique(firsttime$uniqueID))

firsttime <- firsttime %>% group_by(uniqueID) %>% 
  mutate(lastobs = ifelse(year==max(year), 1,0),
         educLAST = ifelse(lastobs==1, educationCAT, NA)) %>% 
  fill(educLAST, .direction="downup")

educationmissing <- firsttime[is.na(firsttime$educationCAT),]
length(unique(educationmissing$uniqueID))


racemissing <- firsttime[is.na(firsttime$racefinal),]
length(unique(racemissing$uniqueID))

firsttime <- firsttime %>% drop_na(educationCAT, racefinal,weight)
length(unique(firsttime$uniqueID))

library(splitstackshape)
firsttime <- expandRows(firsttime, "weight")

mean(firsttime$age)
sd(firsttime$age)

levels(as.factor(firsttime$racefinal))

firsttime %>% ungroup() %>% mutate(sex=ifelse(sex=="female",1,0),
                                   raceblack = ifelse(racefinal=="black",1,0),
                                   racewhite = ifelse(racefinal=="white",1,0),
                                   racehispanic = ifelse(racefinal=="hispanic",1,0),
                                   raceNative = ifelse(racefinal=="Native",1,0),
                                   raceother = ifelse(racefinal=="other",1,0),
                                   raceasian = ifelse(racefinal=="Asian/PI",1,0),
                                   lehs = ifelse(educLAST=="LEHS",1,0),
                                   somec = ifelse(educLAST=="SomeC",1,0),
                                   college = ifelse(educLAST=="College",1,0)) %>% summarise(mean=mean(sex),
                                                                                             black=mean(raceblack)*100,
                                                                                             white=mean(racewhite)*100,
                                                                                             hispanic=mean(racehispanic)*100,
                                                                                             native = mean(raceNative)*100,
                                                                                             other=mean(raceother)*100,
                                                                                             asian=mean(raceasian)*100,
                                                                                             lehs = mean(lehs)*100,
                                                                                             somec = mean(somec)*100,
                                                                                             college = mean(college)*100
                                                                                             )

secondtime <- education %>% filter(year>=2010) %>% drop_na(racefinal,educationCAT,
                                                           weight)
length(unique(secondtime$uniqueID))


secondtime <- secondtime %>% group_by(uniqueID) %>% 
  mutate(lastobs = ifelse(year==max(year), 1,0),
         educLAST = ifelse(lastobs==1, educationCAT, NA)) %>% 
  fill(educLAST, .direction="downup")

educationmissing <- secondtime[is.na(secondtime$educationCAT),]
length(unique(educationmissing$uniqueID))

racemissing <- secondtime[is.na(secondtime$racefinal),]
length(unique(racemissing$uniqueID))

secondtime <- secondtime %>% drop_na(educationCAT, racefinal,weight)
length(unique(secondtime$uniqueID))

library(splitstackshape)
secondtime <- expandRows(secondtime, "weight")

levels(as.factor(secondtime$racefinal))
mean(secondtime$age)
sd(secondtime$age)
secondtime %>% ungroup() %>% mutate(sex=ifelse(sex=="female",1,0),
                                   raceblack = ifelse(racefinal=="black",1,0),
                                   racewhite = ifelse(racefinal=="white",1,0),
                                   racehispanic = ifelse(racefinal=="hispanic",1,0),
                                   raceNative = ifelse(racefinal=="Native",1,0),
                                   raceother = ifelse(racefinal=="other",1,0),
                                   raceasian = ifelse(racefinal=="Asian/PI",1,0),
                                   lehs = ifelse(educLAST=="LEHS",1,0),
                                   somec = ifelse(educLAST=="SomeC",1,0),
                                   college = ifelse(educLAST=="College",1,0)) %>% summarise(mean=mean(sex),
                                                                                                black=mean(raceblack)*100,
                                                                                                white=mean(racewhite)*100,
                                                                                                hispanic=mean(racehispanic)*100,
                                                                                                native = mean(raceNative)*100,
                                                                                                other=mean(raceother)*100,
                                                                                                asian=mean(raceasian)*100,
                                                                                                lehs = mean(lehs)*100,
                                                                                                somec = mean(somec)*100,
                                                                                                college = mean(college)*100
                                   )

education <- education %>% drop_na(educationCAT, racefinal,weight)
length(unique(education$uniqueID))

education[is.na(education$highestEd),]

library(splitstackshape)

education$newweight <- round(education$weight*10)

neweducation <- expandRows(education, "weight")
write.csv(neweducation, "SIMAH_workplace/education_transitions/PSID_reweighted_2019_weight_parental_income.csv", row.names=F)                                                                                        
