# script to extract descriptive statistics for education transitions paper

setwd("~/Google Drive/SIMAH Sheffield")
library(dplyr)
library(tidyr)
library(mice)

education <- read.csv("SIMAH_workplace/education_transitions/alldata_2019_parentED_final_income.csv") %>% 
  group_by(uniqueID) %>% fill(c(mother_num, father_num), .direction="downup") %>% 
  mutate(mother_num2=max(mother_num),
         father_num2=max(father_num))

summary(education)

toimpute <- education %>% dplyr::select(uniqueID, year, relationshiptohead, sex, age, highestEd, 
                                        racefinal, father_num2, mother_num2, family_income) %>% 
  mutate_at(c("relationshiptohead","sex", "racefinal","father_num2","mother_num2"), as.factor) %>% 
  mutate(family_income = ifelse(family_income<0, 0, family_income)) %>% 
  mutate(relationshiptohead=ifelse(relationshiptohead=="head", "head",
                                   ifelse(relationshiptohead=="wife/partner","wife/partner","other"))) %>% 
  group_by(uniqueID) %>% 
  mutate(max_ED = max(highestEd, na.rm=T),
            max_income = max(family_income, na.rm=T),
         max_age = max(age)) %>% 
  dplyr::select(uniqueID, sex, max_age, racefinal, father_num2, mother_num2, max_ED, max_income) %>%
  distinct()

test <- toimpute %>% group_by(uniqueID) %>% tally()
IDS <- subset(test, n>1)$uniqueID

number_function <- function(data){
  data$number <- 1:nrow(data)
  return(data)
}

duplicate <- toimpute[toimpute$uniqueID %in% IDS,] %>% 
  group_by(uniqueID) %>% group_modify(~number_function(.)) %>% 
  add_tally() %>% 
  mutate(number = paste0("number",number)) %>% 
  pivot_wider(names_from=number, values_from=racefinal)
duplicate <- recode_race_duplicate(duplicate)
duplicate <- duplicate %>% dplyr::select(uniqueID, sex, newrace)

toimpute <- left_join(toimpute, duplicate) %>% group_by(uniqueID) %>% add_tally() %>% 
  mutate(racefinal = as.character(racefinal))
toimpute$newracefinal <- ifelse(toimpute$n>1, toimpute$newrace, toimpute$racefinal)

toimpute <- toimpute %>% dplyr::select(-c(racefinal, newrace, n)) %>% distinct() %>% 
  rename(racefinal=newracefinal) %>% group_by(uniqueID) %>% 
  add_tally() %>% mutate(max_ED = ifelse(max_ED==-Inf, NA, max_ED),
                         max_income = ifelse(max_income==-Inf, NA, max_income),
                         racefinal = as.factor(racefinal))

library(mice)  
toimpute$n <- NULL
predictorMatrix = quickpred(toimpute)
predictorMatrix[,1] <- 0

toimpute <- mice(toimpute, m=20)

complete <- complete(toimpute, "long")

summary(complete)
test <- complete %>% dplyr::select(.imp, .id, mother_num, father_num) %>% distinct()
saveRDS(complete, "SIMAH_workplace/education_transitions/imputed_parental_education.RDS")

# join imputed data back up with the old data 
tojoin <- complete %>% dplyr::select(.imp, uniqueID, father_num2, mother_num2, racefinal) %>% 
  rename(imp = .imp) %>% 
  pivot_wider(names_from=imp, values_from=racefinal)



summary(education$mother_num)
summary(complete$mother_num)

education$father_num <- complete$father_num
education$mother_num <- complete$mother_num

education <- education %>% filter(age<=34)

education <- education %>% filter(weight!=0)
length(unique(education$uniqueID))

summary(as.factor(education$year))

education <- education %>% 
  mutate(parental_education_sum = sum(mother_num, father_num, na.rm=T),
        bothLEHS = ifelse(parental_education_sum == 0, NA,
                          ifelse(parental_education_sum==1 | parental_education_sum==2, 1,0)),
        oneCollegeplus = ifelse(father_num==3 | mother_num==3, 1,
                                ifelse(parental_education_sum==0, NA, 0)),
        bothCollegeplus = ifelse(parental_education_sum==0, NA,
                                 ifelse(parental_education_sum==6, 1, 0)),
        maxBoth = pmax(mother_num, father_num, na.rm=T))

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
neweducation <- neweducation %>% 
  dplyr::select(uniqueID, year, relationshiptohead, sex, age, 
                highestEd, educationCAT, racefinal,
                father_num, mother_num, bothLEHS,
                oneCollegeplus, bothCollegeplus, maxBoth,
                family_income)

write.csv(neweducation, "SIMAH_workplace/education_transitions/PSID_reweighted_2019_weight_parental_income.csv", row.names=F)                                                                                        
