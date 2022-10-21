# script to process PSID data for Markov modelling 
library(foreign)
library(dplyr)
library(tidyr)
setwd("~/Google Drive/SIMAH Sheffield")

subset <- read.csv("SIMAH_workplace/education_transitions/alldata_2019_parentED.csv")

data <- read.dbf("SIMAH_workplace/education_transitions/J305156/J305156.dbf")

data$origINTNO <- data$ER30001
data$ID <- data$ER30002
data$uniqueID <- (data$origINTNO*1000) + data$ID

headfathered <- 	c("V318", "V793", "V1484", "V2196", "V2822", "V3240", "V3662", "V4138", "V4681", "V5601", "V6150", "V6747", "V7380",
                   "V8032", "V8656", "V9342", "V10989", "V11922", "V13549", "V14596", "V16070", "V17467", "V18798", "V20098",
                   "V21404", "V23260", "ER3924", "ER6794", "ER9040", "ER11816", "ER15894", "ER19955", "ER23392",  "ER27356",
                   "ER40531", "ER46508", "ER51869", "ER57622", "ER64773", "ER70845",  "ER76860")
headfather <- data %>% select(uniqueID, c(headfathered))
years <- c(1968:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
names(headfather)[2:42] <- years
headfather <- headfather %>% pivot_longer(cols='1968':'2019', names_to="year", values_to="head_fathered") %>% 
  mutate(head_fathered = ifelse(head_fathered<=4, "LEHS",
                                ifelse(head_fathered>4 & head_fathered<=6, "SomeC",
                                       ifelse(head_fathered> 6 & head_fathered<=8, "College", NA)))) %>% 
  group_by(uniqueID) %>% fill(head_fathered, .direction=c("downup"))

headmothered <- 	c("V3634", "V4139", "V4682", "V5602", "V6151", "V6748", "V7381",  "V8033", "V8657", "V9343", "V10990", "V11923",
                   "V13550", "V14597", "V16071", "V17468", "V18799", "V20099", "V21405", "V23261", "ER3926", "ER6796", "ER9042",
                   "ER11824", "ER15903", "ER19964", "ER23401", "ER27366", "ER40541", "ER46518", "ER51879", "ER57632", "ER64783",
                   "ER70855", "ER76870")

headmother <- data %>% select(uniqueID, c(headmothered))
years <- c(1974:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
names(headmother)[2:36] <- years
headmother <- headmother %>% pivot_longer(cols='1974':'2019', names_to="year", values_to="head_mothered") %>% 
  mutate(head_mothered = ifelse(head_mothered<=4, "LEHS",
                                ifelse(head_mothered>4 & head_mothered<=6, "SomeC",
                                       ifelse(head_mothered> 6 & head_mothered<=8, "College", NA)))) %>% 
  group_by(uniqueID) %>% fill(head_mothered, .direction=c("downup")) %>% dplyr::select(uniqueID, year, head_mothered)

spousefathered <- 	c("V3608","V4108","V4753","V5572","V6121","V6718","V7351","V8003","V8627",
                     "V9313","V10960","V12277","V13485","V14532","V16006","V17403","V18734","V20034",
                     "V21340","V23197","ER3864","ER6734","ER8980","ER11735","ER15809","ER19870","ER23307",
                     "ER27267", "ER40442", "ER46414", "ER51775", "ER57512", "ER64634", "ER70707","ER76715")

spousefather <- data %>% select(uniqueID, c(spousefathered))

years <- c(1974:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
names(spousefather)[2:36] <- years
spousefather <- spousefather %>% pivot_longer(cols='1974':'2019', names_to="year", values_to="spouse_fathered") %>% 
  mutate(spouse_fathered = ifelse(spouse_fathered<=4, "LEHS",
                                ifelse(spouse_fathered>4 & spouse_fathered<=6, "SomeC",
                                       ifelse(spouse_fathered> 6 & spouse_fathered<=8, "College", NA)))) %>% 
  group_by(uniqueID) %>% fill(spouse_fathered, .direction=c("downup")) %>% dplyr::select(uniqueID, year, spouse_fathered) %>% 
  mutate(maxyear = ifelse(year==max(year),1,0)) %>% filter(maxyear==1) %>% dplyr::select(-c(year,maxyear))

spousemothered <- 	c("V3609","V4109","V4754","V5573","V6122","V6719","V7352","V8004","V8628",
                     "V9314","V10961","V12278","V13486","V14533","V16007","V17404","V18735","V20035",
                     "V21341","V23198","ER3866","ER6736","ER8982","ER11743","ER15818","ER19879","ER23316",
                     "ER27277", "ER40452", "ER46424", "ER51785", "ER57522", "ER64644", "ER70717","ER76725")

spousemother <- data %>% select(uniqueID, c(spousemothered))

years <- c(1974:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
names(spousemother)[2:36] <- years
spousemother <- spousemother %>% pivot_longer(cols='1974':'2019', names_to="year", values_to="spouse_mothered") %>% 
  mutate(spouse_mothered = ifelse(spouse_mothered<=4, "LEHS",
                                  ifelse(spouse_mothered>4 & spouse_mothered<=6, "SomeC",
                                         ifelse(spouse_mothered> 6 & spouse_mothered<=8, "College", NA)))) %>% 
  group_by(uniqueID) %>% fill(spouse_mothered, .direction=c("downup")) %>% 
  mutate(maxyear = ifelse(year==max(year),1,0)) %>% filter(maxyear==1) %>% dplyr::select(-c(year,maxyear))

combined <- left_join(headfather, headmother)
combined <- left_join(combined, spousefather)
combined <- left_join(combined, spousemother)

subset <- left_join(subset, combined)

subset$motherseducation <- ifelse(subset$relationshiptohead=="head", subset$head_mothered,
                                  ifelse(subset$relationshiptohead=="wife/partner", subset$spouse_mothered,NA))
# subset$motherseducation <- ifelse(is.na(subset$motherseducation, subset$edMother, subset$motherseducation))

subset$fatherseducation <- ifelse(subset$relationshiptohead=="head", subset$head_fathered,
                                  ifelse(subset$relationshiptohead=="wife/partner", subset$spouse_fathered, NA))
# subset$fatherseducation <- ifelse(is.na(subset$fatherseducation, subset$edFather, subset$fatherseducation))

subset <- subset %>% 
  mutate(father_num = ifelse(fatherseducation=="LEHS",1,
                             ifelse(fatherseducation=="SomeC",2,
                                    ifelse(fatherseducation=="College",3,NA))),
         mother_num = ifelse(motherseducation=="LEHS",1,
                             ifelse(motherseducation=="SomeC",2,
                                    ifelse(motherseducation=="College",3,NA)))) %>% 
  group_by(uniqueID, year) %>% 
  mutate(parental_education_sum = sum(mother_num, father_num, na.rm=T),
         bothLEHS = ifelse(parental_education_sum == 0, NA,
                           ifelse(parental_education_sum==1 | parental_education_sum==2, 1,0)),
         oneCollegeplus = ifelse(father_num==3 | mother_num==3, 1,
                                 ifelse(parental_education_sum==0, NA, 0)),
         bothCollegeplus = ifelse(parental_education_sum==0, NA,
                                  ifelse(parental_education_sum==6, 1, 0)),
         maxBoth = pmax(mother_num, father_num, na.rm=T))

subset <- write.csv(subset,"SIMAH_workplace/education_transitions/alldata_2019_parentED_final.csv")
