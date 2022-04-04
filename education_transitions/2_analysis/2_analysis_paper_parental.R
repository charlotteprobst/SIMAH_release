# script to generate MSM model for education for education transitions paper
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)

setwd("~/Google Drive/SIMAH Sheffield")

data <- read_csv("SIMAH_workplace/education_transitions/PSID_reweighted_2019_weight_parental.csv")

# # # function for assigning IDs - for each replication of an individual append a number to the end of the original ID
IDfunction <- function(data){
  n <- nrow(data)
  data$ID <- 1:n
  data$newID <- as.numeric(paste(data$uniqueID2, data$ID, sep="."))
  data$newID <- data$newID*100
  return(data)
}
#
data <- data %>% filter(age<=34) %>% filter(age>=18)
# # apply the ID function to each original individual
data <- data %>% mutate(uniqueID2 = uniqueID) %>% group_by(uniqueID, year,) %>%
  group_modify(~IDfunction(.))
#
data$newID <- as.numeric(data$newID)
# # #
# # # # check that there are no duplicate newIDs for different original IDs
test <- data %>% group_by(newID) %>% summarise(min=min(uniqueID), max=max(uniqueID),
                                               difference = ifelse(min==max, 0, 1))
summary(test$difference)
rm(test)
# # # # no there aren't - each newID is based on on a single original ID

# # # # save copy of the reweighted data with the new IDs 
write.csv(data, "SIMAH_workplace/reweighted_PSID_2019_IDs_parental.csv", row.names=F)

#### SCRIPT CAN BE STARTED FROM HERE IF REWEIGHTED DATA WITH IDS EXISTS ####
data <- read_csv("SIMAH_workplace/reweighted_PSID_2019_IDs_parental.csv")

summary(data$highestEd)
# states need to be in numeric format for MSM 
data$educNUM <- ifelse(data$highestEd<=12, 1,
                       ifelse(data$highestEd==13, 2,
                              ifelse(data$highestEd==14,3,
                                     ifelse(data$highestEd==15,4,
                                            ifelse(data$highestEd>=16,5,NA)))))
summary(data$educNUM)

# remove anyone with only one year of data- this gives an error in MSM 
data <- data %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) %>% mutate(sex=factor(sex),
                                         sex=ifelse(sex=="female",1,0))
# all data needs to be ordered by ID then year
data <- as.data.frame(lapply(data, unlist))
data <- data[order(data$newID, data$year),]

# n's per year - to decide on year split 
data %>% group_by(year) %>% tally()
data$racefinal <- ifelse(data$racefinal=="Native","other",
                         ifelse(data$racefinal=="Asian/PI","Asian",data$racefinal))
data$racefinal <- as.factor(data$racefinal)
summary(data$racefinal)
data$racefinal <- relevel(data$racefinal, ref="white")

statetable.msm(educNUM, newID, data)

# Q matrix of allowed instantaneous transitions - only allowed to transition between 1-2 and 2-3 (and staying the same)
Q <- rbind( c(0.5, 0.5, 0, 0, 0),
            c(0, 0.5, 0.5, 0, 0),
            c(0, 0, 0.5, 0.5, 0),
            c(0, 0, 0, 0.5, 0.5),
            c(0, 0, 0, 0, 0.5))

E <- rbind( c(0, 0.1, 0.1, 0.1, 0.1),
            c(0.1, 0, 0.1, 0.1, 0.1),
            c(0.1, 0.1, 0, 0.1, 0.1),
            c(0.1, 0.1, 0.1, 0, 0.1),
            c(0.1, 0.1, 0.1, 0.1, 0))

data <- data %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1)
# all data needs to be ordered by ID then year
data <- as.data.frame(lapply(data, unlist))
data <- data[order(data$newID, data$year),]

data$educNUM <- ifelse(data$age==18 & data$educNUM>2, 1, data$educNUM)

statetable.msm(educNUM, newID, data=data)

data$sex <- as.factor(data$sex)

# who in the data transitions backwards, i.e. education gets lower - not allowed
source("SIMAH_code/education_transitions/2_analysis/cleaning_education_function.R")
backIDs <- getIDs(data)
data <- data[!data$newID %in% backIDs,]
summary(data)

Q <- crudeinits.msm(educNUM~year, newID, data=data, qmatrix=Q)
data$agesq <- data$age^2
data$agescaled <- scale(data$age, center=T)
data$agesqscaled <- scale(data$agesq, center=T)

data <- data %>% rowwise() %>% mutate(parental_education = sum(mother_num, father_num, na.rm=T))

data$parental_education <- ifelse(data$parental_education>=3, 1,
                                         ifelse(data$parental_education==0, NA, 0))

# at least one parent has a college degree 
data$parental_education_college = ifelse(data$mother_num>=3 | data$father_num>=3, 1, 
                                 ifelse(data$parental_education==0, NA, 0))

data1 <- data %>% filter(year<=2009)
length(unique(data1$newID))
length(unique(data1$uniqueID))
data1 <- data1 %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1)
# all data needs to be ordered by ID then year
data1 <- as.data.frame(lapply(data1, unlist))
data1 <- data1[order(data1$newID, data1$year),]

Q <- crudeinits.msm(educNUM~year, newID, data=data1, qmatrix=Q)

educ.msm1 <- msm(educNUM~year, newID, data=data1, qmatrix=Q,
                 center=FALSE,
                 covariates=~parental_education_college,
                 control=list(trace=1, fnscale=761147, maxit=500))
educ.msm1

educ.msm1 <- msm(educNUM~year, newID, data=data1, qmatrix=Q,
                 center=FALSE,
                 covariates=~agescaled + sex + racefinal + parental_education_college,
                 control=list(trace=1, fnscale=60601, maxit=1000))
educ.msm1

pmatrix.msm(educ.msm1, t=1, covariates=list(agescaled = -1.5, sex = "0", racefinal="white", parental_education_college=1))


educ.msm1 <- msm(educNUM~year, newID, data=data1, qmatrix=Q,
                 center=FALSE,
                 covariates=~agescaled + sex + racefinal + parental_education_college*racefinal,
                 control=list(trace=1, fnscale=60601, maxit=1000))
educ.msm1

# constraints for the effects of covariates
# model 1
educ.msm1 <- msm(educNUM~year, newID, data=data1, qmatrix=Q,
                 center=FALSE,
                 covariates=~agescaled+agesqscaled+sex+racefinal+parental_education*racefinal,
                 control=list(trace=1, fnscale=761147, maxit=500))
educ.msm1 

data1$educationMother <- ifelse(data1$mother_num<=2, "noBA",
                                ifelse(data1$mother_num==3, "BA", ))

educ.msm2 <- msm(educNUM~year, newID, data=data1, qmatrix=Q,
                 center=FALSE,
                 covariates=~agescaled + agesqscaled +sex*racefinal+parental_education,
                 control=list(trace=1, maxit=500, fnscale=336947))

educ.msm2

data2 <- data %>% filter(year>=2011)
data2 <- data2 %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1)
# all data needs to be ordered by ID then year
data2 <- as.data.frame(lapply(data2, unlist))
data2 <- data2[order(data2$newID, data2$year),]

Q <- crudeinits.msm(educNUM~year, newID, data=data2, qmatrix=Q)

educ.msmmother <- msm(educNUM~year, newID, data=data2, qmatrix=Q,
                      center=FALSE,
                      covariates=~motherseducation,
                      control=list(trace=1, maxit=200, fnscale=15437))
educ.msmmother

educ.msmfather <- msm(educNUM~year, newID, data=data2, qmatrix=Q,
                      center=FALSE,
                      covariates=~fatherseducation,
                      control=list(trace=1, maxit=200, fnscale=15437))
educ.msmfather


educ.msm3 <- msm(educNUM~year, newID, data=data2, qmatrix=Q,
                 center=FALSE,
                 covariates=~agescaled+agesqscaled+sex+racefinal+parental_education*sex,
                 control=list(trace=1, maxit=200, fnscale=15437))
educ.msm3

educ.msm4 <- msm(educNUM~year, newID, data=data2, qmatrix=Q,
                 center=FALSE,
                 covariates=~agescaled+agesqscaled+sex*racefinal+motherseducation + fatherseducation,
                 control=list(trace=1, maxit=200, fnscale=15437))
educ.msm4

AIC(educ.msm1, educ.msm2, educ.msm3, educ.msm4)

saveRDS(educ.msm1, "SIMAH_workplace/education_transitions/model1_2019_final.RDS")
saveRDS(educ.msm2, "SIMAH_workplace/education_transitions/model2_2019_final.RDS")
saveRDS(educ.msm3, "SIMAH_workplace/education_transitions/model3_2019_final.RDS")
saveRDS(educ.msm4, "SIMAH_workplace/education_transitions/model4_2019_final.RDS")
