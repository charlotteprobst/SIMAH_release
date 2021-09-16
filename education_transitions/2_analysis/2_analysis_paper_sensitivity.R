# MSM model for education 
setwd("~/Desktop/repos/SIMAH/Data/Education_transitions/2_analysis")
gc()
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
# data <- read_csv("PSID_reweighted_2019.csv")
# # #
# # # # function for assigning IDs - for each replication of an individual append a number to the end of the original ID
# IDfunction <- function(data){
#   n <- nrow(data)
#   data$ID <- 1:n
#   data$newID <- as.numeric(paste(data$uniqueID2, data$ID, sep="."))
#   data$newID <- data$newID*100
#   return(data)
# }
# # 
# data <- data %>% filter(age<=34) %>% filter(age>=18)
# # # apply the ID function to each original individual
# data <- data %>% mutate(uniqueID2 = uniqueID) %>% group_by(uniqueID, year,) %>%
#   group_modify(~IDfunction(.))
# # 
# # data$newID <- as.numeric(data$newID)
# # # # # 
# # # # # # check that there are no duplicate newIDs for different original IDs
# # # # # test <- data %>% group_by(newID) %>% summarise(min=min(uniqueID), max=max(uniqueID),
# # # # #                                                difference = ifelse(min==max, 0, 1))
# # # # # 
# # # # # summary(test$difference)
# # # # # rm(test)
# # # # # # no there aren't - each newID is based on on a single original ID
# # # # # 
# # # # # # save copy of the data
# # write.csv(data, "reweighted_PSID_2019.csv", row.names=F)

data <- read_csv("reweighted_PSID_2019.csv")

summary(data$highestEd)
# states need to be in numeric format for MSM 
data$educNUM <- ifelse(data$highestEd<=12, 1,
                       ifelse(data$highestEd==13, 2,
                              ifelse(data$highestEd==14,3,
                                     ifelse(data$highestEd==15,4,
                                            ifelse(data$highestEd>=16,6,NA)))))
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

# dummy variable for years
data$timevary <- cut(data$year,
                     breaks=c(0,2009,2020),
                     labels=c("1999-2009","2011-2019"))

data$timevary <- as.factor(data$timevary)

# data$racefinal <- ifelse(data$racefinal=="Asian/PI","other",
#                          ifelse(data$racefinal=="Native","other",data$racefinal))
# data$racefinal <- ifelse(data$racefinal=="Native","other",data$racefinal)
data$racefinal <- as.factor(data$racefinal)
summary(data$racefinal)
data$racefinal <- relevel(data$racefinal, ref="white")

# highest observation for each individual 
data <- data %>% group_by(newID) %>% mutate(finalobs=ifelse(year==max(year),highestEd, NA),
                                            finalobs = ifelse(finalobs<=12, 1,
                                                              ifelse(finalobs>12 & finalobs<16, 4,
                                                                     ifelse(finalobs>=16, 5, NA)))) %>% 
  mutate(educNUM = ifelse(year==max(year), finalobs,
                          ifelse(highestEd<=12, 1,
                                 ifelse(highestEd==13, 2,
                                        ifelse(highestEd==14, 3,
                                               ifelse(highestEd==15,4,
                                                      ifelse(highestEd>=16, 5, NA)))))))

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

# how many people in the data actually go backwards
source("cleaning_education_function.R")
# 
backIDs <- getIDs(data)
data <- data[!data$newID %in% backIDs,]
summary(data)
# now get new data for each of those IDs for education over time
data$agesq <- data$age^2
data$age3 <- -data$age^3
data1 <- data %>% filter(timevary=="1999-2009")
length(unique(data1$newID))
length(unique(data1$uniqueID))
data1 <- data1 %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1)
# all data needs to be ordered by ID then year
data1 <- as.data.frame(lapply(data1, unlist))
data1 <- data1[order(data1$newID, data1$year),]
data %>% group_by(year) %>% tally()

Q <- crudeinits.msm(educNUM~year, newID, data=data1, qmatrix=Q)

# constraints for the effects of covariates
# model 1
educ.msm1 <- msm(educNUM~year, newID, data=data1, qmatrix=Q,
                 center=FALSE,
                 covariates=~sex+racefinal+age,
                 control=list(trace=1, fnscale=371824, maxit=500))
educ.msm1 

educ.msm2 <- msm(educNUM~year, newID, data=data1, qmatrix=Q,
                 center=FALSE,
                 covariates=~age+sex*racefinal,
                 control=list(trace=1, maxit=500, fnscale=371824))

educ.msm2

data2 <- data %>% filter(timevary=="2011-2019")
data2 <- data2 %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1)
# all data needs to be ordered by ID then year
data2 <- as.data.frame(lapply(data2, unlist))
data2 <- data2[order(data2$newID, data2$year),]
data2$agesq <- -data2$age^2
Q <- crudeinits.msm(educNUM~year, newID, data=data2, qmatrix=Q)

educ.msm3 <- msm(educNUM~year, newID, data=data2, qmatrix=Q,
                 center=FALSE,
                 covariates=~sex+racefinal+age+age^2,
                 control=list(trace=1, maxit=200, fnscale=15437))
educ.msm3

educ.msm4 <- msm(educNUM~year, newID, data=data2, qmatrix=Q,
                 center=FALSE,
                 covariates=~sex+racefinal+age+age^2+sex*racefinal,
                 control=list(trace=1, maxit=200, fnscale=15437))
educ.msm4

AIC(educ.msm1, educ.msm2, educ.msm3, educ.msm4)
source("extractdata.R")
model1 <- extractdata(educ.msm1) %>% mutate(time = "1999-2009", model="sexraceage", num=1)
model2 <- extractdata(educ.msm2) %>% mutate(time = "1999-2009", model="sex*raceage", num=2)
model3 <- extractdata(educ.msm3) %>% mutate(time = "2011-2019", model="sexraceage", num=1)
model4 <- extractdata(educ.msm4) %>% mutate(time = "2011-2019", model="sex*raceage", num=2)

saveRDS(educ.msm1, "model1_new_sensitivity.RDS")
saveRDS(educ.msm2, "model2_new_sensitivity.RDS")
saveRDS(educ.msm3, "model3_new_sensitivity.RDS")
saveRDS(educ.msm4, "model4_new_sensitivity.RDS")

# adjust standard errors

# write.csv(model, "msmmodel_output_tunnel_paper.csv", row.names=F)

length(unique(data1$uniqueID))
length(unique(data1$newID))
length(unique(data2$uniqueID))
length(unique(data2$newID))
# 

model <- rbind(model1, model2, model3, model4)
names <- names(model)[2:25]
model <- model %>% mutate_at(names, function(x) round(x, digits=2)) %>% select_if(~ !any(is.na(.))) %>% 
  pivot_longer(cols=`LEHS->SomeC1_Estimate`:`SomeC3->College_Upper`) %>% 
  separate(name, into=c("from","to","type", sep="_")) %>% select_if(~ !any(is.na(.))) %>% 
  filter(Variable!="base") %>% mutate(Transition=paste(from, to, sep="_")) %>% pivot_wider(names_from=type, values_from=value) %>% 
  mutate(SE=(Upper-Estimate)/1.96,
         SD = ifelse(time=="1999-2009",
                     SE*(sqrt(119597)),
                     ifelse(time=="2011-2019",
                            SE*(sqrt(137336)), NA)),
         newSE = ifelse(time=="1999-2009",
                        SD / (sqrt(8665)),
                        ifelse(time=="2011-2019",
                               SD / (sqrt(10491)), NA)),
         newLower = (log(Estimate))-(newSE*1.96),
         newUpper = (log(Estimate))+(newSE*1.96),
         newLower = exp(newLower),
         newUpper = exp(newUpper),
         newLower = round(newLower, digits=2),
         newUpper = round(newUpper, digits=2)) %>% select(-c(Lower, Upper, SE,
                                                         SD, newSE)) %>% 
  mutate(EstimateCI = paste(Estimate, " (", newLower, ",", newUpper, ")", sep="")) %>% select(num, time, Variable, Transition, EstimateCI) %>% 
  pivot_wider(names_from=Transition, values_from=EstimateCI) %>% 
  pivot_wider(names_from=time, values_from=c(LEHS_SomeC1:SomeC3_College)) %>% filter(Variable!="age")

write.csv(model, "msmmodel_output_tunnel_paper_2019.csv", row.names=F)

# sensitivity analyses
# 3 states and Native American
AIC(educ.msm1, educ.msm2, educ.msm3, educ.msm4)

source("extractTP.R")

mapping <- data.frame(age=unique(data$age))
mapping$agesq <- -mapping$age^2
age <- sort(unique(data$age))
sex <- c(0,1)
race <- unique(data$racefinal)

prob1 <- extractTP(educ.msm1, age, sex, race, mapping)
prob1$Time <- "1999-2007"
prob1$Type <- "interaction"
prob2 <- extractTP(educ.msm3, age, sex, race, mapping)
prob2$Time <- "2008-2017"
prob2$Type <- "interaction"

probs <- rbind(prob1, prob2)
probs$agenew <- NULL

write.csv(probs, "TP_nointeraction.csv", row.names=F)

educ.msm1
educ.msm3

model <- educ.msm1
options(scipen=999)
options(digits=2)
q <- qmatrix.msm(educ.msm3)

example <- pmatrix.msm(educ.msm3, ci=c("normal"), covariates=list(agecen=0, sex=1, racefinal="white"))
example$estimates
example$L
example$U


extractbaseline <- function(model, time){
  baseline <- data.frame(unclass(pmatrix.msm(model)))
  baseline$StateFrom <- row.names(baseline)
  baseline <- baseline %>% pivot_longer(cols=State.1:State.5,
                                                                  names_to="StateTo", values_to="prob") %>% 
    mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
                               endsWith(StateTo,"2") ~ "State 2",
                               endsWith(StateTo,"3") ~ "State 3",
                               endsWith(StateTo,"4") ~ "State 4",
                               endsWith(StateTo,"5") ~ "State 5")) %>% 
    filter(StateFrom=="State 1" & StateTo=="State 1"|
           StateFrom=="State 1" & StateTo=="State 2"|
           StateFrom=="State 2" & StateTo=="State 2"|
            StateFrom=="State 2" & StateTo=="State 3"|
             StateFrom=="State 3" & StateTo=="State 3"|
             StateFrom=="State 3" & StateTo=="State 4"|
             StateFrom=="State 4" & StateTo=="State 4"|
             StateFrom=="State 4" & StateTo=="State 5"|
             StateFrom=="State 5" & StateTo=="State5") %>% mutate(period=time)
  return(baseline)
  
}

baseline1 <- extractbaseline(educ.msm1, "1999-2007")
baseline2 <- extractbaseline(educ.msm3, "2008-2017")

baseline <- rbind(baseline1, baseline2)
write.csv(baseline, "baseline_TP.csv", row.names=F)

# probs$racefinal <- recode(probs$racefinal,
#                           "black"="Black",
#                           "hispanic"="Hispanic",
#                           "Native"= "Native American",
#                           "other"="Asian/others",
#                           "white"="White")

probs <- read.csv("TP_tunnel2.csv")
# plot the output
library(ggplot2)
test <- probs %>% filter(age=="21" & sex=="female") %>% filter(Prob!=1 & Prob!=0) %>% drop_na(Transition) %>% 
  filter(Transition=="LEHS->LEHS" | Transition=="LEHS->SomeC1" | Transition=="SomeC1->SomeC1" |
           Transition=="SomeC1->SomeC2" | Transition=="SomeC2->SomeC2" | Transition=="SomeC2->SomeC3" |
           Transition=="SomeC3->SomeC3" | Transition=="SomeC3->College")

test$Transition <- factor(test$Transition,
                          levels=c("LEHS->LEHS","LEHS->SomeC1","SomeC1->SomeC1",
                                     "SomeC1->SomeC2","SomeC2->SomeC2","SomeC2->SomeC3",
                                     "SomeC3->SomeC3","SomeC3->College"))

ggplot(data=test, aes(x=Time, y=Prob, colour=racefinal, fill=racefinal)) + geom_col(position="dodge") + ylim(0,NA) + 
  facet_wrap(~Transition, ncol=4, scales="fixed") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_bw() + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper), position="dodge", colour="black") + theme(legend.title=element_blank(),
                                                                                       legend.position="bottom") + 
  xlab("Year group") + ylab("Transition probability") + ggtitle("age = 21, Female")

ggsave("MSM_age21F.png", dpi=300, height=19, width=33, units="cm")
