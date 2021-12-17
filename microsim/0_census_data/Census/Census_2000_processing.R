### census data 2000 processing script - 
#### to get population constraints for 12 year olds and for the check 
library(dplyr)
library(tidyr)
###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/SIMAH/Microsimulation"
setwd(paste(WorkingDirectory))

raw <- read.csv("SIMAH_workplace/microsim/census_data/nhgis0049_ds149_2000_state.csv")
##removing columns 
raw <- raw[-c(1:4, 6:28)]

ages <- c(0:102)
races <- c("all races","Hispanic",
           "WhiteAlone", "WhiteCombo",
           "BlackAlone","BlackCombo",
           "AmInAlone", "AmInCombo",
           "AsianAlone", "AsianCombo",
           "NativeAlone", "NativeCombo",
           "OtherAlone", "OtherCombo",
           "Twoplus")
sex <- c("male","female")

cats <- vector()
for(i in races){
  for(j in sex){
    for(k in ages){
      cats[paste(i,j,k)] <- paste(i,j,k, sep="_")
    }
  }
}

names(raw)[2:3091] <- cats

raw <- raw %>% pivot_longer(cols=`all races_male_0`:Twoplus_female_102) %>% 
  separate(name, into=c("race","sex","age"), sep="_") %>% 
  mutate(age= as.integer(as.character(age))) 

USA <- raw %>% group_by(sex, age,race) %>% summarise(value=sum(value)) %>% 
  mutate(STATE="USA")
  
raw <- rbind(raw, USA)

cons <- raw %>% filter(race!="allraces") %>% filter(race=="AmInCombo" | race=="AsianCombo" | race=="BlackCombo" |
                        race=="Hispanic" | race=="NativeCombo" | race=="OtherCombo" |
                        race=="Twoplus" | race=="WhiteCombo") %>% 
  mutate(race=ifelse(race=="BlackCombo","BLA",
                     ifelse(race=="WhiteCombo", "WHI",
                            ifelse(race=="Hispanic", "SPA",
                                   "OTH")))) %>% 
  group_by(STATE,age,race,sex) %>% summarise(value=sum(value)) %>% filter(age>=18 & age<=80) 

cons <- data.frame(cons)

test <- cons %>% mutate(agecat = cut(age,
                                    breaks=c(0,14,19,24,34,44,54,59,64,74,84,102))) %>% 
  group_by(agecat) %>% filter(STATE=="Alabama") %>% summarise(sum(value))

write.csv(cons, "SIMAH_workplace/microsim/census_data/overallcons2000.csv")

# now get 18 year olds
# individuals aged 9-17 in the year 2000 (would be 18 in 2001 - 2009)
eighteen <- raw %>% filter(race!="allraces") %>% filter(race=="AmInCombo" | race=="AsianCombo" | race=="BlackCombo" |
                             race=="Hispanic" | race=="NativeCombo" | race=="OtherCombo" |
                             race=="Twoplus" | race=="WhiteCombo") %>% 
  mutate(race=ifelse(race=="BlackCombo","BLA",
                     ifelse(race=="WhiteCombo", "WHI",
                            ifelse(race=="Hispanic", "SPA",
                                   "OTH")))) %>% 
  group_by(STATE,age,race,sex) %>% summarise(value=sum(value)) %>% filter(age>=9 & age<=17) 

write.csv(eighteen, "SIMAH_workplace/microsim/census_data/18yearoldcons2000.csv", row.names=F)
