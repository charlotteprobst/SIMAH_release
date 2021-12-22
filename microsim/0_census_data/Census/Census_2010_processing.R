### census data 2000 processing script - 
###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/SIMAH/Microsimulation"
setwd(paste(WorkingDirectory))
#### to get population constraints for 12 year olds and for the check 

raw <- read.csv("SIMAH_workplace/microsim/census_data/nhgis0059_ds181_2010_state.csv")
##removing columns 
raw <- raw[-c(1:4, 6:32)]
library(janitor)
raw <- raw %>% 
  adorn_totals("row")
raw[53,1]
raw[53,1] <- "USA"

raw <- raw %>% select(-contains(c("001","002","106")))

ages <- c(0:102)
races <- c("all races","Hispanic",
           "Not Hispanic",
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

names(raw)[2:3297] <- cats

raw <- raw %>% pivot_longer(cols=`all races_male_0`:Twoplus_female_102) %>% 
  separate(name, into=c("race","sex","age"), sep="_") %>% 
  mutate(age= as.integer(as.character(age)))

cons <- raw %>% filter(race!="allraces") %>% filter(race=="AmInCombo" | race=="AsianCombo" | race=="BlackCombo" |
                                                      race=="Hispanic" | race=="NativeCombo" | race=="OtherCombo" |
                                                      race=="Twoplus" | race=="WhiteCombo") %>% 
  mutate(race=ifelse(race=="BlackCombo","BLA",
                     ifelse(race=="WhiteCombo", "WHI",
                            ifelse(race=="Hispanic", "SPA",
                                   "OTH")))) %>% 
  group_by(STATE,age,race,sex) %>% summarise(value=sum(value)) %>% filter(age>=18 & age<=80) 

cons <- data.frame(cons)

write.csv(cons, "SIMAH_workplace/microsim/census_data/overallcons2000.csv")

# now get 18 year olds
# individuals aged 9-17 in the year 2010 (would be 18 in 2011 - 2019)
eighteen <- raw %>% filter(race!="allraces") %>% filter(race=="AmInCombo" | race=="AsianCombo" | race=="BlackCombo" |
                                                          race=="Hispanic" | race=="NativeCombo" | race=="OtherCombo" |
                                                          race=="Twoplus" | race=="WhiteCombo") %>% 
  mutate(race=ifelse(race=="BlackCombo","BLA",
                     ifelse(race=="WhiteCombo", "WHI",
                            ifelse(race=="Hispanic", "SPA",
                                   "OTH")))) %>% 
  group_by(STATE,age,race,sex) %>% summarise(value=sum(value)) %>% filter(age>=9 & age<=18) 

write.csv(eighteen, "SIMAH_workplace/microsim/census_data/18yearoldcons2010.csv")
