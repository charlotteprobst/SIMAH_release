### census data 1990 processing script - 

#### to get population constraints for 12 year olds and for the check 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/SIMAH/Microsimulation"
setwd(paste(WorkingDirectory))
raw <- read.csv("SIMAH_workplace/microsim/census_data/nhgis0050_ds122_1990_state.csv")

##removing columns 
raw <- raw[-c(1:15, 17:19)]

ages <- c(0:102)
races <- c("all races","Hispanic",
           "White", "Black",
           "AmIn","Asian", "Other")
sex <- c("male","female")

cats <- vector()
for(i in races){
  for(j in sex){
    for(k in ages){
      cats[paste(i,j,k)] <- paste(i,j,k, sep="_")
    }
  }
}

names(raw)[2:1443] <- cats

raw <- raw %>% pivot_longer(cols=`all races_male_0`:Other_female_102) %>% 
  separate(name, into=c("race","sex","age"), sep="_") %>% 
  mutate(age= as.integer(as.character(age))) 

USA <- raw %>% group_by(sex, age,race) %>% summarise(value=sum(value)) %>% 
  mutate(STATE="USA")

raw <- rbind(raw, USA)

cons <- raw %>% filter(race!="all races") %>% 
  mutate(racenew=ifelse(race=="Black","BLA",
                     ifelse(race=="White", "WHI",
                            ifelse(race=="Hispanic", "SPA",
                                   "OTH")))) %>% 
  group_by(STATE,age,race,sex) %>% summarise(value=sum(value)) %>% filter(age>=18 & age<=80) 

cons <- data.frame(cons)

write.csv(cons, "SIMAH_workplace/microsim/census_data/overallcons1990.csv")

eighteen <- raw %>% filter(race!="allraces") %>% 
  mutate(race=ifelse(race=="Black","BLA",
                     ifelse(race=="White", "WHI",
                            ifelse(race=="Hispanic", "SPA",
                                   "OTH")))) %>% 
  group_by(STATE,age,race,sex) %>% summarise(value=sum(value)) %>% filter(age>=9 & age<=24) 

write.csv(eighteen, "SIMAH_workplace/microsim/census_data/18yearoldcons1990.csv", row.names=F)
