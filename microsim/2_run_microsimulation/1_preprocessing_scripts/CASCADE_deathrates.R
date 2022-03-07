# death rates processing script 
#####read in the death rates - raw deaths per year by age and sex 
# read in 1979-1998 death rates first 
deathrates1 <- read.delim("SIMAH_workplace/microsim/1_input_data/Compressed Mortality, 1979-1998 (1).txt")

# 1999 death rates onwards come from different CDC Wonder system so read in separately 
deathrates2 <- read.delim("SIMAH_workplace/microsim/1_input_data/Compressed Mortality, 1999-2016 (1).txt")

setdiff(levels(deathrates1$Age.Group), levels(deathrates2$Age.Group))
setdiff(levels(deathrates2$Age.Group), levels(deathrates1$Age.Group))

deathrates <- rbind(deathrates1, deathrates2)
rm(deathrates1, deathrates2)

deathrates <- deathrates %>% dplyr::select(Year, Gender, Age.Group, State, Deaths) %>% drop_na() %>% 
  mutate(Gender = factor(Gender),
         Age = factor(Age.Group),
         State=factor(State),
         Sex = recode(Gender, "Male"="m", "Female"="f")) %>% dplyr::select(-c(Age.Group, Gender)) %>% filter(Year>=1980)

deathratesUSA <- deathrates %>% group_by(Year,Age,Sex) %>% summarise(Deaths=sum(Deaths))
deathratesUSA$State <- "USA"

deathrates <- rbind(data.frame(deathrates), data.frame(deathratesUSA))
rm(deathratesUSA)

deathrates <- deathrates %>% filter(State==SelectedState) %>% 
  filter(Age!="10-14 years") %>% 
  mutate(Age = gsub("years","",Age),
         Age = gsub(" ","", Age), 
         Deaths = Deaths*proportion,
         Age = ifelse(Age=="75-84", "75.", Age),
         Deaths = ifelse(Age=="15-19", Deaths/5*2,
                         ifelse(Age=="75.", Deaths/10*6, Deaths)),
         cat = paste(Age, Sex, sep="_"),
         Count = Deaths) %>% dplyr::select(Year, cat, Count) %>% filter(Year>=1980)
#adjust for the proportion currently being run in the microsim
# adjust the totals for the age groups that aren't fully in the microsim - 75-84 and 15-19
# assuming equal death rates for each year group 

# READ IN CIRRHOSIS DEATHS - TO WORK REFERENCE RATE AND REMOVE CIRRHOSIS DEATHS FROM THE TOTAL DEATHS
if(cirrhosis==1 & mortality==1){
cirrhosisdata <- read.csv("SIMAH_workplace/microsim/1_input_data/LC_deaths_CDC_2.csv")[c(1:3,35:48)]
cirrhosisdata <- cirrhosisdata %>% mutate(CDC..80 = CDC..80/10,
                                          CDC.75.=CDC.75_79+CDC..80,
                                          CDC.25_34 = CDC.25_29+CDC.30_34,
                                          CDC.35_44 = CDC.35_39+CDC.40_44,
                                          CDC.45_54 = CDC.45_49+CDC.50_54,
                                          CDC.55_64 = CDC.55_59+CDC.60_64,
                                          CDC.65_74 = CDC.65_69+CDC.70_74) %>% dplyr::select(States, Sex, Year, CDC.15_19, CDC.20_24, CDC.25_34,
                                                                                      CDC.35_44, CDC.45_54, CDC.55_64,
                                                                                      CDC.65_74, CDC.75.) %>% 
  pivot_longer(cols=CDC.15_19:CDC.75., names_to="agegroup", values_to="deaths") %>% 
  mutate(agegroup=gsub("CDC.", "", agegroup),
         agegroup=gsub("_","-", agegroup),
         sex=recode(Sex, "1"="m","2"="f"),
         State=recode(States, "USA"="USA",
                      "CA"="California",
                      "MN"="Minnesota",
                      "NY"="New York",
                      "TN"="Tennessee",
                      "TX"="Texas")) %>% filter(State==SelectedState) %>% 
  mutate(Deaths=deaths*proportion,
         count=ifelse(agegroup=="15-19",Deaths/5*2,Deaths),
         cat=paste(agegroup, sex, sep="_")) %>% dplyr::select(Year, cat, count)
  
cirrhosisdeaths1984 <- cirrhosisdata %>% filter(Year==1984)  %>% 
   mutate(count=round(count*100))

# # ADJUST DEATH RATES = total deaths - cirrhosis deaths 
setdiff(names(cirrhosisdata), names(deathrates))
setdiff(levels(as.factor(cirrhosisdata$cat)), levels(as.factor(deathrates$cat)))
deathrates <- left_join(deathrates, cirrhosisdata)
deathrates[is.na(deathrates)] <- 0
deathrates$Count <- deathrates$Count - deathrates$count
deathrates <- deathrates %>% dplyr::select(Year, cat, Count)
# cirrhosisdeaths1984$count <- cirrhosisdeaths1984$count*100
# cirrhosisdeaths1984 <- cirrhosisdeaths1984 %>% separate(cat, into=c("age","cat"), sep="_") %>% 
#   group_by(cat) %>% mutate(count=sum(count))
}else if(cirrhosis==1 & mortality==0){
  cirrhosisdata <- read.csv("SIMAH_workplace/microsim/1_input_data/LC_hosp_Output.csv") %>% 
    dplyr::select(c(year,location,sex_id,tups15to19:tupsplus80)) %>% 
    filter(location==SelectedState) %>% 
    mutate(tupsplus80 = tupsplus80/10) %>% 
    pivot_longer(cols=tups15to19:tupsplus80) %>% 
    mutate(name=gsub("tups","",name),
           agecat = ifelse(name=="15to19","15-19",
                           ifelse(name=="20to24","20-24",
                           ifelse(name=="25to29"|name=="30to34","25-34",
                                  ifelse(name=="35to39"|name=="40to44","35-44",
                                         ifelse(name=="45to49"|name=="50to54","45-54",
                                                ifelse(name=="55to59"|name=="60to64","55-64",
                                                       ifelse(name=="65to69"|name=="70to74","65-74","75.")))))))) %>% 
    group_by(year, sex_id, agecat) %>% summarise(value=sum(value)) %>% 
    mutate(value=value*proportion,
           count=ifelse(agecat=="15-19",value/5*2,value),
           sex = ifelse(sex_id=="female","f","m"),
           cat=paste(agecat, sex, sep="_")) %>% rename(Year=year) %>% ungroup() %>% dplyr::select(Year, cat, count)
  cirrhosisdeaths1984 <- cirrhosisdata %>% filter(Year==1984) %>% 
    mutate(count=round(count*100))
}
