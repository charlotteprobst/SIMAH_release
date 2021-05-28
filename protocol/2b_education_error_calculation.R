# read in the summarised data - comparison of microsimulation and death rates data
df <- read.csv("SIMAH_workplace/protocol/output_data/microsim_education_summary.csv") %>% 
  mutate(sex = microsim.init.sex,
    edclass = factor(microsim.init.education, levels=c("LEHS","SomeC","College")),
         race = recode(microsim.init.race, "BLA"="Black","WHI"="White","SPA"="Hispanic","OTH"="Other"))

# first without race breakdown 
summary <- df %>% group_by(sex, edclass, datatype, year) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% 
  group_by(sex, datatype, year) %>% 
  mutate(percent=n/sum(n)) %>% select(-n) %>% 
  pivot_wider(names_from=datatype, values_from=percent) %>% 
  mutate(differenceACS = abs(microsim-ACS)*100,
         differenceCensus = abs(microsim-Census)*100,
         differencePSID = abs(microsim-PSID)*100) %>% 
  group_by(sex, edclass) %>% 
  summarise(meanACS=round(mean(differenceACS, na.rm=T),digits=2),
            sdACS = round(sd(differenceACS, na.rm=T),digits=2),
            meanCensus=round(mean(differenceCensus, na.rm=T),digits=2),
            sdCensus=round(sd(differenceCensus, na.rm=T), digits=2),
            meanPSID=round(mean(differencePSID, na.rm=T),digits=2),
            sdPSID = round(sd(differencePSID, na.rm=T),digits=2)) %>% 
  pivot_longer(cols=c(meanACS:sdPSID)) %>% 
  mutate(type=ifelse(grepl("mean", name), "Mean","SD"),
         name=gsub("mean","",name),
         name=gsub("sd","",name)) %>% 
  pivot_wider(names_from=type, values_from=value) %>% 
  pivot_wider(names_from=sex, values_from=c(Mean,SD)) %>% 
  select(edclass, name, Mean_Male, SD_Male, Mean_Female, SD_Female) %>% 
  mutate(name=factor(name, levels=c("Census","ACS","PSID"))) %>% group_by(edclass) %>% 
  arrange(name, .by_group=T)

write.csv(summary, "SIMAH_workplace/protocol/output_data/SuppTable2p1.csv", row.names=F)

# with race breakdown
summary <- df %>% group_by(microsim.init.race, edclass, datatype, year) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% 
  group_by(microsim.init.race, datatype, year) %>% drop_na() %>%
  mutate(percent=n/sum(n)) %>% select(-n) %>% 
  pivot_wider(names_from=datatype, values_from=percent) %>% 
  mutate(differenceACS = abs((microsim-ACS)/microsim)*100,
         differenceCensus = abs((microsim-Census)/microsim)*100,
         differencePSID = abs((microsim-PSID)/microsim)*100) %>% 
  group_by(microsim.init.race, edclass) %>% 
  summarise(meanACS=round(mean(differenceACS, na.rm=T),digits=2),
            sdACS = round(sd(differenceACS, na.rm=T),digits=2),
            meanCensus=round(mean(differenceCensus, na.rm=T),digits=2),
            sdCensus=round(sd(differenceCensus, na.rm=T), digits=2),
            meanPSID=round(mean(differencePSID, na.rm=T),digits=2),
            sdPSID = round(sd(differencePSID, na.rm=T),digits=2)) %>% 
  pivot_longer(cols=c(meanACS:sdPSID)) %>% 
  mutate(type=ifelse(grepl("mean", name), "Mean","SD"),
         name=gsub("mean","",name),
         name=gsub("sd","",name)) %>% 
  pivot_wider(names_from=type, values_from=value) %>% 
  pivot_wider(names_from=microsim.init.race, values_from=c(Mean,SD)) %>% 
  select(edclass, name, Mean_BLA, SD_BLA, Mean_WHI, SD_WHI, Mean_SPA, SD_SPA, Mean_OTH, SD_OTH) %>% 
  mutate(name=factor(name, levels=c("Census","ACS","PSID"))) %>% group_by(edclass) %>% 
  arrange(name, .by_group=T)

write.csv(summary, "SIMAH_workplace/protocol/output_data/SuppTable2p2.csv", row.names=F)

