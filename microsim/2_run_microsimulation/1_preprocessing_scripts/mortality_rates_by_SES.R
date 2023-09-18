# all cause mortality by SES  

deathcounts <- read.csv(paste0(DataDirectory,"allethn_sumCOD_0020_SIMAH.csv")) %>%
  mutate(Sex=recode(sex, "1"="m","2"="f"),
         agecat = recode(age_gp, "18"="18-24","25"="25-29",
                         "30"="30-34","35"="35-39","40"="40-44",
                         "45"="45-49","50"="50-54","55"="55-59",
                         "60"="60-64","65"="65-69","70"="70-74","75"="75-79","80"="80+"),
         Race = recode(race, "White"="WHI","Black"="BLA","Hispanic"="SPA","Other"="OTH"),
         edclass = recode(edclass, "4+yrs"="College")) %>%
  mutate(cat=paste(Sex,agecat,Race,edclass, sep="")) %>%
  dplyr::select(year,Sex, agecat, Race, edclass, LVDCmort, HLVDCmort, DMmort,
                IHDmort, ISTRmort, HYPHDmort, AUDmort, UIJmort, MVACCmort, IJmort, RESTmort) %>% 
  mutate(Race = recode(Race, "WHI"="White","BLA"="Black","SPA"="Hispanic","OTH"="Other")) %>% 
  group_by(year, Sex, agecat, Race, edclass) %>% 
  summarise(allcausemort = sum(across(LVDCmort:RESTmort)))

popcounts <- read.csv("SIMAH_workplace/ACS/ACS_popcounts_2000_2021_bystate_age_gp.csv") %>% 
  filter(state=="USA") %>% 
  group_by(year, sex, race, age_gp, edclass) %>% summarise(TPop=sum(TPop)) %>% 
  mutate(sex = recode(sex, "1"="m","2"="f")) %>% 
  rename(Sex=sex, Race=race, agecat=age_gp)

deathcounts <- left_join(deathcounts, popcounts)

agest <- deathcounts %>% filter(year==2010) %>% group_by(Sex, agecat, edclass) %>% 
  summarise(total=sum(TPop)) %>% ungroup() %>% 
  group_by(Sex,edclass) %>% 
  mutate(prop = total/sum(total)) %>% dplyr::select(Sex, agecat, prop)

byed <- deathcounts %>% left_join(.,agest) %>% 
  group_by(year, agecat, Sex, edclass) %>% distinct() %>% 
  summarise(mortalityrate = (sum(allcausemort)/sum(TPop))*100000,
            weightedmortality = mortalityrate*prop) %>% 
  ungroup() %>% 
  group_by(year, Sex, edclass) %>% 
  summarise(mortality = sum(weightedmortality))

ggplot(data=byed, aes(x=year, y=mortality)) + geom_line() +
  facet_grid(cols=vars(Sex), rows=vars(edclass))
