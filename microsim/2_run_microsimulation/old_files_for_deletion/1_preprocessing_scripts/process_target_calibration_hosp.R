if(mortality==1){
cirrhosismortality <- read.csv("SIMAH_workplace/microsim/1_input_data/LC_deaths_CDC_2.csv")[c(1:3,35:48)]
cirrhosismortality <- cirrhosismortality %>% mutate(CDC..80 = CDC..80/5,
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
         cat=paste(agegroup, sex, sep="_")) %>% dplyr::select(Year, cat, sex, agegroup, count) %>% group_by(Year,sex,agegroup) %>% 
  summarise(count=sum(count)*100)
target <- cirrhosismortality
}else if(mortality==0){
  cirrhosismorbidity <- read.csv("SIMAH_workplace/microsim/1_input_data/LC_hosp_Output.csv") %>% 
    dplyr::select(c(year,location,sex_id,tups15to19:tupsplus80,tups15to19_U:tupsplus80_L)) %>% 
    filter(location==SelectedState) %>% 
    mutate(tupsplus80 = tupsplus80/10,
           tupsplus80_U = tupsplus80_U/10,
           tupsplus80_L = tupsplus80_L/10) %>% 
    pivot_longer(cols=tups15to19:tupsplus80_L) %>% 
    mutate(name=gsub("tups","",name),
           type = ifelse(grepl("L",name), "Lower",
                         ifelse(grepl("U",name), "Upper","PE")),
           name = gsub("_U","",name),
           name = gsub("_L", "",name),
           agecat = ifelse(name=="15to19","15-19",
                           ifelse(name=="20to24","20-24",
                                  ifelse(name=="25to29"|name=="30to34","25-34",
                                         ifelse(name=="35to39"|name=="40to44","35-44",
                                                ifelse(name=="45to49"|name=="50to54","45-54",
                                                       ifelse(name=="55to59"|name=="60to64","55-64",
                                                              ifelse(name=="65to69"|name=="70to74","65-74","75.")))))))) %>%
    pivot_wider(names_from=type, values_from=value) %>% filter(name!="_value") %>% 
    group_by(year, sex_id, agecat) %>% summarise(PE=sum(PE),
                                                 Lower = sum(Lower),
                                                 Upper = sum(Upper)) %>% 
    mutate(PE=PE*proportion,
           PE=ifelse(agecat=="15-19",PE/5*2,PE),
           Lower = Lower*proportion,
           Lower=ifelse(agecat=="15-19",Lower/5*2, Lower),
           Upper = Upper*proportion,
           Upper = ifelse(agecat=="15-19", Upper/5*2, Upper),
           sex = ifelse(sex_id=="female","f","m"),
           cat=paste(agecat, sex, sep="_")) %>% rename(Year=year, agegroup=agecat) %>% ungroup() %>% 
    dplyr::select(Year, cat, sex, agegroup, PE, Lower,Upper) %>% group_by(Year,sex) %>% 
    summarise(PE=sum(PE)*100,
              Lower=sum(Lower)*100,
              Upper=sum(Upper)*100)
  target <- cirrhosismorbidity
}
# variance <- read.csv("input_data/seed_variance.csv")
# target <- left_join(target,variance) %>% group_by(sex,agegroup)
# target <- as.numeric(cirrhosismortality$count)[1:27]
# rm(cirrhosismortality)
