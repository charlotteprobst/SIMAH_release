Cirrhosis <- do.call(rbind,Cirrhosis)

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
  dplyr::select(Year, cat, sex, agegroup, PE, Lower,Upper) %>% group_by(Year,sex,agegroup) %>% 
  summarise(PE=sum(PE),
            Lower=sum(Lower),
            Upper=sum(Upper)) %>% rename(microsim.init.sex=sex)

forplot <- cirrhosismorbidity %>% 
  mutate(microsim.init.sex=ifelse(microsim.init.sex=="f","Women","Men"),
         agegroup = ifelse(agegroup=="15-19","18-24",
                           ifelse(agegroup=="20-24","18-24", 
                                  ifelse(agegroup=="75.","75-79", agegroup)))) %>% 
  group_by(Year, microsim.init.sex, agegroup) %>% summarise(
    PE=sum(PE)*(1/proportion),
    Lower=sum(Lower)*(1/proportion),
    Upper=sum(Upper)*(1/proportion))

ggplot(data=forplot, aes(x=Year, y=PE)) + geom_line(size=0.5) +
  facet_grid(cols=vars(agegroup),rows=vars(microsim.init.sex)) +
  theme_bw() + 
  theme(strip.background = element_rect(fill="white"),
                     text = element_text(size=20)) +
  geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.3) +
  ylab("Total N Liver Cirrhosis Hospitalizations")
ggsave("SIMAH_workplace/microsim/2_output_data/plots/LC_morbidity_by_age.png", dpi=300,
       width=33, height=19, units="cm")

compare <- left_join(Cirrhosis, cirrhosismorbidity)
compare$microsim.init.sex <- ifelse(compare$microsim.init.sex=="f","female","male")

ggplot(data=compare, aes(x=Year, y=n, colour=as.factor(samplenum))) + geom_line() + 
  facet_grid(cols=vars(agegroup),rows=vars(microsim.init.sex)) + 
  geom_line(aes(x=Year, y=PE),colour="black") + 
  geom_ribbon(aes(x=Year, ymin=Lower, ymax=Upper), colour=NA, alpha=0.4) + theme_bw()

ggsave("SIMAH_workplace/microsim/2_output_data/plots/morbidity_PE.png",
       dpi=300, width=33, height=19, units="cm")


