popcounts <- read.csv("SIMAH_workplace/microsim/census_data/ACS_population_constraints_educ.csv") %>% 
  mutate(microsim.init.race = case_when(microsim.init.race=="WHI" ~ "White",
                                        microsim.init.race=="BLA" ~ "Black",
                                        microsim.init.race=="SPA" ~ "Hispanic",
                                        microsim.init.race=="OTH" ~ "Others"),
         agecat = case_when(agecat=="18" | agecat=="19-24" ~ "18",
                            agecat=="25-29" ~ "25",
                            agecat=="30-34" ~ "30",
                            agecat=="35-39" ~ "35",
                            agecat=="40-44" ~ "40",
                            agecat=="45-49" ~ "45",
                            agecat=="50-54" ~ "50",
                            agecat=="55-59" ~ "55",
                            agecat=="60-64" ~ "60",
                            agecat=="65-69" ~ "65",
                            agecat=="70-74" ~ "70",
                            agecat=="75-79" ~ "75")) %>% 
  group_by(Year, agecat, microsim.init.sex,microsim.init.race, EDUC) %>% 
  summarise(TotalPop = sum(TotalPop))

mort <- read.csv("SIMAH_workplace/microsim/1_input_data/allethn_sumCOD_0022_SIMAH.csv") %>% 
  rename(EDUC=edclass, microsim.init.sex=sex, Year=year, agecat=age_gp) %>% 
  mutate(microsim.init.race = case_when(race=="1-WNH" ~ "White",
                                        race=="2-BNH" ~ "Black",
                                        race=="3-Hisp" ~ "Hispanic",
                                        race=="4-Others" ~ "Others"),
         
         microsim.init.sex = case_when(microsim.init.sex==1 ~ "m",
                                       microsim.init.sex==2 ~ "f"),
         agecat = as.character(as.numeric(agecat))) %>% 
  dplyr::select(Year, agecat, microsim.init.sex, EDUC, microsim.init.race, LVDCmort, 
                AUDmort, UIJmort, IJmort)


mort <- left_join(mort, popcounts)

mort <- mort %>% 
  mutate(agecat=case_when(agecat=="18" ~ "18",
                          agecat=="25" | agecat=="30" ~ "25",
                          agecat=="35" | agecat=="40" ~ "35",
                          agecat=="45" | agecat=="50" ~ "45",
                          agecat=="55" | agecat=="60" ~ "55",
                          agecat=="65" | agecat=="70" | agecat=="75" ~ "65")) %>% 
  group_by(Year, agecat, microsim.init.sex) %>% 
  summarise(TotalPop = sum(TotalPop),
            LVDCrate = (sum(LVDCmort)/TotalPop)*100000,
            AUDrate = (sum(AUDmort)/TotalPop)*100000,
            IJrate = (sum(IJmort)/TotalPop)*100000,
            UIJrate = (sum(UIJmort)/TotalPop)*100000) %>% 
  # mutate(EDUC = factor(EDUC, levels=c("LEHS","SomeC","College"))) %>% 
  drop_na()

agest <- mort %>% filter(Year==2010) %>% 
  group_by(agecat, microsim.init.sex) %>% 
  summarise(TotalPop=sum(TotalPop)) %>% 
  ungroup() %>% 
  group_by(microsim.init.sex) %>% 
  mutate(prop=TotalPop/sum(TotalPop)) %>% 
  dplyr::select(-TotalPop)

mort <- left_join(mort, agest)

mort <- mort %>% pivot_longer(LVDCrate:UIJrate) 

mort <- mort %>% 
  mutate(weighted_rate = value*prop) %>% 
  group_by(Year, microsim.init.sex, name) %>% 
  summarise(agest_rate = sum(weighted_rate))

test <- mort %>% filter(Year==2015) %>% 
  mutate(agest_rate = round(agest_rate, digits=2)) %>% 
  pivot_wider(names_from=name, 
                                                    values_from=agest_rate) %>% 
  rename(sex=microsim.init.sex,
         AUD = AUDrate, Suicide=IJrate, LiverDisease=LVDCrate,
         UnintentInj=UIJrate) %>% ungroup() %>% 
  mutate(sex = ifelse(sex=="f","Women","Men")) %>% dplyr::select(-Year)


mort <- mort %>% group_by(Year,microsim.init.sex,
                          EDUC,microsim.init.race) %>% 
  summarise(RESTmort = sum(RESTmort),
            TotalPop = sum(TotalPop),
            rate = (RESTmort/TotalPop)*100000)


ggplot(data=mort, aes(x=Year, y=rate, colour=microsim.init.sex)) + 
  geom_line() + xlim(2015,2022) + ylim(0,NA) + 
  facet_grid(cols=vars(EDUC),
             rows=vars(microsim.init.race), scales="free") +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank())
ggsave("SIMAH_workplace/microsim/1_input_data/plots_mortality_2022.png",
       dpi=300, width=33, height=19, units="cm")
  