# plot beverage preferences 

# by age sex and education 
summary_beverages <- basepop %>% 
  mutate(agecat = cut(microsim.init.age,
                      breaks=c(0,24,44,64,100),
                      labels=c("18-24","25-44","45-64","65+"))) %>% 
  filter(AlcCAT!="Non-drinker") %>% 
  mutate(microsim.init.education = ifelse(microsim.init.education=="SomeCPlus","SomeC",
                                          microsim.init.education),
         microsim.init.education = factor(microsim.init.education,
                                          levels=c("LEHS","SomeC","College")),
         AlcCAT = case_when(AlcCAT=="Low risk" ~ "Category I",
                            AlcCAT=="Medium risk" ~ "Category II",
                            AlcCAT=="High risk" ~ "Category III"),
         microsim.init.sex = ifelse(microsim.init.sex=="f","Women","Men"),
         microsim.init.race = case_when(
           microsim.init.race=="BLA" ~ "Black",
           microsim.init.race=="OTH" ~ "Others",
           microsim.init.race=="SPA" ~ "Hispanic",
           microsim.init.race=="WHI" ~ "White"
         )) %>% 
  group_by(agecat, microsim.init.sex, microsim.init.education, AlcCAT) %>% 
  summarise(beer = mean(beergpd),
            wine = mean(winegpd),
            liquor = mean(liqgpd),
            coolers = mean(coolgpd)) %>% 
  pivot_longer(cols=c(beer:coolers)) 
  

ggplot(summary_beverages, aes(x=agecat, y=value, colour=name, group=name)) + 
  geom_line(linewidth=1)+
  facet_grid(cols=vars(microsim.init.sex, microsim.init.education), rows=vars(AlcCAT), scales="free") + 
  theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position="bottom") + 
  xlab("Age") + ylab("Mean grams per day")
getwd()
ggsave("SIMAH_workplace/microsim/beverage_preferences_v1.png", dpi=300, 
       width=33, height=19, units="cm")

# simplified version 
summary_beverages <- basepop %>% 
  mutate(agecat = cut(microsim.init.age,
                      breaks=c(0,24,44,64,100),
                      labels=c("18-24","25-44","45-64","65+"))) %>% 
  filter(AlcCAT!="Non-drinker") %>% 
  mutate(microsim.init.education = ifelse(microsim.init.education=="SomeCPlus","SomeC",
                                          microsim.init.education),
         microsim.init.education = factor(microsim.init.education,
                                          levels=c("LEHS","SomeC","College")),
         AlcCAT = case_when(AlcCAT=="Low risk" ~ "Category I",
                            AlcCAT=="Medium risk" ~ "Category II",
                            AlcCAT=="High risk" ~ "Category III"),
         microsim.init.sex = ifelse(microsim.init.sex=="f","Women","Men"),
         microsim.init.race = case_when(
           microsim.init.race=="BLA" ~ "Black",
           microsim.init.race=="OTH" ~ "Others",
           microsim.init.race=="SPA" ~ "Hispanic",
           microsim.init.race=="WHI" ~ "White"
         )) %>% 
  group_by(microsim.init.sex, AlcCAT) %>% 
  summarise(beer = sum(beergpd),
            wine = sum(winegpd),
            liquor = sum(liqgpd),
            coolers = sum(coolgpd)) %>% 
  pivot_longer(cols=c(beer:coolers)) 

ggplot(summary_beverages, aes(x=AlcCAT, y=value, colour=name, group=name)) + 
  geom_line(linewidth=1)+
  facet_grid(cols=vars(microsim.init.sex), scales="free") + 
  theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position="bottom") + 
  xlab("Alcohol category") + ylab("Total grams per day")
getwd()
ggsave("SIMAH_workplace/microsim/beverage_preferences_v1_simplified.png", dpi=300, 
       width=33, height=19, units="cm")

