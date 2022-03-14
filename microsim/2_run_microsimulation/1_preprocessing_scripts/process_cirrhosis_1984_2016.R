# first read in the data 1984 - 1998
cirrhosis1984_1998 <- read.delim("SIMAH_workplace/microsim/1_input_data/Cirrhosis_Compressed Mortality, 1979-1998.txt") %>% 
  filter(Notes!="Total") %>% dplyr::select(-Notes) %>% 
  mutate(sex = ifelse(Gender=="Female","f","m")) %>% rename(agegroup=Age.Group.Code) %>% 
  mutate(agegroup=ifelse(agegroup=="75-84","75.",agegroup),
         Deaths = ifelse(agegroup=="15-19",Deaths/5*2,
                         ifelse(agegroup=="75.",Deaths/10*6,
                                Deaths)),
         Population = ifelse(agegroup=="15-19",Population/5*2,
                             ifelse(agegroup=="75.", Population/10*6,Population)),
         Deaths = Deaths*proportion,
         Population=Population*proportion,
         Crude.Rate=parse_number(Crude.Rate),
         cat= paste(agegroup, sex, sep="_")) %>% dplyr::select(Year, cat, sex, agegroup, Deaths,Population) %>% 
  group_by(Year, sex, agegroup) %>% summarise(rate = sum(Deaths)/sum(Population)*100000,
                                              count=sum(Deaths)*100, Population=sum(Population))

cirrhosis1999_2016 <- read.delim("SIMAH_workplace/microsim/1_input_data/Cirrhosis_Compressed Mortality, 1999-2016.txt") %>% 
  filter(Notes!="Total") %>% dplyr::select(-Notes) %>% 
  mutate(sex = ifelse(Gender=="Female","f","m")) %>% rename(agegroup=Age.Group.Code) %>% 
  mutate(agegroup=ifelse(agegroup=="75-84","75.",agegroup),
         Deaths = ifelse(agegroup=="15-19",Deaths/5*2,
                         ifelse(agegroup=="75.",Deaths/10*6,
                                Deaths)),
         Population = ifelse(agegroup=="15-19",Population/5*2,
                             ifelse(agegroup=="75.", Population/10*6,Population)),
         Deaths = Deaths*proportion,
         Population=Population*proportion,
         Crude.Rate=parse_number(Crude.Rate),
         cat= paste(agegroup, sex, sep="_")) %>% dplyr::select(Year, cat, sex, agegroup, Deaths,Population) %>% 
  group_by(Year, sex, agegroup) %>% summarise(rate = sum(Deaths)/sum(Population)*100000,
                                              count=sum(Deaths)*100, Population=sum(Population))
cirrhosismortality <- rbind(cirrhosis1984_1998, cirrhosis1999_2016) %>% drop_na()
rm(cirrhosis1984_1998, cirrhosis1999_2016)
  
# cirrhosismortality$sex <- ifelse(cirrhosismortality$sex=="f","Women","Men")
# ggplot(data=cirrhosismortality, aes(x=Year, y=Crude.Rate)) + geom_line(size=1) + facet_grid(cols=vars(agegroup), rows=vars(sex)) +
#   theme_bw() + ylab("cirrhosis mortality rate per 100,000 population") +
#   theme(strip.background = element_rect(fill="white"))
# ggsave("SIMAH_workplace/microsim/2_output_data/calibration_output/mortality_rate_plot_target.png",
#        dpi=300, width=33, height=19, units="cm")
