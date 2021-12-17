cirrhosisdata <- read.csv("SIMAH_workplace/microsim/1_input_data/LC_hosp_Output.csv") %>% 
  dplyr::select(c(year,location,sex_id,tups15to19:tupsplus80)) %>% 
  filter(location=="USA") %>% 
  pivot_longer(cols=tups15to19:tupsplus80) %>% 
  mutate(name=gsub("tups","",name),
         agecat = ifelse(name=="15to19"|name=="20to24","18-24",
                         ifelse(name=="25to29"|name=="30to34","25-34",
                                ifelse(name=="35to39"|name=="40to44","35-44",
                                       ifelse(name=="45to49"|name=="50to54","45-54",
                                              ifelse(name=="55to59"|name=="60to64","55-64",
                                                     ifelse(name=="65to69"|name=="70to74","65-74","75+"))))))) %>% 
  group_by(year, sex_id, agecat) %>% summarise(value=sum(value))


ggplot(data=cirrhosisdata, aes(x=year, y=value)) + geom_line() + 
  facet_grid(cols=vars(agecat), rows=vars(sex_id)) + theme_bw()

ggsave("SIMAH_workplace/microsim/2_output_data/plots/LC_morbidity_by_age.png", dpi=300, 
       width=33, height=19, units="cm")


forplot <- cirrhosisdata %>% 
  separate(cat, into=c("agecat","sex"), sep="_") %>% 
  mutate(agecat = ifelse(agecat=="15-19","18-24",
                         ifelse(agecat=="20-24","18-24",
                                ifelse(agecat=="75.", "75-79",agecat)))) %>% 
  group_by(Year, sex, agecat) %>% summarise(count=sum(count)) %>% 
  data.frame(.) %>% mutate(data="CASCADE")

toexpand <- expand.grid(Year=c(1984:2012), age=c(18:79), sex=c("m","f")) %>% 
  mutate(agecat=cut(age, breaks=c(0,24,34,44,54,64,74,100),
                    labels=c("18-24","25-34","35-44","45-54","55-64","65-74","75-79")))
toexpand <- left_join(toexpand, forplot) %>% 
  mutate(count = ifelse(agecat=="18-24",count/7,
                        ifelse(agecat=="75-79", count/5, count/10)),
         birthyear = Year - age,
         birthcohort = cut(birthyear, breaks=c(1900,1920,1925,1930,1935,1940,1945,1950,
                                               1955, 1960, 1965, 1970, 1975, 1980,1985,
                                               1990,1995),
                           labels=c("1900-1920","1921-1925","1926-1930","1931-1935","1936-1940",
                                    "1941-1945","1946-1950","1951-1955","1956-1960","1961-1965","1966-1970",
                                    "1971-1975","1976-1980","1981-1985","1986-1990","1991-1995")))

summary <- toexpand %>% group_by(Year, sex, birthcohort) %>% 
  summarise(count=sum(count)*100) %>% mutate(type="target") 


names(summary)

names(Cirrhosis)

CirrhosisSummary <- Cirrhosis %>% ungroup() %>% rename(count=n, birthcohort=cohort, sex=microsim.init.sex) %>% 
  dplyr::select(-c(seed,samplenum)) %>% mutate(type="microsim")

names(summary)

summary <- summary %>% rename(target=count) %>% dplyr::select(-type)

CirrhosisSummary <- rbind(CirrhosisSummary,summary)
CirrhosisSummary <- left_join(CirrhosisSummary, summary)

ggplot(data=CirrhosisSummary, aes(x=Year, y=count, colour=pathway)) + geom_line() + 
  geom_line(aes(x=Year, y=target),colour="black") + 
  facet_grid(cols=vars(birthcohort), rows=vars(sex)) + theme_bw()
ggsave("SIMAH_workplace/microsim/2_output_data/plots/LC_by_cohort_pathway.png", dpi=300, 
       width=33, height=19, units="cm")


ggplot(data=summary, aes(x=Year, y=count)) + geom_line() + 
  facet_grid(cols=vars(birthcohort), rows=vars(sex), scales="free") + ylim(0,NA) + 
  theme_bw() + xlim(1984,2012)
ggsave("output_data/LC_by_cohort_full.png", dpi=300, width=33, height=19, units="cm")

summary <- toexpand %>% filter(birthyear>=1936 & birthyear<=1980) %>% group_by(Year, sex, birthcohort) %>% 
  summarise(count=sum(count))

ggplot(data=summary, aes(x=Year, y=count)) + geom_line() + 
  facet_grid(cols=vars(birthcohort), rows=vars(sex), scales="free") + ylim(0,NA) + 
  theme_bw() + xlim(1984,2012)
ggsave("output_data/LC_by_cohort_subset.png", dpi=300, width=33, height=19, units="cm")

library(ggplot2)
birthcohort <- data.frame(birthcohort)
ggplot(data=birthcohort, aes(x=Year, y=count)) +
  geom_point() + 
  facet_grid(cols=vars(cohort), rows=vars(sex))
birthcohort$cohort <- as.factor(birthcohort$cohort)
ggplot(data=birthcohort, aes(x=Year, y=count)) + geom_line() + 
  facet_wrap()

# different plot - 10 year age groups by birth year
new <- toexpand %>% group_by(birthyear, agecat, sex) %>% summarise(count=sum(count))

ggplot(data=new, aes(x=birthyear, y=count, colour=agecat)) + geom_line() + 
  facet_grid(rows=vars(sex)) + theme_bw()

ggsave("output_data/birthyearbyagecat.png", dpi=300, width=33, height=19, units="cm")

new <- toexpand %>% group_by(Year, agecat, sex) %>% summarise(count=sum(count))

ggplot(data=new, aes(x=Year, y=count, colour=agecat)) + geom_line() + 
  facet_grid(rows=vars(sex)) + theme_bw()



SIMAH <- read.csv("output_data/LC_SIMAH.csv") %>% 
  rename(Year=year, count=total) %>% mutate(data="SIMAH")

compare <- rbind(forplot, SIMAH)

write.csv(compare, "output_data/CASCADE_SIMAH_compare.csv", row.names=F)

ggplot(data=compare, aes(x=Year, y=count, colour=data)) + 
  geom_line() + facet_grid(rows=vars(sex), cols=vars(agecat)) +
  theme_bw()
ggsave("output_data/CASCADE_SIMAH_compare.png", dpi=300, width=33, height=19,
       units="cm")  

# compare to CDC data by age group 
CDC <- read.delim("output_data/Compressed Mortality, 1979-1998 .txt") %>% 
  drop_na() %>% filter(Notes!="Total") %>% 
  dplyr::select(-c(Notes, Year.Code, Gender.Code, Age.Group, Population, Crude.Rate)) %>% 
  mutate(agecat = ifelse(Age.Group.Code=="15-19" | Age.Group.Code=="20-24","18-24",
                         ifelse(Age.Group.Code=="75-84","75-79", Age.Group.Code))) %>% 
  rename(sex=Gender, count=Deaths) %>% 
  mutate(sex=recode(sex, "Female"="f","Male"="m")) %>% 
  group_by(Year, sex, agecat) %>% summarise(count=sum(count)) %>% 
  mutate(data="CDC")

compare <- rbind(compare, CDC)

ggplot(data=compare, aes(x=Year, y=count, colour=data)) + 
  geom_line() + facet_grid(rows=vars(sex), cols=vars(agecat)) +
  theme_bw()
ggsave("output_data/CASCADE_SIMAH_compare.png", dpi=300, width=33, height=19,
       units="cm")  