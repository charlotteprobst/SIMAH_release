
# libraries required:
library("tidyverse")
library("DemoDecomp")
library("dplyr")
library("reshape")
library("data.table")
library(readr)


## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")
#setwd("~/Documents/Promotion/Mortality US")
setwd("~/Google Drive/SIMAH Sheffield/")

mortality <- read_csv("SIMAH_workplace/mortality/3_out data/allethn_sumCOD_0020_LE_decomp.csv") %>% 
  filter(year>=2018)

popcounts <- read_csv("SIMAH_workplace/demography/ACS_popcounts_2000_2020.csv") %>% 
  filter(state=="USA") %>% filter(year>=2018)

mortality <- left_join(mortality, popcounts) %>% 
  pivot_longer(Tmort:RESTmort) %>% 
  group_by(year, age_gp, sex,name) %>% 
  summarise(TPop=sum(TPop),
            TMort = sum(value))

age2019 <- mortality %>% filter(year==2018) %>% 
  ungroup() %>% 
  group_by(year, sex, age_gp) %>% 
  summarise(poptotal = mean(TPop)) %>% ungroup() %>% 
  group_by(year, sex) %>% 
  mutate(percent = poptotal / sum(poptotal)) %>% ungroup() %>% dplyr::select(sex, age_gp, percent)

mortality <- left_join(mortality, age2019) %>% 
  group_by(year, age_gp, sex, name) %>% 
  mutate(weightedrate = (TMort/TPop*100000)*percent) %>% 
  ungroup() %>% 
  group_by(year, sex, name) %>% 
  summarise(agestrate = sum(weightedrate)) %>% 
  pivot_wider(names_from=year, values_from=agestrate) %>% 
  dplyr::rename(Rate_2018 = `2018`, Rate_2019 = `2019`, Rate_2020=`2020`) %>% 
  mutate(Percent_increase_18_19 = round((Rate_2019-Rate_2018)/Rate_2018,4)*100,
         Percent_increase_19_20 = round((Rate_2020-Rate_2019)/Rate_2019,4)*100,
         sex = ifelse(sex==1, "Men","Women"))

write.csv(mortality, "SIMAH_workplace/mortality/3_out data/relativechange2019_2020.csv",row.names=F)


