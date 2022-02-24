# Analyse LE decomposition for 2020 weighted data
# Mortality data NCHS
# Population data: ACS
# Project: SIMAH


# libraries required:
library("tidyverse")
library("DemoDecomp")
library("dplyr")
library("reshape")
library("data.table")


## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")
#setwd("~/Documents/Promotion/Mortality US")
setwd("~/Google Drive/SIMAH Sheffield/")

Results_SES_raw <- read.csv(paste0("SIMAH_workplace/life_expectancy/2_out_data/dResults_contrib_", 2018, "_", 2020, "ACS.csv") ) %>% 
  mutate(type="raw_ACS")
Results_SES_model <- read.csv(paste0("SIMAH_workplace/life_expectancy/2_out_data/dResults_ACSmodel_contrib_", 2018, "_", 2020, "ACS.csv") ) %>% 
  mutate(type="model_ACS")

# calculate the difference in 2020 life expectancy for raw ACS and modelled ACS pop counts 

Summary_SES <- rbind(Results_SES_raw, Results_SES_model) %>% 
  dplyr::select(sex, edclass, start_year, end_year, LE1, LE2, type) %>% 
  filter(start_year==2019) %>% 
  pivot_longer(cols=c(LE1:LE2)) %>% 
  mutate(sex = ifelse(sex==1,"Men","Women"),
         edclass = factor(edclass, levels=c("LEHS","SomeC","College")))

ggplot(data=Summary_SES, aes(x=name, y=value, colour=type)) + 
  geom_point() + facet_grid(cols=vars(edclass), rows=vars(sex)) +
  theme_bw() + xlab("") + ylab("life expectancy at 18")

Results_race_raw <- read.csv(paste0("SIMAH_workplace/life_expectancy/2_out_data/dResults_contrib_", 2018, "_", 2020, "race_ACS.csv") ) %>% 
  mutate(type="raw_ACS")
Results_race_model <- read.csv(paste0("SIMAH_workplace/life_expectancy/2_out_data/dResults_ACSmodel_contrib_", 2018, "_", 2020, "race_ACS.csv") ) %>% 
  mutate(type="model_ACS")

Summary_race <- rbind(Results_race_raw, Results_race_model) %>% 
  dplyr::select(sex, edclass, race, start_year, end_year, LE1, LE2, type) %>% 
  filter(start_year==2019) %>% 
  pivot_longer(cols=c(LE1:LE2)) %>% 
  mutate(sex = ifelse(sex==1,"Men","Women"),
         edclass = factor(edclass, levels=c("LEHS","SomeC","College")))

ggplot(data=subset(Summary_race, sex=="Men"), aes(x=name, y=value, colour=type)) + 
  geom_point() + facet_grid(cols=vars(edclass), rows=vars(race)) +
  theme_bw() + xlab("") + ylab("life expectancy at 18") + ggtitle("Men")

ggplot(data=subset(Summary_race, sex=="Women"), aes(x=name, y=value, colour=type)) + 
  geom_point() + facet_grid(cols=vars(edclass), rows=vars(race)) +
  theme_bw() + xlab("") + ylab("life expectancy at 18") + ggtitle("Women")

fortable <- Summary_race %>% 
  pivot_wider(names_from=type, values_from=value) %>% 
  pivot_wider(names_from=name, values_from=c(raw_ACS,model_ACS)) %>% 
  dplyr::select(-model_ACS_LE1) %>% 
  mutate(raw_ACS_LE1 = round(raw_ACS_LE1, digits=2),
         raw_ACS_LE2 = round(raw_ACS_LE2, digits=2),
         model_ACS_LE2 = round(model_ACS_LE2, digits=2)) %>%
  dplyr::rename(LE_2019 = raw_ACS_LE1,
                LE_2020_ACSweights = raw_ACS_LE2,
                LE_2020_modelACS = model_ACS_LE2) %>% 
  dplyr::select(-c(start_year,end_year))
write.csv(fortable, "SIMAH_workplace/life_expectancy/2_out_data/Table_summary_modelACS.csv",row.names=F)
