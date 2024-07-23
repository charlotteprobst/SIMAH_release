WorkingDirectory <- "/Users/charlottebuckley/Google Drive/SIMAH Sheffield"

OutputDirectory <- paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/continuous_calibration_2")

implausibility <- read.csv(paste0(OutputDirectory, "/implausibility-1.csv"))

data <- read_csv(paste0(OutputDirectory, "/output-1.csv"))

# calculate variance for implausibility equation
variance <- data %>%
  group_by(year, samplenum, microsim.init.sex, microsim.init.race, agecat, microsim.init.education, AlcCAT) %>%
  summarise(variance = var(meansimulation)) %>%
  ungroup() %>%
  group_by(year, microsim.init.sex, microsim.init.race, agecat, microsim.init.education, AlcCAT) %>%
  summarise(v_s = mean(variance, na.rm=T),
            v_s = ifelse(is.na(v_s), 1e-7, v_s))

target <- brfss %>%
  mutate(agecat=cut(microsim.init.age,
                    breaks=c(0,24,64,100),
                    labels=c("18-24","25-64","65+"))) %>%
  group_by(YEAR, microsim.init.sex, agecat, microsim.init.education, microsim.init.race, AlcCAT) %>%
  filter(microsim.init.alc.gpd!=0) %>%
  summarise(meanbrfss = mean(microsim.init.alc.gpd),
            se = std.error(microsim.init.alc.gpd)) %>%
  rename(year=YEAR)

data <- left_join(data, target, by=c("year","microsim.init.sex","agecat","microsim.init.education","microsim.init.race","AlcCAT"))

# get rid of grouping by seed
data <- data %>%
  group_by(year, samplenum, microsim.init.sex, microsim.init.race, agecat, microsim.init.education, AlcCAT) %>%
  summarise(meansimulation = mean(meansimulation, na.rm=T),
            meantarget = mean(meanbrfss, na.rm=T),
            se = mean(se, na.rm=T))

data <- left_join(data, variance, by=c("year","microsim.init.sex","microsim.init.race","agecat","microsim.init.education","AlcCAT"))

data <- data %>% mutate(microsim.init.education = ifelse(agecat=="18-24" & microsim.init.education=="College","SomeC",
                                                         microsim.init.education))

implausibility <- data %>%
  group_by(year, samplenum, microsim.init.sex, microsim.init.race, microsim.init.education, agecat, AlcCAT) %>%
  # filter(microsim.init.race!="OTH") %>%
  summarise(meansimulation = mean(meansimulation, na.rm=T),
            meantarget = mean(meantarget, na.rm=T),
            se = mean(se, na.rm=T),
            v_s = mean(v_s, na.rm=T),
            # v_o = mean(variance),
            # todo - check implausibility equation in Andrianakis paper
            # should be SE^2?
            implausibility = abs(meansimulation-meantarget)/sqrt(v_s+se^2)) %>%
  group_by(samplenum, microsim.init.sex, microsim.init.race, microsim.init.education, agecat, AlcCAT) %>%
  filter(microsim.init.sex=="m") %>% filter(agecat=="65+") %>% filter(AlcCAT=="Low risk") %>%
  # group_by(samplenum) %>%
  filter(year!=2000) %>%
  summarise(mean = mean(implausibility, na.rm=T),
            max = max(implausibility, na.rm=T))

# now figure out for each group - which sample number is the best?
min <- implausibility %>% 
  group_by(microsim.init.sex, microsim.init.race, microsim.init.education, 
           agecat, AlcCAT) %>% 
  rename(sample=samplenum) %>% 
  mutate(min_value= min(max),
         toselect = ifelse(min_value==max, 1,0),
         microsim.init.sex=ifelse(microsim.init.sex=="f","Female","Male"),
         microsim.init.race=ifelse(microsim.init.race=="BLA","Black",
                                   ifelse(microsim.init.race=="WHI","White",
                                          ifelse(microsim.init.race=="SPA","Hispanic","Other"))),
         group=paste(AlcCAT, microsim.init.education, agecat, microsim.init.race,
                      microsim.init.sex, sep="_")) %>% filter(toselect==1) %>% 
  ungroup() %>% 
  dplyr::select(group, sample, toselect)

#read in the distribution values
distribution <- read_csv(paste0(OutputDirectory, "/lhs_regression-1.csv")) %>% 
  dplyr::select(-c(...1))

distribution <- left_join(distribution, min) %>% drop_na() %>% dplyr::select(-c(sample,toselect))

groups <- c("Low risk_LEHS_65\\+_White_Male","Low risk_LEHS_65\\+_Hispanic_Male",
            "Low risk_SomeC_65\\+_White_Male","Low risk_SomeC_65\\+_Hispanic_Male",
            "Low risk_LEHS_65\\+_Black_Male","Low risk_LEHS_65\\+_Other_Male",
            "Low risk_SomeC_65\\+_Black_Male","Low risk_SomeC_65\\+_Black_Male")

distribution <- distribution %>% 
  filter(str_detect(group, paste(groups, collapse="|")))

distributions_orig <- read.csv("SIMAH_workplace/microsim/1_input_data/CatContDistr_beta.csv") %>%
  dplyr::select(group, shape1, shape2, min, max)

distributions_orig <- distributions_orig %>% 
  filter(!str_detect(group, paste(groups, collapse="|")))

distributions_orig <- rbind(distribution, distributions_orig)

# test <- distributions_orig %>% group_by(group) %>% tally()
OutputDirectory <- "/Users/charlottebuckley/Google Drive/SIMAH Sheffield/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/continuous_calibration"
write.csv(distributions_orig, paste0(OutputDirectory, "/calibration_continuous_distribution.csv"), row.names=F)
  