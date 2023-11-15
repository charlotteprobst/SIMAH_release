# SIMAH project Sep 2023 
# analysing the run to run variability of education transitions
library(tidyverse)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/education_calibration")

# load in microsim R package
setwd(paste(WorkingDirectory))

data <- read.csv(paste0(DataDirectory, "/prior_range_uninflated_neworder.csv")) %>% 
  mutate(AGECAT = cut(microsim.init.age,
                      breaks=c(0,24,34,44,54,64,79),
                      labels=c("18-24","25-34","35-44","45-54",
                               "55-64","65-79")),
         SEX = ifelse(microsim.init.sex=="m", "Men","Women"),
         RACE = recode(microsim.init.race, "BLA"="Black","WHI"="White","SPA"="Hispanic",
                       "OTH"="Others")) %>% 
  rename(EDUC=microsim.init.education, YEAR=year) %>% 
  group_by(YEAR, samplenum, seed, SEX, AGECAT,RACE,
           EDUC) %>% 
  summarise(n=sum(n)) %>% 
  ungroup() %>% 
  group_by(YEAR, samplenum, SEX, AGECAT, EDUC,RACE) %>% 
  summarise(n=mean(n)) %>% 
  ungroup() %>% 
  group_by(YEAR, samplenum, SEX, AGECAT,RACE) %>% 
  mutate(prop=n/sum(n), YEAR=as.integer(YEAR)) %>% 
  dplyr::select(-n) %>% drop_na()

# read in target data 
targets <- read.csv(paste0(DataDirectory, "/target_data.csv")) %>% 
  # group_by(YEAR,SEX,AGECAT,EDUC,RACE) %>% 
  # summarise(n=sum(n)) %>% 
  dplyr::select(-n) %>%
  rename(target=prop)

data <- left_join(data,targets) %>% 
  mutate(EDUC = factor(EDUC, levels=c("LEHS","SomeC","College")))

# calculate implausibility 
implausibility <- data %>% 
  filter(AGECAT=="18-24") %>%
  filter(YEAR<=2019) %>% 
  group_by(YEAR, samplenum, SEX,AGECAT, RACE, EDUC) %>% 
  summarise(implausibility = abs(prop-target)/sqrt(SE))

summary_implausibility <- implausibility %>% 
  group_by(samplenum) %>% 
  summarise(max = max(implausibility),
            mean = mean(implausibility)) %>% 
  ungroup() %>% 
  mutate(percentile = ntile(mean,100))

samples <- summary_implausibility %>% filter(percentile<=10)

# now read in the samples
markovmodel <- readRDS(paste0(DataDirectory, "/sampled_markov USA -1.RDS")) %>% 
  filter(SampleNum %in% samples$samplenum)

samples <- markovmodel %>% dplyr::select(-c(SampleNum, inflation, TimePeriod)) %>% 
  mutate_all(as.numeric)
estimates <- colMeans(samples)
cov <- cov(samples)

newsamples <- mvrnorm(n=100, estimates, cov)

compare_top10 <- samples %>% pivot_longer(qbase:`qcov.31`) %>% 
  group_by(name) %>% summarise(min_top10=min(value),
                               max_top10=max(value),
                               mean_top10=mean(value))

compare_newsamples <- newsamples %>% as.data.frame() %>% 
  pivot_longer(qbase:qcov.31) %>% 
  group_by(name) %>% summarise(min_sampled=min(value),
                               max_sampled=max(value),
                               mean_sampled=mean(value))

compare_top10 <-left_join(compare_top10, compare_newsamples) 

compare_top10 <- compare_top10 %>% 
  pivot_longer(min_top10:mean_sampled, names_to="type") %>% 
  mutate(sample = ifelse(grepl("sampled",type), "sampled", "top10"),
         type = gsub("_top10","",type),
         type = gsub("_sampled","", type)) %>% 
  pivot_wider(names_from=type, values_from=value)

ggplot(data=compare_top10, aes(x=name, y=mean, colour=sample)) + 
  geom_point() + 
  geom_errorbar(aes(ymin=min, ymax=max)) + 
  ylab("mean +/- min and max") + 
  xlab("parameter") + 
  theme_bw() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom")

ggsave(paste0(DataDirectory, "/parametervalues_sampled.png"),
       dpi=300, width=33, height=19, units="cm")
  



