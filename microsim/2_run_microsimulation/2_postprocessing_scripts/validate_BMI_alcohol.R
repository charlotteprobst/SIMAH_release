#####Wrapper code for dynamic microsimulation
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
options(scipen=999)

###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
setwd(paste(WorkingDirectory))

SelectedState <- "USA"

brfss <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_reweighted_upshifted_1984_2020.RDS") %>% 
  mutate(agegroup = cut(age_var, 
                        breaks=c(0,19,24,34,44,54,64,74,100),
                        labels=c("15-19","20-24","25-34","35-44","45-54","55-64","65-74","75+"))) %>% 
           group_by(YEAR, sex_recode, agegroup) %>% 
           filter(gramsperday!=0) %>% 
           summarise(meanGPD = mean(gramsperday)) %>% 
  rename(year=YEAR, microsim.init.sex=sex_recode) %>% 
  mutate(data="Observed", microsim.init.sex=ifelse(microsim.init.sex=="Male","Men","Women"))

alcohol_data <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/Alcohol_use_validation.RDS")[[1]]%>% 
  mutate(year = as.numeric(year), data="Simulated",
         microsim.init.sex= ifelse(microsim.init.sex=="m","Men","Women"),
         agegroup =as.character(agegroup),
          agegroup = ifelse(agegroup=="75.","75+",agegroup)) %>% 
  dplyr::select(-c(seed,samplenum))

alcohol_data <- rbind(alcohol_data, brfss) %>% filter(agegroup!="15-19" & agegroup!="20-24") %>% filter(year<=2019 & year>=1985)

test <- alcohol_data %>% pivot_wider(names_from=data, values_from=meanGPD) %>% 
  mutate(pct_diff = (abs(Simulated-Observed))/((Simulated+Observed)/2)*100,
         abs_diff = abs(Simulated-Observed)) %>% 
  group_by(microsim.init.sex, agegroup) %>% 
  summarise(mean_abs = mean(abs_diff),
            min_abs = min(abs_diff),
            max_abs = max(abs_diff),
            sd_abs = sd(abs_diff))

ggplot(data=alcohol_data, aes(x=year, y=meanGPD, colour=data, linetype=data)) + geom_line(size=1, alpha=0.8) + 
  facet_grid(cols=vars(agegroup), rows=vars(microsim.init.sex)) + ylim(0,NA) + xlab("") + 
  ylab("Mean grams per day") + theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position = "bottom",
        strip.background = element_rect(fill="white")) + 
  scale_colour_manual(values=c("black","grey70")) + 
  scale_linetype_manual(values=c("solid","dashed"))
ggsave("SIMAH_workplace/microsim/2_output_data/publication/SuppFig1.png",dpi=300, width=33, height=19, units="cm")

brfss <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_reweighted_upshifted_1984_2020.RDS") %>% 
  mutate(agegroup = cut(age_var, 
                        breaks=c(0,19,24,34,44,54,64,74,100),
                        labels=c("15-19","20-24","25-34","35-44","45-54","55-64","65-74","75+"))) %>% 
  group_by(YEAR, sex_recode, agegroup) %>% 
  mutate(microsim.init.BMI = ifelse(BMI<15, 15,
                                    ifelse(BMI>50, 50, BMI))) %>% 
  summarise(meanBMI = mean(microsim.init.BMI)) %>% 
  rename(year=YEAR, microsim.init.sex=sex_recode) %>% 
  mutate(data="Observed", microsim.init.sex=ifelse(microsim.init.sex=="Male","Men","Women"))

BMI_data <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/BMI_validation.RDS")[[1]] %>% 
  mutate(year = as.numeric(year), data="Simulated",
         microsim.init.sex= ifelse(microsim.init.sex=="m","Men","Women"),
         agegroup = as.character(agegroup),
         agegroup = ifelse(agegroup=="75.","75+",agegroup)) %>% 
  dplyr::select(-c(seed,samplenum))

BMI_data <- rbind(BMI_data, brfss) %>% filter(agegroup!="15-19" & agegroup!="20-24") %>% filter(year<=2019 & year>=1985)

test <- BMI_data %>% pivot_wider(names_from=data, values_from=meanBMI) %>% 
  mutate(pct_diff = (abs(Simulated-Observed))/((Simulated+Observed)/2)*100,
         abs_diff = abs(Simulated-Observed)) %>% 
  group_by(microsim.init.sex, agegroup) %>% 
  summarise(mean_abs = mean(abs_diff),
            min_abs = min(abs_diff),
            max_abs = max(abs_diff),
            sd_abs = sd(abs_diff))

ggplot(data=BMI_data, aes(x=year, y=meanBMI, colour=data, linetype=data)) + geom_line(size=1, alpha=0.8) + 
  facet_grid(cols=vars(agegroup), rows=vars(microsim.init.sex)) + ylim(0,NA) + xlab("") + 
  ylab("Mean Body Mass Index (BMI)") + theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position = "bottom",
        strip.background = element_rect(fill="white")) + 
  scale_colour_manual(values=c("black","grey70")) + 
  scale_linetype_manual(values=c("solid","dashed"))
ggsave("SIMAH_workplace/microsim/2_output_data/publication/SuppFig2.png",dpi=300, width=33, height=19, units="cm")

