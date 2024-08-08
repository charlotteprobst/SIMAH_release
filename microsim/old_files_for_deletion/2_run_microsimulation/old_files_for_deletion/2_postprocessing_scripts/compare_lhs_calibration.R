# comparing the calibrated parameters from the final wave of age standardised versus non age st
# now plot the lhs samples 
setwd("~/Google Drive/SIMAH Sheffield/")
library(dplyr)
library(tidyr)
library(ggplot2)

files <- (Sys.glob(paste("SIMAH_workplace/microsim/2_output_data/calibration_output_decay/lhs*.csv", sep="")))
index <- c(1,10,11,12,13,14,15,2,3,4,5,6,7,8,9)
# files
files <- files[order(index)]
list <- lapply(files, function(x) read.csv(x)) 
for(i in 1:length(list)){
  list[[i]]$wave <- i
}
filesagest <- do.call(rbind, list) %>% pivot_longer(cols=BETA_MALE_MORTALITY:DECAY_LENGTH) %>% 
  mutate(wave=as.factor(wave), calibration="age standardized") %>% filter(wave=="15")

prior <- do.call(rbind, list) %>% pivot_longer(cols=BETA_MALE_MORTALITY:DECAY_LENGTH) %>% 
  mutate(wave=as.factor(wave), calibration="prior") %>% filter(wave=="1") 

files <- (Sys.glob(paste("SIMAH_workplace/microsim/2_output_data/calibration_output_decay_agesp/lhs*.csv", sep="")))
index <- c(1,10,11,12,13,14,15,2,3,4,5,6,7,8,9)
# files
files <- files[order(index)]
list <- lapply(files, function(x) read.csv(x)) 
for(i in 1:length(list)){
  list[[i]]$wave <- i
}
filesagesp <- do.call(rbind, list) %>% pivot_longer(cols=BETA_MALE_MORTALITY:DECAY_LENGTH) %>% 
  mutate(wave=as.factor(wave), calibration="age specific") %>% filter(wave=="15")

files <- rbind(filesagest, filesagesp,prior)

files <- files %>% 
  mutate(name = recode(name, "BETA_FEMALE_MORTALITY" = "BHeavyUse_women",
                       "BETA_MALE_MORTALITY"="BHeavyUse_men",
                       "BETA_FORMER_DRINKERS_MEN" = "BFormerdrinker_men",
                       "BETA_FORMER_DRINKERS_WOMEN"="BFormerdrinker_women",
                       "METABOLIC_BETA1_MALE" = "BMetabolic1_men",
                       "METABOLIC_BETA2_MALE" = "BMetabolic2_men",
                       "METABOLIC_BETA1_FEMALE" = "BMetabolic1_women",
                       "METABOLIC_BETA2_FEMALE" = "BMetabolic2_women",
                       "BETA_HEPATITIS" = "BHepatitis",
                       "THRESHOLD" = "Threshold",
                       "THRESHOLD_MODIFIER" = "Threshold_modifier",
                       "DECAY_SPEED"= "Decay speed",
                       "DECAY_LENGTH" = "Decay length",
                       "IRR_correlation" = "History correlation"),
         name = factor(name, levels=c("BHeavyUse_women","BHeavyUse_men","BFormerdrinker_men",
                                      "BFormerdrinker_women","BMetabolic1_men","BMetabolic2_men",
                                      "BMetabolic1_women","BMetabolic2_women","BHepatitis",
                                      "Threshold", "Threshold_modifier","Decay speed","Decay length",
                                      "History correlation")),
         calibration = factor(calibration, levels=c("prior","age standardized","age specific")))


cbp1 <- c("#999999", "#E69F00", "#56B4E9") 

ggplot(data=files, aes(x=value, group=calibration, fill=calibration, colour=calibration)) + 
  geom_density(aes(x=value, y=..scaled..,), alpha=0.9, inherit.aes = TRUE) + 
  facet_wrap(~name, scales="free", ncol=) + theme_bw() +
  scale_fill_manual(values=cbp1) + 
  scale_colour_manual(values=cbp1) + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        text = element_text(size=18)) + xlab("")

ggsave(paste0("SIMAH_workplace/microsim/2_output_data/publication/SuppFig6.png"),
       dpi=300, width=33, height=19, units="cm")


parallel <- files %>% dplyr::select(SampleNum,name, value,calibration) %>% 
  filter(calibration!="prior") %>% distinct() %>% 
  pivot_wider(names_from=name, values_from=value) %>% 
  mutate(SampleNum = as.factor(SampleNum)) %>% 
  sample_n(100)

ggparcoord(subset(parallel, calibration="age standardized"), columns=3:15, groupColumn=1) +
  theme(legend.position="none")



means <- files %>% group_by(name, calibration) %>% 
  summarise(mean = round(mean(value),2),
            sd = round(sd(value),2),
            min = round(min(value),2),
            max = round(max(value),2),
            summarystat = ifelse(name=="THRESHOLD" | name=="DECAY_SPEED" | name=="THRESHOLD_MODIFIER", paste0(min,"--",max),
                                 paste(sd))) %>% distinct() %>% 
  dplyr::select(name, calibration, mean, summarystat) %>% 
  pivot_wider(names_from=calibration, values_from=c(mean,summarystat)) %>% 
  dplyr::select(mean_prior, summarystat_prior,
                `mean_age standardized`, `summarystat_age standardized`,
                `mean_age specific`, `summarystat_age specific`)
write.csv(means, "SIMAH_workplace/microsim/2_output_data/publication/Table2Posterior.csv", row.names=F)
