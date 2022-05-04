#####Wrapper code for dynamic microsimulation
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
options(scipen=999)

####EDIT ONLY BELOW HERE ### 
###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
setwd(paste(WorkingDirectory))

# first plot how implausibility changes over waves
files <- (Sys.glob(paste("SIMAH_workplace/microsim/2_output_data/calibration_output/implausibility*.csv", sep="")))

index <- c(1,10,11,12,13,14,15,2,3,4,5,6,7,8,9)
files <- files[order(index)]
# 
files
list <- lapply(files, function(x) read.csv(x, header = TRUE)) 
for(i in 1:length(list)){
  list[[i]]$wave <- i
}

imp <- do.call(rbind, list)
ggplot(data=imp, aes(x=as.factor(wave), y=maximplausibility)) + geom_boxplot() + theme_bw() + 
  xlab("wave") + ylab("implausibility")
ggsave("SIMAH_workplace/microsim/2_output_data/calibration_output/plots/implausibility.png",
       dpi=300, width=33, height=19, units="cm")

# now plot cirrhosis output over waves 
files <- (Sys.glob(paste("SIMAH_workplace/microsim/2_output_data/calibration_output/Cirrhosis*.RDS", sep="")))
index <- c(1,10,11,12,13,14,15,2,3,4,5,6,7,8,9)
files
files <- files[order(index)]
list <- lapply(files, function(x) readRDS(x)) 
for(i in 1:length(list)){
  list[[i]] <- as.data.frame(do.call(rbind,list[[i]]))
  list[[i]]$wave <- i
}
files <- do.call(rbind, list)
WholePopSize <- read.csv("SIMAH_workplace/microsim/1_generating_population/constraintsUSA.csv") %>% 
  dplyr::select(marriedF:unmarriedM) %>% mutate(total=marriedF+unmarriedF+marriedM+unmarriedM)
proportion <- 200000/WholePopSize$total

source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/process_cirrhosis_1984_2016.R") 

meansim <- files %>% group_by(wave, year, samplenum, microsim.init.sex, agegroup) %>% 
  summarise(microsim = mean(rateper100000),
            microsim = ifelse(is.na(microsim),0,microsim)) %>% 
  rename(sex=microsim.init.sex)

cirrhosismortality <- cirrhosismortality %>% rename(year=Year) %>% 
  dplyr::select(year, sex, agegroup, rate) %>% 
  rename(target=rate)

meansim <- left_join(meansim, cirrhosismortality) 
# %>% 
#   pivot_longer(microsim:target)
meansim <- meansim %>% group_by(wave, year, sex, agegroup) %>% 
  mutate(min = min(microsim), max=max(microsim))

subset <- meansim %>% filter(agegroup!="15-19") %>% filter(agegroup!="20-24") %>% 
  filter(agegroup!="25-34") %>% mutate(agegroup=ifelse(agegroup=="75.","75+",agegroup))

ggplot(data=subset(subset, sex=="f"), aes(x=year, y=target)) + 
  facet_grid(cols=vars(wave), rows=vars(agegroup), scales="free") +
  geom_ribbon(aes(ymin=min, ymax=max), fill="grey70") +
  geom_line() + theme_bw() + ggtitle("Women") + 
  ylab("Mortality rate per 100,000 population")
ggsave(paste0("SIMAH_workplace/microsim/2_output_data/calibration_output/plots/cirrhosis_agesp_women.png"),
       dpi=300, width=32, height=21, units="cm")


ggplot(data=subset(subset, sex=="m"), aes(x=year, y=target)) + 
  facet_grid(cols=vars(wave), rows=vars(agegroup), scales="free") +
  geom_ribbon(aes(ymin=min, ymax=max), fill="grey70") +
  geom_line() + theme_bw() + ggtitle("Men") + 
  ylab("Mortality rate per 100,000 population")
ggsave(paste0("SIMAH_workplace/microsim/2_output_data/calibration_output/plots/cirrhosis_agesp_men.png"),
       dpi=300, width=32, height=21, units="cm")


# now plot the final wave for all age groups 
finalwave <- meansim %>% filter(wave==max(meansim$wave))
ggplot(data=finalwave, aes(x=year, y=value, colour=name, linetype=as.factor(samplenum))) + 
  geom_line(size=1) + 
  facet_grid(rows=vars(sex), cols=vars(agegroup)) +
  scale_linetype_discrete(guide="none") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text = element_text(size=12),
        strip.background = element_rect(fill="white")) +
  ylab("rate per 100,000") + xlab("")
ggsave(paste0("SIMAH_workplace/microsim/2_output_data/calibration_output/plots/cirrhosis_finalwave.png"),
       dpi=300, width=33, height=19, units="cm")

# now plot the lhs samples 
files <- (Sys.glob(paste("SIMAH_workplace/microsim/2_output_data/calibration_output/lhs*.csv", sep="")))
index <- c(1,10,11,12,13,14,15,2,3,4,5,6,7,8,9)
# files
files <- files[order(index)]
list <- lapply(files, function(x) read.csv(x)) 
for(i in 1:length(list)){
  list[[i]]$wave <- i
}

files <- do.call(rbind, list) %>% pivot_longer(cols=BETA_MALE_MORTALITY:DECAY_SPEED) %>% 
  mutate(wave=as.factor(wave))

nb.cols <- 15
mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(nb.cols)

ggplot(data=files, aes(x=value, group=wave, fill=wave, colour=wave)) + 
  geom_density(aes(x=value, y=..scaled..,), alpha=0.4, inherit.aes = TRUE) + 
  facet_wrap(~name, scales="free") + 
  theme_bw() +
  theme(legend.position="bottom") + 
  scale_colour_manual(values=mycolors) + 
  scale_fill_manual(values=mycolors) + xlab("") + ylab("density")
ggsave(paste0("SIMAH_workplace/microsim/2_output_data/calibration_output/plots/lhs_density_agesp.png"),
       dpi=300, width=33, height=19, units="cm")

# save the top 3 LH samples for the best run from the final wave 
topruns <- as.numeric(unlist(imp %>% filter(wave==max(imp$wave)) %>% 
  mutate(ntile = ntile(maximplausibility, nrow(.))) %>% 
  filter(ntile<=3) %>% dplyr::select(samplenum)))
toplhs <- do.call(rbind,list) %>% filter(wave==max(imp$wave)) %>% 
  filter(SampleNum %in% topruns)
