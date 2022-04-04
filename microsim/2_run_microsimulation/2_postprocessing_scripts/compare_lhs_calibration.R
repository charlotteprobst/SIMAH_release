# comparing the calibrated parameters from the final wave of age standardised versus non age st
# now plot the lhs samples 
files <- (Sys.glob(paste("SIMAH_workplace/microsim/2_output_data/calibration_output_agest/lhs*.csv", sep="")))
index <- c(1,10,11,12,13,14,15,2,3,4,5,6,7,8,9)
# files
files <- files[order(index)]
list <- lapply(files, function(x) read.csv(x)) 
for(i in 1:length(list)){
  list[[i]]$wave <- i
}
filesagest <- do.call(rbind, list) %>% pivot_longer(cols=BETA_MALE_MORTALITY:DECAY_SPEED) %>% 
  mutate(wave=as.factor(wave), calibration="age standardized") %>% filter(wave=="15")

prior <- do.call(rbind, list) %>% pivot_longer(cols=BETA_MALE_MORTALITY:DECAY_SPEED) %>% 
  mutate(wave=as.factor(wave), calibration="prior") %>% filter(wave=="1") 

files <- (Sys.glob(paste("SIMAH_workplace/microsim/2_output_data/calibration_output/lhs*.csv", sep="")))
index <- c(1,10,11,12,13,14,15,2,3,4,5,6,7,8,9)
# files
files <- files[order(index)]
list <- lapply(files, function(x) read.csv(x)) 
for(i in 1:length(list)){
  list[[i]]$wave <- i
}
filesagesp <- do.call(rbind, list) %>% pivot_longer(cols=BETA_MALE_MORTALITY:DECAY_SPEED) %>% 
  mutate(wave=as.factor(wave), calibration="age specific") %>% filter(wave=="15")

files <- rbind(filesagest, filesagesp,prior)


ggplot(data=files, aes(x=value, group=calibration, fill=calibration, colour=calibration)) + 
  geom_density(aes(x=value, y=..scaled..,), alpha=0.4, inherit.aes = TRUE) + 
  facet_wrap(~name, scales="free") + theme_bw()
ggsave(paste0("SIMAH_workplace/microsim/2_output_data/calibration_output_agest/plots/lhs_density_compare.png"),
       dpi=300, width=33, height=19, units="cm")
