# comparing the calibrated parameters from the final wave of age standardised versus non age st
# now plot the lhs samples 
setwd("~/Google Drive/SIMAH Sheffield/")
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

files <- (Sys.glob(paste("SIMAH_workplace/microsim/2_output_data/calibration_output_decay/Cirrhosis_output*.RDS", sep="")))
index <- c(1,10,11,12,13,14,15,2,3,4,5,6,7,8,9)
# files
files <- files[order(index)]
list <- lapply(files, function(x) read_rds(x)) 
meansim <- list()
for(i in 1:length(list)){
  list[[i]] <- do.call(rbind, list[[i]])
  age2010 <- list[[i]] %>% filter(year==2010) %>% 
    ungroup() %>% 
    group_by(year, microsim.init.sex, agegroup) %>% 
    summarise(poptotal = mean(populationtotal)) %>% ungroup() %>% 
    group_by(year, microsim.init.sex) %>% 
    mutate(percent = poptotal / sum(poptotal)) %>% ungroup() %>% dplyr::select(microsim.init.sex, agegroup, percent)
  
  list[[i]] <- list[[i]] %>% group_by(year, microsim.init.sex, agegroup)
  
  sim <- left_join(list[[i]], age2010) %>% 
    mutate(cirrhosistotal = ifelse(is.na(cirrhosistotal),0, cirrhosistotal)) %>% 
    group_by(year, samplenum, seed, microsim.init.sex, agegroup) %>% 
    mutate(weightedrate = (cirrhosistotal/populationtotal*100000)*percent) %>% ungroup() %>% 
    group_by(year, samplenum, seed, microsim.init.sex) %>% 
    summarise(microsim = sum(weightedrate)) %>% rename(sex=microsim.init.sex) %>% 
    group_by(year, samplenum, sex) %>% 
    mutate(variance = var(microsim,na.rm=T),
           variance = ifelse(is.na(variance),0.001,variance))
  
  meansim[[i]] <- sim %>% group_by(year, sex, samplenum) %>% 
    summarise(microsim=mean(microsim,na.rm=T),
              meanvariance = mean(variance,na.rm=T),
              se = sqrt(meanvariance /2 )) %>% rename(Year=year)
  
  meansim[[i]] <- left_join(meansim, cirrhosismortality_agest) %>% 
    rename(target=agestrate) %>% 
    mutate(sex=ifelse(sex=="m","Men","Women")) %>% 
    mutate(difference = abs(microsim-target),
           pct_difference = round(difference / ((microsim+target)/2)*100,digits=2))
}
