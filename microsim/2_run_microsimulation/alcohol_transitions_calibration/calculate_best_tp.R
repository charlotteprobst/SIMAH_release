  # calculate best fitting TPs for alcohol use 
  
  rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
  
  library(devtools)
  library(roxygen2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(fitdistrplus)
  library(lhs)
  library(truncnorm)
  library(foreach)
  library(doParallel)
  library(ggplot2)
  options(dplyr.summarise.inform = FALSE)
  
  WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
  setwd(WorkingDirectory)
  
  implausibility <- read.csv("SIMAH_workplace/microsim/2_output_data/Alc_calibration_implausibility.csv") %>% 
    mutate(best = ntile(implausibility, nrow(.)),
           samplenum = as.numeric(samplenum)) %>% filter(best==1)
  
  bestsample <- as.numeric(implausibility$samplenum[1])
  
  
  outputs <- read_rds("SIMAH_workplace/microsim/2_output_data/Alc_calibration_output.RDS")
  
  names(outputs) <- 1:5000
  outputs <- outputs[-which(sapply(outputs, is.null))]
  
  target <- read.csv(paste0(WorkingDirectory,"SIMAH_workplace/microsim/1_input_data/brfss_alcohol_summary.csv")) %>%
    group_by(YEAR, microsim.init.sex, education_summary, AlcCAT) %>%
    summarise(n=sum(n)) %>% rename(sex=microsim.init.sex, year=YEAR, education=education_summary) %>%
    group_by(year, sex, education) %>%
    mutate(targetpercent=n/sum(n),
           sepercent = sqrt((targetpercent*(1-targetpercent))/sum(n)),
           lower_ci = targetpercent - (2.576*sepercent),
           upper_ci = targetpercent + (2.576*sepercent)) %>%
    dplyr::select(-c(n))
  
  
  calculate_implausibility <- function(data){
    data <- data %>% 
      group_by(year, samplenum, microsim.init.sex, microsim.init.education, AlcCAT) %>% 
      summarise(n=sum(n)) %>% 
      mutate(year = as.integer(as.character(year))) %>% 
      rename(sex = microsim.init.sex, education=microsim.init.education) %>% 
      group_by(year, samplenum, sex, education) %>% 
      mutate(simulatedpercent=n/sum(n)) %>% 
      dplyr::select(-n)
    
    implausibility <- left_join(data, target, by=c("year","sex","education","AlcCAT")) %>% 
      mutate(v_m =0.1,
             v_t = sqrt(sepercent^2),
             absdiff = abs(simulatedpercent - targetpercent),
             implausibility = ifelse(simulatedpercent>=lower_ci & simulatedpercent<=upper_ci, 0,
                                     abs(simulatedpercent - targetpercent)/(v_m + v_t))) %>% 
      group_by(samplenum)  %>% 
      summarise(implausibility = mean(implausibility))
    return(implausibility)
  }
  
  implausibility <- lapply(outputs, calculate_implausibility)
  implausibility <- do.call(rbind, implausibility) %>% 
    mutate(rank = ntile(implausibility, nrow(.)))
  best_output <- as.numeric(subset(implausibility, rank==1)$samplenum)
    
  
  output <- outputs[[best_output]]
  

  output <- output %>% 
    group_by(year, samplenum, microsim.init.sex, microsim.init.education, AlcCAT) %>%
    summarise(n=sum(n)) %>%
    mutate(year = as.integer(as.character(year))
           # microsim.init.race = ifelse(microsim.init.race=="BLA","Black",
           #                             ifelse(microsim.init.race=="WHI","White",
           #                                    ifelse(microsim.init.race=="SPA","Hispanic",
           #                                           ifelse(microsim.init.race=="OTH","Other", NA))))
           ) %>%
    rename(sex=microsim.init.sex, education=microsim.init.education) %>% 
    group_by(year, samplenum, sex, education) %>%
    mutate(simulatedpercent=n/sum(n)) %>%
    dplyr::select(-n)
  
  compare <- left_join(output, target) %>% 
    dplyr::select(year, sex, education, AlcCAT, simulatedpercent, targetpercent, lower_ci, upper_ci) %>% 
    pivot_longer(simulatedpercent:targetpercent) %>% 
    mutate(education = factor(education, levels=c("LEHS","SomeC","College")),
           AlcCAT = factor(ifelse(AlcCAT=="Low risk", "Category I",
                                  ifelse(AlcCAT=="Medium risk", "Category II",
                                         ifelse(AlcCAT=="High risk","Category III", AlcCAT))),
                           levels=c("Category I","Category II","Category III","Non-drinker")))
  compare <- compare %>% pivot_wider(names_from=name, values_from=value) 

ggplot(data=subset(compare, sex=="f"), aes(x=year, y=simulatedpercent, colour=as.factor(samplenum), fill=as.factor(samplenum))) + geom_line(size=1) + 
  geom_line(aes(x=year, y=targetpercent), colour="black", size=2) + 
  facet_grid(cols=vars(education), rows=vars(AlcCAT), scales="free") + 
  # geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci)) + 
  scale_y_continuous(labels=scales::percent, limits=c(0,NA)) + theme_bw() + 
  theme(legend.position="none") + 
  ggtitle("Women")

ggplot(data=subset(compare, sex=="m"), aes(x=year, y=simulatedpercent, colour=as.factor(samplenum), fill=as.factor(samplenum))) + geom_line(size=1) + 
  geom_line(aes(x=year, y=targetpercent), colour="black", size=2) + 
  facet_grid(cols=vars(education), rows=vars(AlcCAT), scales="free") + 
  # geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci)) + 
  scale_y_continuous(labels=scales::percent, limits=c(0,NA)) + theme_bw() + 
  theme(legend.position="none") + 
  ggtitle("Men")
