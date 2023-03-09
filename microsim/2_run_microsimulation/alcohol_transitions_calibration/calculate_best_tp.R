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
  
  outputs <- read_rds("SIMAH_workplace/microsim/2_output_data/Alc_calibration_output_newTP.RDS")
  
  names(outputs) <- 1:5000
  outputs <- outputs[-which(sapply(outputs, is.null))]
  
  # brfsstarget <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_reweighted_upshifted_1984_2020.RDS") %>%
  #   filter(YEAR>=2000) %>%
  #   mutate(agecat = cut(age_var,
  #                                breaks=c(0,20,25,29,39,49,64,100),
  #                                labels=c("18-20","21-25","26-29","30-39","40-49","50-64","65+")),
  #          microsim.init.sex = ifelse(sex_recode=="Male","m","f"),
  #          AlcCAT =  ifelse(gramsperday==0, "Non-drinker",
  #                                                                 ifelse(microsim.init.sex=="m" & gramsperday>0 &
  #                                                                          gramsperday<=40, "Low risk",
  #                                                                        ifelse(microsim.init.sex=="f" & gramsperday>0 &
  #                                                                                 gramsperday<=20, "Low risk",
  #                                                                               ifelse(microsim.init.sex=="m" & gramsperday>40 &
  #                                                                                        gramsperday<=60, "Medium risk",
  #                                                                                      ifelse(microsim.init.sex=="f" & gramsperday>20 &
  #                                                                                               gramsperday<=40, "Medium risk",
  #                                                                                             ifelse(microsim.init.sex=="m" & gramsperday>60,
  #                                                                                                    "High risk",
  #                                                                                                    ifelse(microsim.init.sex=="f" & gramsperday>40,
  #                                                                                                           "High risk", NA)))))))) %>%
  #   group_by(YEAR, microsim.init.sex, agecat, education_summary, AlcCAT) %>%
  #   tally()
  # # 
  # write.csv(brfsstarget, "SIMAH_workplace/microsim/1_input_data/brfss_alcohol_summary_age.csv", row.names=F)
  
  target <- read.csv(paste0(WorkingDirectory,"SIMAH_workplace/microsim/1_input_data/brfss_alcohol_summary.csv")) %>%
    # mutate(AlcCAT=as.factor(AlcCAT), YEAR=as.factor(YEAR),agecat=as.factor(agecat), education_summary=as.factor(education_summary),
    #        race_eth=as.factor(race_eth)) %>%
    mutate(year = as.numeric(as.character(YEAR)),
           year_cat = cut(year,
                          breaks=c(0,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020))) %>% 
    # rename(year=YEAR) %>% 
    group_by(year_cat, microsim.init.sex, race_eth, education_summary, AlcCAT, .drop=FALSE) %>%
    summarise(n=sum(n)) %>% 
    rename(sex=microsim.init.sex, education=education_summary, race=race_eth) %>%
    group_by(year_cat, sex, race, education) %>%
    mutate(targetpercent=n/sum(n),
           targetpercent = ifelse(is.na(targetpercent),0,targetpercent),
           sepercent = sqrt((targetpercent*(1-targetpercent))/sum(n)),
           sepercent = ifelse(is.na(sepercent),0,sepercent),
           lower_ci = targetpercent - (2.576*sepercent),
           upper_ci = targetpercent + (2.576*sepercent)) %>%
    dplyr::select(-c(n))
  
  
  calculate_implausibility <- function(data){
    data <- data %>% 
      mutate(year = as.numeric(as.character(year)),
             year_cat = cut(year,
                            breaks=c(0,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020))) %>% 
      group_by(year_cat, samplenum, microsim.init.sex, microsim.init.education, microsim.init.race, AlcCAT) %>% 
      summarise(n=sum(n)) %>% 
      mutate(microsim.init.race = ifelse(microsim.init.race=="BLA","Black",ifelse(microsim.init.race=="WHI","White",ifelse(microsim.init.race=="SPA","Hispanic","Other")))) %>%
      rename(sex = microsim.init.sex, education=microsim.init.education, race=microsim.init.race) %>% 
      group_by(year_cat, samplenum, sex, education, race) %>% 
      mutate(simulatedpercent=n/sum(n)) %>% 
      dplyr::select(-n)
    
    implausibility <- left_join(data, target, by=c("year_cat","sex","education","race","AlcCAT")) %>% 
      mutate(v_m =0.1,
             v_t = sepercent^2,
             absdiff = abs(simulatedpercent - targetpercent),
             implausibility = ifelse(simulatedpercent>=lower_ci & simulatedpercent<=upper_ci, 0,
                                     abs(simulatedpercent - targetpercent)/sqrt(v_m + v_t))) %>% 
      # group_by(samplenum, sex, race, education, AlcCAT)  %>% 
      # summarise(implausibility = mean(implausibility, na.rm=T)) %>% 
      group_by(samplenum, sex, race, education) %>% 
      summarise(implausibility = mean(implausibility, na.rm=T))
    return(implausibility)
  }
  
  implausibility <- lapply(outputs, calculate_implausibility)
  implausibility <- do.call(rbind, implausibility) %>% 
    group_by(sex, education, race) %>% 
    mutate(rank = ntile(implausibility, 4000))
  
  # calculate_ks <- function(data){
  #   data <- data %>% 
  #     mutate(year = as.numeric(as.character(year)),
  #            year_cat = cut(year,
  #                           breaks=c(0,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020))) %>% 
  #     group_by(year_cat, samplenum, microsim.init.sex, microsim.init.education, microsim.init.race, AlcCAT) %>% 
  #     summarise(n=sum(n)) %>% 
  #     mutate(microsim.init.race = ifelse(microsim.init.race=="BLA","Black",ifelse(microsim.init.race=="WHI","White",ifelse(microsim.init.race=="SPA","Hispanic","Other")))) %>%
  #     rename(sex = microsim.init.sex, education=microsim.init.education, race=microsim.init.race) %>% 
  #     group_by(year_cat, samplenum, sex, education, race) %>% 
  #     mutate(simulatedpercent=n/sum(n)) %>% 
  #     dplyr::select(-n)
  #   
  #   
  #   kstest <- function(subset){
  #     test <- ks.test(subset$simulatedpercent, subset$targetpercent)
  #     subset$statistic <- as.numeric(test$statistic)
  #     subset$p <- test$p.value
  #     subset <- data.frame(subset)
  #     return(subset)
  #   }
  #   
  #   ks <- left_join(data, target, by=c("year_cat","sex","education","race","AlcCAT")) %>% 
  #     group_by(samplenum, sex, education, race, AlcCAT) %>% 
  #     do(kstest(.)) %>% ungroup() %>% 
  #     dplyr::select(samplenum, sex, education, race, statistic, p) %>% 
  #     group_by(samplenum, sex, race, education) %>% distinct()
  #   return(ks)
  # }
  # 
  # kolmogorov <- lapply(outputs, calculate_ks)
  #   
  # kolmogorov <- do.call(rbind, kolmogorov)
  # 
  # ks <- kolmogorov %>% ungroup() %>% 
  #   group_by(sex, education, race) %>% 
  #   mutate(rank = ntile(p, 5000))
  
  # now match with the original TPs to get a "hybrid best TP"
  TPs <- read_rds("SIMAH_workplace/microsim/1_input_data/transitionslist_newTP.RDS")
  
  names <- names(outputs)
  
  # remove the runs
  TPs <- TPs[names]
  names(TPs)==names
  
  best_output <- implausibility %>% filter(rank==1)
  
  # best_ks_output <- ks %>% filter(rank==5000)
  
  # now subset the best TPs
  best <- as.numeric(unique(best_output$samplenum))
  # best_ks <- as.numeric(unique(best_ks_output$samplenum))
  
  
  bestTP <- TPs[best]
  # bestTP <- TPs[best_ks]
  
  
  for(i in names(bestTP)){
    bestTP[[i]]$samplenum <- i
  }
  
  bestTP <- do.call(rbind,bestTP) %>% 
    mutate(sex = ifelse(grepl("f",cat),"f","m"),
           race = ifelse(grepl("BLA",cat),"Black",
                         ifelse(grepl("WHI",cat),"White",
                                ifelse(grepl("SPA",cat), "Hispanic",
                                       ifelse(grepl("_OTH_", cat), "Other",NA)))),
           education = ifelse(grepl("LEHS",cat),"LEHS",
                              ifelse(grepl("SomeC",cat),"SomeC","College")))
  
  best_output <- implausibility %>% filter(rank==1) %>% 
    rename(topsample=samplenum) %>% 
    dplyr::select(sex, education, race, topsample)
  
  # best_output <- ks %>% filter(rank==5000) %>% 
  #   rename(topsample=samplenum) %>% 
  #   dplyr::select(sex, education, topsample)
  
  
  bestTP <- left_join(bestTP, best_output)
  
  bestTP <- bestTP %>% 
    mutate(match = ifelse(topsample==samplenum,1,0)) %>% 
    filter(match==1)
  
  
  bestTP_output <- bestTP %>% ungroup() %>% 
    dplyr::select(cat, StateTo, cumsum)
  
  saveRDS(bestTP_output,"SIMAH_workplace/microsim/1_input_data/calibrated_newTP_mean.RDS")
  