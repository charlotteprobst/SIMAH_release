calculateimplausibility <- function(Cirrhosis,sex,N_REPS){
  # source("preprocessing_scripts/process_target_calibration.R")
  source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/process_target_calibration_hosp.R")
  # OR 
  selectedSex <- sex
  sim <- do.call(rbind,Cirrhosis) %>% 
    group_by(Year, samplenum, seed, microsim.init.sex) %>% 
    mutate(microsim=sum(n)/100) %>% rename(sex=microsim.init.sex) %>% ungroup()
  meansim <- sim %>% group_by(Year, sex, samplenum) %>% summarise(microsim=mean(microsim))
  variance <- sim %>% group_by(Year, sex, samplenum) %>% summarise(variance=var(microsim,na.rm=T)) %>% ungroup() %>% 
    group_by(Year, sex) %>% summarise(meanvariance=mean(variance,na.rm=T),
                                      std.err = sqrt(meanvariance/N_REPS))
  #N_REPS
  # update implausibility to STD ERROR 
  # variance[is.na(variance)] <- 0.01
  target <- target %>% filter(Year<=2010) %>% rename(target=PE) %>% 
    mutate(target=target/100)
  target <- left_join(target,variance) %>% ungroup() %>% dplyr::select(Year, sex, target,Lower,Upper, meanvariance,std.err) %>% 
    mutate(Lower=Lower/100, Upper=Upper/100)
  meansim <- left_join(meansim, target)
  if(sex!="b"){
  meansim <- meansim %>% mutate(implausibility=(abs(microsim-target)) / std.err) %>% 
    filter(sex==selectedSex)
  }else{
  meansim <- meansim %>% mutate(implausibility=(abs(microsim-target)) / std.err) 
  }
  return(meansim)
}


