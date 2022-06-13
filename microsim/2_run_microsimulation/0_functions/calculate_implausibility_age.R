calculateimplausibility <- function(Cirrhosis,targetdata,N_REPS){
  sim <- do.call(rbind,Cirrhosis) %>% 
    group_by(year, samplenum, seed, microsim.init.sex) %>% 
    mutate(microsim=ifelse(is.na(rateper100000),0,rateper100000)) %>% rename(sex=microsim.init.sex) %>% ungroup()
  meansim <- sim %>% group_by(year, sex, agegroup, samplenum) %>% summarise(microsim=mean(microsim))
  variance <- sim %>% group_by(year, sex, agegroup,samplenum) %>% summarise(variance=var(microsim,na.rm=T),
                                                                            variance = ifelse(is.na(variance),0,variance)) %>% ungroup() %>% 
    group_by(year, sex,agegroup) %>% summarise(meanvariance=mean(variance,na.rm=T),
                                      std.err = sqrt(meanvariance/N_REPS))
  #N_REPS
  # update implausibility to STD ERROR 
  # variance[is.na(variance)] <- 0.01
  target <- targetdata %>% filter(Year<=2010 & Year>=1984) %>% rename(target=rate, year=Year) %>% 
    dplyr::select(year, sex, agegroup, target) %>% mutate(target=ifelse(is.na(target),0,target))
  target <- left_join(target,variance) %>% ungroup() %>% dplyr::select(year, sex, agegroup, target, meanvariance,std.err)
  meansim <- left_join(meansim, target)
  meansim <- meansim %>% mutate(implausibility=(abs(microsim-target)) / std.err,
                                implausibility = ifelse(implausibility==Inf, 0, implausibility))
  implausibility <- meansim %>% group_by(samplenum) %>% summarise(maximplausibility=max(implausibility,na.rm=T))
  meansim <- meansim %>% dplyr::select(year, sex, agegroup, samplenum, microsim,target) %>% 
    pivot_longer(cols=microsim:target)
  return(list(implausibility,meansim))
}


