calculateimplausibility <- function(Cirrhosis,targetdata,N_REPS){
  sim <- do.call(rbind,Cirrhosis) %>% 
    group_by(year, samplenum, seed, microsim.init.sex, agegroup) %>% 
    summarise(poptotal = sum(populationtotal),
              cirrhosistotal = sum(cirrhosistotal),
              cirrhosistotal = ifelse(is.na(cirrhosistotal), 0, cirrhosistotal))
  age2010 <- sim %>% filter(year==2010) %>% 
    ungroup() %>% 
    group_by(year, microsim.init.sex, agegroup) %>% 
    summarise(poptotal = mean(poptotal)) %>% ungroup() %>% 
    group_by(year, microsim.init.sex) %>% 
    mutate(percent = poptotal / sum(poptotal)) %>% ungroup() %>% dplyr::select(microsim.init.sex, agegroup, percent)
  sim <- left_join(sim, age2010) %>% 
    group_by(year, samplenum, seed, microsim.init.sex, agegroup) %>% 
    mutate(weightedrate = (cirrhosistotal/poptotal*100000)*percent) %>% ungroup() %>% 
    group_by(year, samplenum, seed, microsim.init.sex) %>% 
    summarise(microsim = sum(weightedrate)) %>% rename(sex=microsim.init.sex)
  
  meansim <- sim %>% group_by(year, sex, samplenum) %>% summarise(microsim=mean(microsim))
  variance <- sim %>% group_by(year, sex,samplenum) %>% summarise(variance=var(microsim,na.rm=T),
                                                                            variance = ifelse(is.na(variance),0,variance)) %>% ungroup() %>% 
    group_by(year, sex) %>% summarise(meanvariance=mean(variance,na.rm=T),
                                      std.err = sqrt(meanvariance/N_REPS))
  #N_REPS
  # update implausibility to STD ERROR 
  # variance[is.na(variance)] <- 0.01
  target <- targetdata %>% filter(Year<=2010 & Year>=1984) %>% rename(target=agestrate, year=Year) %>% 
    dplyr::select(year, sex, target) %>% mutate(target=ifelse(is.na(target),0,target))
  target <- left_join(target,variance) %>% ungroup() %>% dplyr::select(year, sex, target, meanvariance,std.err)
  meansim <- left_join(meansim, target)
  meansim <- meansim %>% mutate(implausibility=(abs(microsim-target)) / std.err,
                                implausibility = ifelse(implausibility==Inf, 0, implausibility))
  implausibility <- meansim %>% group_by(samplenum) %>% summarise(maximplausibility=max(implausibility,na.rm=T))
  meansim <- meansim %>% dplyr::select(year, sex, samplenum, microsim,target) %>% 
    pivot_longer(cols=microsim:target)
  return(list(implausibility,meansim))
}


