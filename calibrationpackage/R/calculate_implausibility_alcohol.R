#' function to calculate implausibility for education transitions calibration
#' this outputs a TP
#' @param
#' @keywords microsimulation markov model implausibility
#' @export
#' @examples
#' calculate_implausibility_alcohol
calculate_implausibility_alcohol <- function(data, targets){
  compare <- data %>%
    mutate(year_cat = cut(year,
                           breaks=c(0,2003,2006,2009,2012,2015,2018,2021),
                           labels=c("2000-2003","2004-2006","2007-2009","2010-2012","2013-2015",
                                    "2016-2018","2019-2021"))) %>%
    group_by(year_cat, samplenum, microsim.init.sex,microsim.init.race,agecat,
             microsim.init.education, AlcCAT) %>%
    summarise(n=mean(n)) %>%
    ungroup() %>%
    group_by(year_cat, samplenum, microsim.init.sex, microsim.init.race,agecat,
             microsim.init.education) %>%
    mutate(propsimulation = n/sum(n))

  variance <- data %>%
    mutate(year_cat = cut(year,
                          breaks=c(0,2003,2006,2009,2012,2015,2018,2021),
                          labels=c("2000-2003","2004-2006","2007-2009","2010-2012","2013-2015",
                                   "2016-2018","2019-2021"))) %>%
    group_by(year_cat, samplenum, seed, microsim.init.sex,microsim.init.race,agecat,
             microsim.init.education, AlcCAT) %>%
    summarise(mean = mean(n)) %>% ungroup() %>%
    group_by(year_cat, samplenum, microsim.init.sex,microsim.init.race,agecat,
             microsim.init.education) %>%
    mutate(prop = mean/sum(mean)) %>% ungroup() %>%
    group_by(year_cat, samplenum, microsim.init.sex, microsim.init.race, agecat, microsim.init.education,
             AlcCAT) %>%
    summarise(variance = var(prop)) %>%
    ungroup() %>%
    group_by(year_cat, microsim.init.sex, microsim.init.race, agecat, microsim.init.education,
             AlcCAT) %>%
    summarise(v_s = mean(variance, na.rm=T))

  compare <- left_join(compare,targets)

  compare <- left_join(compare, variance)

  compare <- compare %>%
    dplyr::select(-c(n,se,v_s,type)) %>%
    pivot_longer(propsimulation:proptarget) %>%
    group_by(year,samplenum,microsim.init.sex,microsim.init.race, agecat, microsim.init.education,
             AlcCAT,name) %>%
    summarise(value=mean(value, na.rm=T))
  #
  implausibility <- compare %>%
    # filter(AGECAT=="18-24") %>%
    # filter(year<=2019) %>%
    group_by(year_cat, samplenum, microsim.init.sex, microsim.init.race, microsim.init.education, agecat,
             AlcCAT) %>%
    summarise(propsimulation = mean(propsimulation),
              proptarget = mean(proptarget),
              se = mean(se),
              v_s = mean(v_s),
              # v_o = mean(variance),
              # todo - check implausibility equation in Andrianakis paper
              # should be SE^2?
              implausibility = abs(propsimulation-proptarget)/sqrt(v_s+se^2)) %>%
    group_by(samplenum) %>%
    summarise(implausibility=max(implausibility, na.rm=T)) %>%
    ungroup() %>%
    mutate(percentile=ntile(implausibility,100))
  return(implausibility)
}
