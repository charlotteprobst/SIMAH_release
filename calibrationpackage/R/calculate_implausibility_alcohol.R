#' function to calculate implausibility for education transitions calibration
#' this outputs a TP
#' @param
#' @keywords microsimulation markov model implausibility
#' @export
#' @examples
#' calculate_implausibility_education
calculate_implausibility_alcohol<- function(data, targets){

  # calculate variance for implausibility equation
  variance <- data %>%
    group_by(year, samplenum, microsim.init.sex, microsim.init.race, agecat, microsim.init.education, AlcCAT) %>%
    summarise(variance = var(propsimulation)) %>%
    ungroup() %>%
    group_by(year, microsim.init.sex, microsim.init.race, agecat, microsim.init.education, AlcCAT) %>%
    summarise(v_s = mean(variance, na.rm=T),
              v_s = ifelse(is.na(v_s), 1e-7, v_s))

  # get rid of grouping by seed
  data <- data %>%
    group_by(year, samplenum, microsim.init.sex, microsim.init.race, agecat, microsim.init.education, AlcCAT) %>%
    summarise(propsimulation = mean(propsimulation))

  data <- left_join(data,targets)

  data <- left_join(data, variance)
  #
  implausibility <- data %>%
    group_by(year, samplenum, microsim.init.sex, microsim.init.race, microsim.init.education, agecat,
             AlcCAT) %>%
    # filter(microsim.init.race!="OTH") %>%
    summarise(propsimulation = mean(propsimulation),
              proptarget = mean(proptarget),
              se = mean(se),
              v_s = mean(v_s),
              # v_o = mean(variance),
              # todo - check implausibility equation in Andrianakis paper
              # should be SE^2?
              implausibility = abs(propsimulation-proptarget)/sqrt(v_s+se^2))
  # %>%
  #   group_by(samplenum) %>%
  #   summarise(implausibility=max(implausibility, na.rm=T)) %>%
  #   ungroup() %>%
  #   mutate(percentile=ntile(implausibility,100))
  return(implausibility)
}
