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
    group_by(year, samplenum, sex, race, agecat, education, alc_cat) %>%
    summarise(variance = var(propsimulation)) %>%
    ungroup() %>%
    group_by(year, sex, race, agecat, education, alc_cat) %>%
    summarise(v_s = mean(variance, na.rm=T),
              v_s = ifelse(is.na(v_s), 1e-7, v_s))

  # get rid of grouping by seed
  data <- data %>%
    group_by(year, samplenum, sex, race, agecat, education, alc_cat) %>%
    summarise(propsimulation = mean(propsimulation, na.rm=T),
              proptarget = mean(proptarget, na.rm=T),
              se = mean(se, na.rm=T))

  data <- left_join(data, variance, by=c("year","sex","race","agecat","education","alc_cat"))
  #
  implausibility <- data %>%
    group_by(year, samplenum, sex, race, education, agecat,
             alc_cat) %>%
    # filter(race!="OTH") %>%
    summarise(propsimulation = mean(propsimulation),
              proptarget = mean(proptarget),
              se = mean(se),
              v_s = mean(v_s),
              # v_o = mean(variance),
              # todo - check implausibility equation in Andrianakis paper
              # should be SE^2?
              implausibility = abs(propsimulation-proptarget)/sqrt(v_s+se^2)) %>%
    group_by(samplenum) %>%
    summarise(mean = mean(implausibility, na.rm=T),
              max = max(implausibility, na.rm=T))
  return(implausibility)
}
