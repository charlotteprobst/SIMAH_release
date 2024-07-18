#' function to calculate implausibility for alcohol continuous calibration
#' this outputs a TP
#' @param
#' @keywords microsimulation markov model implausibility
#' @export
#' @examples
#' calculate_implausibility_alcohol_mean
calculate_implausibility_alcohol_mean <- function(data, brfss){

  # calculate variance for implausibility equation
  variance <- data %>%
    group_by(year, samplenum, microsim.init.sex, microsim.init.race, agecat, microsim.init.education) %>%
    summarise(variance = var(meansimulation)) %>%
    ungroup() %>%
    group_by(year, microsim.init.sex, microsim.init.race, agecat, microsim.init.education) %>%
    summarise(v_s = mean(variance, na.rm=T),
              v_s = ifelse(is.na(v_s), 1e-7, v_s))

  target <- brfss %>%
    mutate(agecat=cut(microsim.init.age,
                      breaks=c(0,24,64,100),
                      labels=c("18-24","25-64","65+"))) %>%
    group_by(YEAR, microsim.init.sex, agecat, microsim.init.education, microsim.init.race) %>%
    filter(microsim.init.alc.gpd!=0) %>%
    summarise(meanbrfss = mean(microsim.init.alc.gpd),
              se = std.error(microsim.init.alc.gpd)) %>%
    rename(year=YEAR)

  data <- left_join(data, target, by=c("year","microsim.init.sex","agecat","microsim.init.education","microsim.init.race"))

  # get rid of grouping by seed
  data <- data %>%
    group_by(year, samplenum, microsim.init.sex, microsim.init.race, agecat, microsim.init.education) %>%
    summarise(meansimulation = mean(meansimulation, na.rm=T),
              meantarget = mean(meanbrfss, na.rm=T),
              se = mean(se, na.rm=T))

  data <- left_join(data, variance, by=c("year","microsim.init.sex","microsim.init.race","agecat","microsim.init.education"))

  data <- data %>% mutate(microsim.init.education = ifelse(agecat=="18-24" & microsim.init.education=="College","SomeC",
                                                           microsim.init.education))


  #
  implausibility <- data %>%
    group_by(year, samplenum, microsim.init.sex, microsim.init.race, microsim.init.education, agecat) %>%
    # filter(microsim.init.race!="OTH") %>%
    summarise(meansimulation = mean(meansimulation),
              meantarget = mean(meantarget),
              se = mean(se),
              v_s = mean(v_s),
              # v_o = mean(variance),
              # todo - check implausibility equation in Andrianakis paper
              # should be SE^2?
              implausibility = abs(meansimulation-meantarget)/sqrt(v_s+se^2)) %>%
    # group_by(samplenum, microsim.init.sex, microsim.init.race, microsim.init.education, agecat) %>%
    # summarise(maximplausibility = max(implausibility),
    #           meanimplausibility = mean(implausibility))
    group_by(samplenum) %>%
    filter(year!=2000) %>%
    summarise(mean = mean(implausibility, na.rm=T),
              max = max(implausibility, na.rm=T))
  return(implausibility)
}
