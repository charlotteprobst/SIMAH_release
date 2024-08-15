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
    group_by(year, samplenum, sex, race, agecat, education, alc_cat) %>%
    summarise(variance = var(meansimulation)) %>%
    ungroup() %>%
    group_by(year, sex, race, agecat, education, alc_cat) %>%
    summarise(v_s = mean(variance, na.rm=T),
              v_s = ifelse(is.na(v_s), 1e-7, v_s))

  target <- brfss %>%
    mutate(agecat=cut(age,
                      breaks=c(0,24,64,100),
                      labels=c("18-24","25-64","65+"))) %>%
    group_by(YEAR, sex, agecat, education, race, alc_cat) %>%
    filter(alc_gpd!=0) %>%
    summarise(meanbrfss = mean(alc_gpd),
              se = std.error(alc_gpd)) %>%
    rename(year=YEAR)

  data <- left_join(data, target, by=c("year","sex","agecat","education","race","alc_cat"))

  # get rid of grouping by seed
  data <- data %>%
    group_by(year, samplenum, sex, race, agecat, education, alc_cat) %>%
    summarise(meansimulation = mean(meansimulation, na.rm=T),
              meantarget = mean(meanbrfss, na.rm=T),
              se = mean(se, na.rm=T))

  data <- left_join(data, variance, by=c("year","sex","race","agecat","education","alc_cat"))

  data <- data %>% mutate(education = ifelse(agecat=="18-24" & education=="College","SomeC",
                                                           education))

  implausibility <- data %>%
    group_by(year, samplenum, sex, race, education, agecat, alc_cat) %>%
    # filter(race!="OTH") %>%
    summarise(meansimulation = mean(meansimulation, na.rm=T),
              meantarget = mean(meantarget, na.rm=T),
              se = mean(se, na.rm=T),
              v_s = mean(v_s, na.rm=T),
              # v_o = mean(variance),
              # todo - check implausibility equation in Andrianakis paper
              # should be SE^2?
              implausibility = abs(meansimulation-meantarget)/sqrt(v_s+se^2)) %>%
    group_by(samplenum, sex, race, education, agecat, alc_cat) %>%
    filter(sex=="m") %>% filter(agecat=="65+") %>% filter(alc_cat=="Low risk") %>%
    # group_by(samplenum) %>%
    filter(year!=2000) %>%
    summarise(mean = mean(implausibility, na.rm=T),
              max = max(implausibility, na.rm=T))
  return(implausibility)
}
