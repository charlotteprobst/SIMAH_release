#' function to calculate implausibility for mortality calibration
#' this outputs a TP
#' @param
#' @keywords microsimulation markov model implausibility
#' @export
#' @examples
#' calculate_implausibility_mortality
calculate_implausibility_mortality<- function(data, agest=0, agestyear=2010, model_error){
  data <- data %>%
    group_by(year, samplenum, seed, sex, agecat, education,cause) %>%
    summarise(popcount=sum(popcount),
              simulated_mortality=sum(simulated_mortality_n),
              observed_mortality=sum(observed_mortality_n),
              simulated_mortality_rate = (simulated_mortality/popcount) * 100000,
              observed_mortality_rate = (observed_mortality/popcount) *100000)
  if(agest==0){
  # calculate variance for implausibility equation
  variance <- data %>%
    group_by(year, samplenum, sex,agecat,education,cause) %>%
    summarise(variance = var(simulated_mortality_rate)) %>%
    ungroup() %>%
    group_by(year, sex,agecat, education, cause) %>%
    summarise(v_s = mean(variance, na.rm=T))

  # remove grouping by seed - take average
  data <- data %>%
    group_by(year,samplenum, sex, agecat, education, cause) %>%
    summarise(simulated_mortality_rate = mean(simulated_mortality_rate),
              observed_mortality_rate = mean(observed_mortality_rate))

# join with variance
  data <- left_join(data, variance, by=c("year","sex","agecat","education","cause"))

  # add a model discrepancy term - adjustable parameter
  data$v_m_rel <- data$observed_mortality_rate*model_error
  data$v_m_abs <- 1
  #
  implausibility <- data %>%
    group_by(year, samplenum, sex, agecat, education, cause) %>%
    mutate(implausibility_relative = abs(simulated_mortality_rate-observed_mortality_rate)/sqrt(v_s+v_m_rel),
           implausibility_absolute = abs(simulated_mortality_rate-observed_mortality_rate)/sqrt(v_s+v_m_abs)) %>%
    group_by(samplenum) %>%
    summarise(mean = mean(implausibility, na.rm=T),
              max = max(implausibility, na.rm=T))
  }
  if(agest==1){
    causes <- unique(data$cause)
    agest <- data %>% filter(year==agestyear) %>%
      filter(samplenum==1) %>% filter(cause==causes[1]) %>%
      ungroup() %>%
      group_by(year, sex, agecat, education) %>%
      summarise(totalpop = sum(popcount)) %>% ungroup() %>%
      group_by(year, sex) %>%
      mutate(percent = totalpop / sum(totalpop)) %>% ungroup() %>% dplyr::select(sex,
                                                                                 agecat, education,
                                                                                 percent)
    data <- left_join(data, agest, by=c("sex","agecat","education"))

    # age standardise the mortality rates
    # weight each age group by multiplying by the proportion of people in that category
    # sum over age groups for each category to get age standardised rates
    data <- data %>%
      group_by(year,samplenum,seed,sex,agecat,education,cause) %>%
      mutate(simulated_mortality_rate_weighted = simulated_mortality_rate*percent,
             observed_mortality_rate_weighted = observed_mortality_rate*percent) %>%
      ungroup() %>%
      group_by(year,samplenum,seed,sex,education,cause) %>%
      summarise(agest_simulated_mortality_rate = sum(simulated_mortality_rate_weighted),
                agest_observed_mortality_rate = sum(observed_mortality_rate_weighted))

    variance <- data %>%
      group_by(year, samplenum, sex,education,cause) %>%
      summarise(variance = var(agest_simulated_mortality_rate)) %>%
      ungroup() %>%
      group_by(year, sex, education, cause) %>%
      summarise(v_s = mean(variance, na.rm=T)) #v_s = variance due to stochasticity

    # remove grouping by seed - take average
    data <- data %>%
      group_by(year,samplenum, sex, education, cause) %>%
      summarise(agest_simulated_mortality_rate = mean(agest_simulated_mortality_rate),
                agest_observed_mortality_rate = mean(agest_observed_mortality_rate))

    # join with variance
    data <- left_join(data, variance, by=c("year","sex","education","cause"))

    # add a model discrepancy term
    data$v_m_rel <- data$agest_observed_mortality_rate*model_error
    data$v_m_abs <- 1

    implausibility <- data %>%
      group_by(year, samplenum, sex, education, cause) %>%
      mutate(implausibility_orig = abs(agest_simulated_mortality_rate-agest_observed_mortality_rate)/sqrt(v_s),
        implausibility_rel = abs(agest_simulated_mortality_rate-agest_observed_mortality_rate)/sqrt(v_s+v_m_rel),
             implausibility_abs = abs(agest_simulated_mortality_rate-agest_observed_mortality_rate)/sqrt(v_s+v_m_abs)) %>%
      group_by(samplenum) %>%
      summarise(mean = mean(implausibility, na.rm=T),
                max = max(implausibility, na.rm=T))

  }
  return(list(implausibility,data))
}
