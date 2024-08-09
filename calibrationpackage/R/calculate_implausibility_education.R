#' function to calculate implausibility for education transitions calibration
#' this outputs a TP
#' @param
#' @keywords microsimulation markov model implausibility
#' @export
#' @examples
#' calculate_implausibility_education
calculate_implausibility_education <- function(data, targets, implausability_year){
  data <- data %>%
    mutate(AGECAT = cut(microsim.init.age,
                                       breaks=c(0,24,34,44,54,64,79),
                                       labels=c("18-24","25-34","35-44","45-54",
                                                "55-64","65-79")),
                          SEX = ifelse(microsim.init.sex=="m", "Men","Women"),
                          RACE = recode(microsim.init.race, "BLA"="Black","WHI"="White","SPA"="Hispanic",
                                        "OTH"="Others")) %>%
    rename(EDUC=microsim.init.education, YEAR=year) %>%
    group_by(YEAR, samplenum, seed, SEX, AGECAT,RACE,EDUC) %>% # added seed
    summarise(n=sum(n)) %>%
    ungroup() %>%
    group_by(YEAR, samplenum, seed, SEX, AGECAT, RACE) %>%
    mutate(prop = n/sum(n))

  variance <- data %>%
    group_by(YEAR, samplenum, SEX, EDUC, RACE, AGECAT) %>% # dropped seed
    summarise(variance = var(prop)) %>%
    ungroup() %>%
    group_by(YEAR, SEX, EDUC, RACE, AGECAT) %>%
    summarise(v_s = mean(variance))

  data <- left_join(data,targets) %>%
    mutate(EDUC = factor(EDUC, levels=c("LEHS","SomeC","College")))

  data <- left_join(data, variance)

  implausibility <- data %>%
    # filter(AGECAT=="18-24") %>%
    filter(YEAR<=implausability_year) %>% # implausibility_year = 2019 for pre-COVID calibration
                                          # and 2022 for COVID calibration
    group_by(YEAR, samplenum, SEX,AGECAT, RACE, EDUC) %>%
    summarise(prop = mean(prop),
              target = mean(target),
              SE = mean(SE),
              v_s = mean(v_s),
              # v_o = mean(variance),
              # todo - check implausibility equation in Andrianakis paper
              # should be SE^2?
              implausibility = abs(prop-target)/sqrt(v_s+SE)) %>%
    group_by(samplenum) %>%
    summarise(implausibility=max(implausibility, na.rm=T)) %>%
    ungroup() %>%
    mutate(percentile=ntile(implausibility,100))
  return(implausibility)
}
