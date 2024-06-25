#' Transitions alcohol consumption of individuals - through regression model
#' @param
#' @keywords microsimulation, alcohol
#' @export
#' @examples
#' transition_alcohol_regression
transition_alcohol_regression <- function(data, model,y){
 # create a dataset with the required variables dummy coded
  data_prediction <- data %>%
    mutate(agecat = cut(microsim.init.age,
                        breaks=c(0,24,64,100),
                        labels=c("18-24","25-64","65+")),
           race = case_when(microsim.init.race=="BLA" ~ "Black, non-Hispanic",
                            microsim.init.race=="WHI" ~ "White, non-Hispanic",
                            microsim.init.race=="OTH" ~ "Other, non-Hispanic",
                            microsim.init.race=="SPA" ~ "Hispanic"),
           sex = case_when(microsim.init.sex=="f" ~ "Women",
                           microsim.init.sex=="m" ~ "Men"),
           ed = case_when(microsim.init.education=="LEHS" ~ "Low",
                          microsim.init.education=="SomeC" ~ "Med",
                          microsim.init.education=="College" ~ "High"),
           ed = ifelse(ed=="High" & agecat=="18-24", "Med",ed),
           strata = paste(agecat, sex, race, ed, sep="_"),
           microsim.init.alc.gpd = ceiling(microsim.init.alc.gpd),
           abstainer = ifelse(AlcCAT=="Non-drinker" & formerdrinker==0, 1,0)) %>%
    dplyr::select(strata, brfssID, agecat, microsim.init.sex,
                  microsim.init.race, microsim.init.education,
                  microsim.init.alc.gpd,
                  AlcCAT, formerdrinker) %>%
    mutate(Women = ifelse(microsim.init.sex=="f", 1,0),
           age2564 = ifelse(agecat=="25-64", 1,0),
           age65 = ifelse(agecat=="65+", 1,0),
           raceblack = ifelse(microsim.init.race=="BLA",1,0),
           racehispanic = ifelse(microsim.init.race=="SPA",1,0),
           raceother = ifelse(microsim.init.race=="OTH",1,0),
           edulow = ifelse(microsim.init.education=="LEHS", 1,0),
           edumed = ifelse(microsim.init.education=="SomeC", 1,0),
           formerdrinker = ifelse(formerdrinker==1,1,0),
           abstainer = ifelse(microsim.init.alc.gpd==0, 1,0),
           cat1 = ifelse(AlcCAT=="Low risk", 1,0),
           cat2 = ifelse(AlcCAT=="Medium risk", 1,0),
           cat3 = ifelse(AlcCAT=="High risk", 1,0),
           alc_scaled = (microsim.init.alc.gpd-mean(microsim.init.alc.gpd)) / sd(microsim.init.alc.gpd))

  intercepts <- model %>% filter(parameter=="(Intercept)") %>%
    filter(type=="abstainermod") %>%
    pivot_wider(names_from=type, values_from=Estimate) %>%
    dplyr::select(-parameter)

  data_prediction <- left_join(data_prediction, intercepts,
                               by=c("strata"))

  zero_part_coef <- model %>% filter(type=="abstainermod") %>%
    filter(parameter!="(Intercept)") %>%
    # filter(year==y) %>%
    dplyr::select(parameter, Estimate) %>% distinct() %>%
    pivot_wider(names_from=parameter, values_from=Estimate)

  time <- y-2001

  names(zero_part_coef) <- ifelse(names(zero_part_coef)=="alc_rounded_1", "alc_daily_g_1", names(zero_part_coef))

  zero_part_lp <- data_prediction$abstainermod +
    as.numeric(zero_part_coef['time'])*time +
    # as.numeric(zero_part_coef['age3_225-64']) * data_prediction$age2564 +
    # as.numeric(zero_part_coef['age3_265+']) * data_prediction$age65 +
    # as.numeric(zero_part_coef['female.factor_2Women']) * data_prediction$Women +
    # as.numeric(zero_part_coef['race.factor_2Black, non-Hispanic']) * data_prediction$raceblack +
    # as.numeric(zero_part_coef['race.factor_2Hispanic']) * data_prediction$racehispanic +
    # as.numeric(zero_part_coef['race.factor_2Other, non-Hispanic']) * data_prediction$raceother +
    # as.numeric(zero_part_coef['edu3_2Low']) * data_prediction$edulow +
    # as.numeric(zero_part_coef['edu3_2Med']) * data_prediction$edumed
    as.numeric(zero_part_coef['abstainer_1']) * data_prediction$abstainer +
    as.numeric(zero_part_coef['cat1_lag']) * data_prediction$cat1
    # as.numeric(zero_part_coef['cat2_lag']) * data_prediction$cat2
    # as.numeric(zero_part_coef['age3_225-64:abstainer_1']) * data_prediction$age2564*data_prediction$abstainer +
    # as.numeric(zero_part_coef['age3_265+:abstainer_1']) * data_prediction$age65*data_prediction$abstainer +
    # as.numeric(zero_part_coef['age3_225-64:cat1_lag']) * data_prediction$age2564*data_prediction$cat1 +
    # as.numeric(zero_part_coef['age3_265+:cat1_lag']) * data_prediction$age65*data_prediction$cat1 +
    # as.numeric(zero_part_coef['age3_225-64:cat2_lag']) * data_prediction$age2564*data_prediction$cat2 +
    # as.numeric(zero_part_coef['age3_265+:cat2_lag']) * data_prediction$age65*data_prediction$cat2

  # Compute the probability of zero using the logistic function
  zero_prob <- 1 / (1+exp(-zero_part_lp))

  # now match individual intercepts to the count data
  intercepts <- model %>% filter(parameter=="(Intercept)") %>%
    filter(type=="drinkermod") %>%
    pivot_wider(names_from=type, values_from=Estimate) %>%
    dplyr::select(-c(parameter,strata)) %>%
    mutate(rank = ntile(drinkermod, nrow(.)))

  # assign each person a random intercept - based on their alcohol use
  data_prediction <- data_prediction %>%
    arrange(microsim.init.alc.gpd) %>%
    mutate(rank = ntile(microsim.init.alc.gpd, nrow(intercepts)))

  data_prediction <- left_join(data_prediction,intercepts, by=c("rank"))


  count_part_coef <- model %>% filter(type=="drinkermod") %>%
    filter(parameter!="(Intercept)") %>%
    # filter(year==y) %>%
    dplyr::select(parameter, Estimate) %>% distinct() %>%
    pivot_wider(names_from=parameter, values_from=Estimate)

  names(count_part_coef) <- ifelse(names(count_part_coef)=="alc_rounded_1", "alc_daily_g_1", names(count_part_coef))

  count_part_lp <- data_prediction$drinkermod +
    as.numeric(count_part_coef['time'])*time +
    as.numeric(count_part_coef['alc_daily_g_1'])*data_prediction$microsim.init.alc.gpd +
    as.numeric(count_part_coef['lagged_catLow risk'])*data_prediction$cat1 +
    as.numeric(count_part_coef['lagged_catMedium risk'])*data_prediction$cat2 +
    as.numeric(count_part_coef['lagged_catHigh risk'])*data_prediction$cat3 +
    as.numeric(count_part_coef['lagged_age25-64'])*data_prediction$age2564 +
    as.numeric(count_part_coef['lagged_age65+'])*data_prediction$age65 +
    as.numeric(count_part_coef['female.factor_2Women']) * data_prediction$Women +
    as.numeric(count_part_coef['race.factor_2Black, non-Hispanic']) * data_prediction$raceblack +
    as.numeric(count_part_coef['race.factor_2Hispanic']) * data_prediction$racehispanic +
    as.numeric(count_part_coef['race.factor_2Other, non-Hispanic']) * data_prediction$raceother +
    as.numeric(count_part_coef['lagged_educationLow']) * data_prediction$edulow +
    as.numeric(count_part_coef['lagged_educationMed']) * data_prediction$edumed

    # as.numeric(count_part_coef['abstainer_1'])*data_prediction$abstainer +
    # as.numeric(count_part_coef['alc_daily_g_1:cat1_lag'])*data_prediction$cat1*data_prediction$microsim.init.alc.gpd +
    # as.numeric(count_part_coef['alc_daily_g_1:cat2_lag'])*data_prediction$cat2*data_prediction$microsim.init.alc.gpd
    # as.numeric(count_part_coef['age3_225-64']) * data_prediction$age2564 +
    # as.numeric(count_part_coef['age3_265+']) * data_prediction$age65 +

    # as.numeric(count_part_coef['alc_daily_g_1']) * data_prediction$microsim.init.alc.gpd +
    # as.numeric(count_part_coef['cat1_lag']) * data_prediction$cat1 +
    # as.numeric(count_part_coef['cat2_lag']) * data_prediction$cat2 +
    # as.numeric(count_part_coef['cat3_lag']) * data_prediction$cat3 +
    # as.numeric(count_part_coef['alc_daily_g_1:cat2_lag']) * data_prediction$cat2*data_prediction$microsim.init.alc.gpd +
    # as.numeric(count_part_coef['alc_daily_g_1:cat3_lag']) * data_prediction$cat3*data_prediction$microsim.init.alc.gpd
    # as.numeric(count_part_coef['female.factor_2Women:cat1_lag']) * data_prediction$Women*data_prediction$cat1 +
    # as.numeric(count_part_coef['female.factor_2Women:cat2_lag']) * data_prediction$Women*data_prediction$cat2 +
    # as.numeric(count_part_coef['female.factor_2Women:cat3_lag']) * data_prediction$Women*data_prediction$cat3
    # as.numeric(count_part_coef['alc_daily_g_1:cat2_1']) * data_prediction$cat2*data_prediction$microsim.init.alc.gpd +
    # as.numeric(count_part_coef['alc_daily_g_1:cat3_1']) * data_prediction$cat3*data_prediction$microsim.init.alc.gpd
    # as.numeric(count_part_coef['age3_225.64.abstainer_1']) * data_prediction$abstainer*data_prediction$age2564 +
    # as.numeric(count_part_coef['age3_265..abstainer_1']) * data_prediction$abstainer*data_prediction$age65 +
    # as.numeric(count_part_coef['alc_rounded_1.cat2_1']) * data_prediction$cat2*data_prediction$microsim.init.alc.gpd +
    # as.numeric(count_part_coef['alc_rounded_1.cat3_1']) * data_prediction$cat3*data_prediction$microsim.init.alc.gpd

  # unstandardise the coefficients
  # logalc <- data_prediction %>% ungroup() %>% filter(microsim.init.alc.gpd>0) %>%
  #   mutate(logalc = log(microsim.init.alc.gpd)) %>%
  #   summarise(mean= mean(logalc),
  #             sd = sd(logalc))
  # #
  # expected_counts <- (count_part_lp - logalc$mean) * logalc$sd

  expected_counts <- exp(count_part_lp)

  predicted_values <- ifelse(runif(nrow(data_prediction)) < zero_prob, 0, expected_counts)

  # abstainers <- ifelse(predicted_values==0, 1,0)
  #
  # mean(abstainers)

  data_prediction$predicted_values <- predicted_values

  data_prediction <- data_prediction %>%
    mutate(formerdrinker = ifelse(formerdrinker==1 & predicted_values==0, 1,
                                  ifelse(formerdrinker==1 & predicted_values>1, 0,
                                         ifelse(microsim.init.alc.gpd>1 & predicted_values==0, 1,
                                                formerdrinker))),
           predicted_values = ifelse(predicted_values>200, 200, predicted_values))

  data$microsim.init.alc.gpd <- NULL
  data$microsim.init.alc.gpd <- data_prediction$predicted_values
  data$formerdrinker <- NULL
  data$formerdrinker <- data_prediction$formerdrinker
  return(data)
}
