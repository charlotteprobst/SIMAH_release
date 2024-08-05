#' Allocate individuals a grams per day of consumption - based on categories
#'
#' @param
#' @keywords allocate alcohol
#' @export
#' @examples
#' allocate_gpd
allocate_gramsperday <- function(data, model){

  totransition <- data %>% filter(totransitioncont==1)

  data_prediction <- totransition %>%
    mutate(agecat = cut(microsim.init.age,
                        breaks=c(0,24,64,100),
                        labels=c("18-24","25-64","65+"))) %>%
    dplyr::select(agecat, microsim.init.sex,
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
           cat1 = ifelse(AlcCAT=="Low risk", 1,0),
           cat2 = ifelse(AlcCAT=="Medium risk", 1,0),
           cat3 = ifelse(AlcCAT=="High risk", 1,0))

  model[is.na(model)] <- 0

  predictors <- list()
  for(i in unique(model$cat)){
    coefs <- model %>% filter(cat==i)
    predictors[[paste(i)]] <- as.numeric(coefs['(Intercept)']) +
      as.numeric(coefs['alc_daily_g_1'])*data_prediction$microsim.init.alc.gpd +
      as.numeric(coefs['cat1_lag'])*data_prediction$cat1 +
      as.numeric(coefs['cat2_lag'])*data_prediction$cat2 +
      as.numeric(coefs['cat3_lag'])*data_prediction$cat3 +
      as.numeric(coefs['female.factor_2Women'])*data_prediction$Women +
      as.numeric(coefs['lagged_age25-64'])*data_prediction$age2564 +
      as.numeric(coefs['lagged_age65+'])*data_prediction$age65 +
      as.numeric(coefs['lagged_educationLow']) * data_prediction$edulow +
      as.numeric(coefs['lagged_educationMed']) * data_prediction$edumed +
      as.numeric(coefs['race.factor_2Black, non-Hispanic']) * data_prediction$raceblack +
      as.numeric(coefs['race.factor_2Hispanic']) * data_prediction$racehispanic +
      as.numeric(coefs['race.factor_2Other, non-Hispanic']) * data_prediction$raceother +
      as.numeric(coefs['alc_daily_g_1:female.factor_2Women'])*data_prediction$microsim.init.alc.gpd*data_prediction$Women

    predictors[[paste(i)]] <- exp(predictors[[paste(i)]])
  }

  data_prediction$predictedlr <- predictors[["Low risk"]]
  data_prediction$predictedmr <- predictors[["Medium risk"]]
  data_prediction$predictedhr <- predictors[["High risk"]]

  data_prediction$predicted <- ifelse(data_prediction$AlcCAT=="Low risk",
                                      data_prediction$predictedlr,
                                      ifelse(data_prediction$AlcCAT=="Medium risk",
                                             data_prediction$predictedmr,
                                             ifelse(data_prediction$AlcCAT=="High risk",
                                                    data_prediction$predictedhr,NA)))

# now cap the predicted values within the category bounds
  data_prediction$predicted_capped <- case_when(
    data_prediction$AlcCAT=="Low risk" & data_prediction$Women==1 & data_prediction$predicted>20 ~ 20,
    data_prediction$AlcCAT=="Low risk" & data_prediction$Women==0 & data_prediction$predicted>40 ~ 40,
    data_prediction$AlcCAT=="Medium risk" & data_prediction$Women==1 & data_prediction$predicted<20 ~ 20,
    data_prediction$AlcCAT=="Medium risk" & data_prediction$Women==1 & data_prediction$predicted>40 ~ 40,
    data_prediction$AlcCAT=="Medium risk" & data_prediction$Women==0 & data_prediction$predicted<40 ~ 40,
    data_prediction$AlcCAT=="Medium risk" & data_prediction$Women==0 & data_prediction$predicted>60 ~ 60,
    data_prediction$AlcCAT=="High risk" & data_prediction$Women==1 & data_prediction$predicted<40 ~ 40,
    data_prediction$AlcCAT=="High risk" & data_prediction$Women==0 & data_prediction$predicted<60 ~ 60,
    data_prediction$AlcCAT=="High risk" & data_prediction$predicted>200 ~ 200,
    .default = data_prediction$predicted)

  totransition$microsim.init.alc.gpd <- data_prediction$predicted_capped

  data_new <- rbind(totransition, subset(data, totransitioncont==0))

  data_new$microsim.init.alc.gpd <- ifelse(data_new$AlcCAT=="Non-drinker", 0,
                                           data_new$microsim.init.alc.gpd)
  data_new$totransitioncont <- NULL

  return(data_new)
}

