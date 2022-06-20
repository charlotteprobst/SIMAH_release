#' Calculates RR of liver cirrhosis through heavy alcohol use
#'
#' @param
#' @keywords cirrhosis alcohol use
#' @export
#' @examples
#' cirrhosis_heavy_use
cirrhosis_heavy_use <- function(data,lhsSample){
    BETA_MALE_MORTALITY <- as.numeric(lhsSample["BETA_MALE_MORTALITY"])
    BETA_FORMER_DRINKERS_MEN <- as.numeric(lhsSample["BETA_FORMER_DRINKERS_MEN"])
    BETA_FEMALE_MORTALITY <- as.numeric(lhsSample["BETA_FEMALE_MORTALITY"])
    BETA_FORMER_DRINKERS_WOMEN <- as.numeric(lhsSample["BETA_FORMER_DRINKERS_WOMEN"])

  data$gpd <- ifelse(data$microsim.init.alc.gpd>200, 200, data$microsim.init.alc.gpd)
  data$RRHeavyUse <- ifelse(data$Cirrhosis_risk==1 &
                              data$microsim.init.sex=="m" &
                              data$microsim.init.drinkingstatus==1,
                            exp(BETA_MALE_MORTALITY*data$gpd),
                            ifelse(data$Cirrhosis_risk==1 &
                                     data$microsim.init.sex=="f" &
                                     data$microsim.init.drinkingstatus==1,
                                   exp(BETA_FEMALE_MORTALITY*data$gpd),
                                   ifelse(data$formerdrinker==1 & data$microsim.init.sex=="m" &
                                            data$Cirrhosis_risk==1,
                                          BETA_FORMER_DRINKERS_MEN,
                                          ifelse(data$formerdrinker==1 & data$microsim.init.sex=="f" &
                                                   data$Cirrhosis_risk==1,
                                                 BETA_FORMER_DRINKERS_WOMEN,1))))

  # modify the risk of former drinkers depending on how many years since they drank
  decay_length= as.numeric(lhsSample["DECAY_LENGTH"])
  decay_speed=as.numeric(lhsSample["DECAY_SPEED"])
  t=0:100
  risk_multiplier=1-exp(-(1/decay_speed)*(decay_length-t))

  risk <- data.frame(yearsincedrink=as.numeric(t), risk_multiplier=risk_multiplier)
  risk$risk_multiplier <- ifelse(risk$yearsincedrink==0, 1,
                                 ifelse(risk$yearsincedrink>=decay_length, 0,
                                        risk$risk_multiplier))
  data <- left_join(data, risk, by=c("yearsincedrink"))
  data$RRHeavyUse <- ifelse(data$formerdrinker==1, data$RRHeavyUse*data$risk_multiplier,
                            data$RRHeavyUse)
  data$RRHeavyUse <- ifelse(data$RRHeavyUse<1, 1, data$RRHeavyUse)
  data <- data %>% dplyr::select(-c(risk_multiplier,gpd))
  return(data)
}
