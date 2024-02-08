#' Calculates RR of motor vehicle injuries
#'
#' @param
#' @keywords motor vehicle injuries
#' @export
#' @examples
#' MVACC mortality risk
MVACC <- function(data,lhs){
  data <- data %>%
    mutate(ageCAT = cut(microsim.init.age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           cat = paste0(microsim.init.sex, ageCAT, microsim.init.education)) %>%
    dplyr::select(-ageCAT)
    B_MVACC1 <- as.numeric(lhs["B_MVACC1"])
    B_MVACC2 <- as.numeric(lhs["B_MVACC2"])
    MVACC_FORMERDRINKER <- as.numeric(lhs["MVACC_FORMERDRINKER"])
    
    data <- data %>%
      mutate(RR_MVACC = ifelse(microsim.init.alc.gpd<60,
                           exp(0 + B_MVACC1*microsim.init.alc.gpd),
                              ifelse(microsim.init.alc.gpd>=60,
                                       exp(B_MVACC1*microsim.init.alc.gpd + B_MVACC2), NA)),
             RR_MVACC = ifelse(formerdrinker==1, exp(MVACC_FORMERDRINKER), RR_MVACC))
  
  ### Code for NHIS results
  # B_MVACC1_MEN <- as.numeric(lhs["B_MVACC1_MEN"])
  # B_MVACC2_MEN <- as.numeric(lhs["B_MVACC2_MEN"])
  # B_MVACC3_MEN <- as.numeric(lhs["B_MVACC3_MEN"])
  # B_MVACC1_WOMEN <- as.numeric(lhs["B_MVACC1_WOMEN"])
  # B_MVACC2_WOMEN <- as.numeric(lhs["B_MVACC2_WOMEN"])
  # MVACC_FORMERDRINKER_MEN <- as.numeric(lhs["MVACC_FORMERDRINKER_MEN"])
  # MVACC_FORMERDRINKER_WOMEN <- as.numeric(lhs["MVACC_FORMERDRINKER_WOMEN"])
  # 
  # data <- data %>%
  #   mutate(RR_MVACC = ifelse(microsim.init.alc.gpd<=20 & microsim.init.sex=="m",
  #                            exp(B_MVACC1_MEN),
  #                            ifelse(microsim.init.alc.gpd<=40 & microsim.init.sex=="m",
  #                                   exp(B_MVACC2_MEN),
  #                                   ifelse(microsim.init.alc.gpd >40 & microsim.init.sex=="m",
  #                                          exp(B_MVACC3_MEN),
  #                                          ifelse(microsim.init.alc.gpd<=20 & microsim.init.sex=="f",
  #                                                 exp(B_MVACC1_WOMEN),
  #                                                 ifelse(microsim.init.alc.gpd>20 & microsim.init.sex=="f",
  #                                                        exp(B_MVACC2_WOMEN), NA))))),
  #          RR_MVACC = ifelse(formerdrinker==1 & microsim.init.sex=="m", 
  #                            exp(MVACC_FORMERDRINKER_MEN), 
  #                            ifelse(formerdrinker==1 & microsim.init.sex=="f",
  #                                   exp(MVACC_FORMERDRINKER_WOMEN), RR_MVACC)))
  
  return(data)
}
