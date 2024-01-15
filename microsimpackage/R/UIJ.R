#' Calculates RR of other unintentional injuries
#'
#' @param
#' @keywords other unintentional injuries
#' @export
#' @examples
#' UIJ mortality risk
UIJ <- function(data,lhs){
  data <- data %>%
    mutate(ageCAT = cut(microsim.init.age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           cat = paste0(microsim.init.sex, ageCAT, microsim.init.education)) %>%
    dplyr::select(-ageCAT)
  B_UIJ1_MEN <- as.numeric(lhs["B_UIJ1_MEN"])
  B_UIJ2_MEN <- as.numeric(lhs["B_UIJ2_MEN"])
  B_UIJ3_MEN <- as.numeric(lhs["B_UIJ3_MEN"])
  B_UIJ1_WOMEN <- as.numeric(lhs["B_UIJ1_WOMEN"])
  B_UIJ2_WOMEN <- as.numeric(lhs["B_UIJ2_WOMEN"])
  UIJ_FORMERDRINKER_MEN <- as.numeric(lhs["UIJ_FORMERDRINKER_MEN"])
  UIJ_FORMERDRINKER_WOMEN <- as.numeric(lhs["UIJ_FORMERDRINKER_WOMEN"])
  
  data <- data %>%
  mutate(RR_UIJ = ifelse(microsim.init.alc.gpd<=20 & microsim.init.sex=="m",
                         exp(B_UIJ1_MEN),
                         ifelse(microsim.init.alc.gpd<=40 & microsim.init.sex=="m",
                                exp(B_UIJ2_MEN),
                                ifelse(microsim.init.alc.gpd>40 & microsim.init.sex=="m",
                                       exp(B_UIJ3_MEN),
                                       ifelse(microsim.init.alc.gpd<=20 & microsim.init.sex=="f",
                                              exp(B_UIJ1_WOMEN),
                                              ifelse(microsim.init.alc.gpd>20 & microsim.init.sex=="f",
                                                     exp(B_UIJ2_WOMEN), NA))))),
         RR_UIJ = ifelse(formerdrinker==1 & microsim.init.sex=="m", 
                         exp(UIJ_FORMERDRINKER_MEN), 
                         ifelse(formerdrinker==1 & microsim.init.sex=="f",
                                exp(UIJ_FORMERDRINKER_WOMEN), RR_UIJ)))
  
  #WHO formula
  # B_UIJ <- as.numeric(lhs["B_UIJ"])
  # UIJ_FORMERDRINKER <- as.numeric(lhs["UIJ_FORMERDRINKER"])
  # data <- data %>%
  #   mutate(RR_UIJ = exp(0 + B_UIJ*microsim.init.alc.gpd),
  #          RR_UIJ = ifelse(formerdrinker==1, exp(UIJ_FORMERDRINKER), RR_UIJ))
  
  return(data)
}