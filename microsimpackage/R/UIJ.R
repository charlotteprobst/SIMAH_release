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
  B_UIJ <- as.numeric(lhs["B_UIJ"])
  UIJ_FORMERDRINKER <- as.numeric(lhs["UIJ_FORMERDRINKER"])
  data <- data %>%
    mutate(RR_UIJ = exp(0 + B_UIJ*microsim.init.alc.gpd),
           RR_UIJ = ifelse(formerdrinker==1, exp(UIJ_FORMERDRINKER), RR_UIJ))
  
  return(data)
}