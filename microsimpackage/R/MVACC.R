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
  B_MVACC <- as.numeric(lhs["B_MVACC"])
  MVACC_FORMERDRINKER <- as.numeric(lhs["MVACC_FORMERDRINKER"])
  data <- data %>%
    mutate(RR_MVACC = exp(0 + B_MVACC*microsim.init.alc.gpd),
           RR_MVACC = ifelse(formerdrinker==1, exp(MVACC_FORMERDRINKER), RR_MVACC))
  
  return(data)
}
