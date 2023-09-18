#' Calculates RR of AUD
#'
#' @param
#' @keywords AUD
#' @export
#' @examples
#' AUD mortality risk
AUD <- function(data,lhs){
  data <- data %>%
    mutate(ageCAT = cut(microsim.init.age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           cat = paste0(microsim.init.sex, ageCAT, microsim.init.education)) %>%
    dplyr::select(-ageCAT)
  B_AUD1_MEN <- as.numeric(lhs["B_AUD1_MEN"])
  B_AUD1_ALL <- as.numeric(lhs["B_AUD1_ALL"])
  data <- data %>%
    mutate(RR_AUD = ifelse(microsim.init.alc.gpd<100 & microsim.init.sex=="m",
                       exp(0 + B_AUD1_MEN*microsim.init.alc.gpd),
                       ifelse(microsim.init.alc.gpd>=100 & microsim.init.sex=="m",
                              exp(0 + B_AUD1_MEN*100),
                              ifelse(microsim.init.alc.gpd<100 & microsim.init.sex=="f",
                                     exp(0 + B_AUD1_ALL*microsim.init.alc.gpd),
                                     ifelse(microsim.init.alc.gpd>=100 & microsim.init.sex=="f",
                                            exp(0 + B_AUD1_ALL*100), NA)))))
  return(data)
}
