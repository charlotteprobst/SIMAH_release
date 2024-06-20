#' Calculates RR of liver cirrhosis through all type pathway
#'
#' @param
#' @keywords cirrhosis all type pathway
#' @export
#' @examples
#' all type pathway pathway
CirrhosisAll <- function(data,lhs){
  data <- data %>%
    mutate(ageCAT = cut(microsim.init.age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           cat = paste0(microsim.init.sex, ageCAT, microsim.init.education)) %>%
    dplyr::select(-ageCAT)
  B_LIVER1_MEN <- as.numeric(lhs["B_LIVER1_MEN"])
  B_LIVER2_MEN <- as.numeric(lhs["B_LIVER2_MEN"])
  B_LIVER1_WOMEN <- as.numeric(lhs["B_LIVER1_WOMEN"])
  B_LIVER2_WOMEN <- as.numeric(lhs["B_LIVER2_WOMEN"])
  LIVER_FORMERDRINKER <- as.numeric(lhs["LIVER_FORMERDRINKER"])
  data <- data %>%
    mutate(RR_LVDC = ifelse(microsim.init.alc.gpd<183.75 & microsim.init.sex=="m",
                       exp(0 + B_LIVER1_MEN*microsim.init.alc.gpd + B_LIVER2_MEN*(microsim.init.alc.gpd^2)),
                       ifelse(microsim.init.alc.gpd>=183.75 & microsim.init.sex=="m",
                              exp(0 + B_LIVER1_MEN*183.75 + B_LIVER2_MEN*(183.75^2)),
                              ifelse(microsim.init.alc.gpd<97.2 & microsim.init.sex=="f",
                                     exp(0 + B_LIVER1_WOMEN*microsim.init.alc.gpd + B_LIVER2_WOMEN*(microsim.init.alc.gpd^2)),
                                     ifelse(microsim.init.alc.gpd>=97.2 & microsim.init.sex=="f",
                                            exp(0 + B_LIVER1_WOMEN*97.2 + B_LIVER2_WOMEN*(97.2^2)), NA)))),
           RR_LVDC = ifelse(formerdrinker==1, exp(LIVER_FORMERDRINKER), RR_LVDC))
  return(data)
}
