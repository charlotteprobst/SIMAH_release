#' Calculates RR of diabetes
#'
#' @param
#' @keywords DIABETES
#' @export
#' @examples
#' DIABETES mortality risk
DM <- function(data,lhs){
  data <- data %>%
    mutate(ageCAT = cut(microsim.init.age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           cat = paste0(microsim.init.sex, ageCAT, microsim.init.education)) %>%
    dplyr::select(-ageCAT)
  B_DM_MEN <- as.numeric(lhs["B_DM_MEN"])
  B_DM1_WOMEN <- as.numeric(lhs["B_DM1_WOMEN"])
  B_DM2_WOMEN <- as.numeric(lhs["B_DM2_WOMEN"])
  DM_FORMERDRINKER_MEN <- as.numeric(lhs["DM_FORMERDRINKER_MEN"])
  DM_FORMERDRINKER_WOMEN <- as.numeric(lhs["DM_FORMERDRINKER_WOMEN"])
  data <- data %>%
     mutate(RR_DM = ifelse(microsim.init.sex=="m",
           exp(0 + B_DM_MEN*microsim.init.alc.gpd),
                  ifelse(microsim.init.sex=="f",
                        exp(0 + B_DM1_WOMEN*microsim.init.alc.gpd
                                 + B_DM2_WOMEN*(pmax((microsim.init.alc.gpd - 2.79)/11.88053, 0)^3 + ((14 - 2.79) * pmax((microsim.init.alc.gpd - 43.74)/11.88053, 0)^3 - (43.74 - 2.79) * (pmax((microsim.init.alc.gpd - 14)/11.88053, 0)^3))  / (43.74 - 14)  )),
                                               NA)),
            RR_DM = ifelse(formerdrinker==1 & microsim.init.sex=="m",
                                 exp(DM_FORMERDRINKER_MEN),
                                 ifelse(formerdrinker==1 & microsim.init.sex=="f",
                                        exp(DM_FORMERDRINKER_WOMEN), RR_DM)))
     return(data)
  }