#' Calculates RR of AUD
#'
#' @param
#' @keywords AUD
#' @export
#' @examples
#' AUD mortality risk
AUD <- function(data,lhs){
  data <- data %>%
    mutate(ageCAT = cut(age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           cat = paste0(sex, ageCAT, education)) %>%
    dplyr::select(-ageCAT)
  B_AUD1_MEN <- as.numeric(lhs["B_AUD1_MEN"])
  B_AUD1_ALL <- as.numeric(lhs["B_AUD1_ALL"])
  AUD_FORMERDRINKER_MEN <- as.numeric(lhs["AUD_FORMERDRINKER_MEN"])
  AUD_FORMERDRINKER_WOMEN <- as.numeric(lhs["AUD_FORMERDRINKER_WOMEN"])
  data <- data %>%
    mutate(RR_AUD = ifelse(alc_gpd<122.51 & sex=="m",
                       exp(0 + B_AUD1_MEN*alc_gpd),
                       ifelse(alc_gpd>=122.51 & sex=="m",
                              exp(0 + B_AUD1_MEN*122.51),
                              ifelse(alc_gpd<114.12 & sex=="f",
                                     exp(0 + B_AUD1_ALL*alc_gpd),
                                     ifelse(alc_gpd>=114.12 & sex=="f",
                                            exp(0 + B_AUD1_ALL*114.12), NA)))),
          RR_AUD = ifelse(formerdrinker==1 & sex=="m",
                 exp(AUD_FORMERDRINKER_MEN),
                 ifelse(formerdrinker==1 & sex=="f",
                        exp(AUD_FORMERDRINKER_WOMEN), RR_AUD)))
  return(data)
}
