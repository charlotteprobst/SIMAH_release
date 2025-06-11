#' Calculates RR of hypertensive heart disease
#'
#' @param
#' @keywords HYPERTENSION
#' @export
#' @examples
#' Hypertensive heart disease mortality risk
HYPHD <- function(data,lhs){
  data <- data %>%
    mutate(ageCAT = cut(age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           cat = paste0(sex, ageCAT, education)) %>%
    dplyr::select(-ageCAT)
  B_HYPHD_MEN <- as.numeric(lhs["B_HYPHD_MEN"])
  B_HYPHD_WOMEN <- as.numeric(lhs["B_HYPHD_WOMEN"])
  HYPHD_FORMERDRINKER <- as.numeric(lhs["HYPHD_FORMERDRINKER"])
  data <- data %>%
    mutate(RR_HYPHD = ifelse(sex=="m" & alc_gpd < 150,
                          exp(0 + B_HYPHD_MEN*alc_gpd),
                          ifelse(sex=="m" & alc_gpd >= 150,
                                 exp(0 + B_HYPHD_MEN*150),
                          ifelse(sex=="f" & alc_gpd < 150,
                                 exp(0 + B_HYPHD_WOMEN*alc_gpd),
                                 ifelse(sex=="f" & alc_gpd >= 150,
                                        exp(0 + B_HYPHD_WOMEN*150), NA)))),
           RR_HYPHD = ifelse(formerdrinker==1, exp(HYPHD_FORMERDRINKER), RR_HYPHD))
  return(data)
}
