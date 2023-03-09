#' Calculates RR of liver cirrhosis through all type pathway - with different SES effects
#'
#' @param
#' @keywords cirrhosis all type pathway interaction effects
#' @export
#' @examples
#' all type pathway pathway including SES interaction
CirrhosisAllInteraction <- function(data,lhs){
  data <- data %>%
    mutate(ageCAT = cut(microsim.init.age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           cat = paste0(microsim.init.sex, ageCAT, microsim.init.education)) %>%
    dplyr::select(-ageCAT)
  # B_LIVER1_MEN <- as.numeric(lhs["B_LIVER1_MEN"])
  # B_LIVER2_MEN <- as.numeric(lhs["B_LIVER2_MEN"])
  # B_LIVER1_WOMEN <- as.numeric(lhs["B_LIVER1_WOMEN"])
  # B_LIVER2_WOMEN <- as.numeric(lhs["B_LIVER2_WOMEN"])

  HighSchool <- log(2.0269)
  SomeC <- log(1.4872)
  GPD <- log(1.0032)
  HighSchoolGPD <- log(1)
  SomeCGPD <- log(1.0023)
  FORMERDRINKER <- as.numeric(lhs["FORMERDRINKER"])

  data <- data %>%
    mutate(RR = ifelse(microsim.init.education=="LEHS",
                       exp(HighSchool + GPD*microsim.init.alc.gpd + HighSchoolGPD*microsim.init.alc.gpd),
                       ifelse(microsim.init.education=="SomeC",
                              exp(SomeC + GPD*microsim.init.alc.gpd + SomeCGPD*microsim.init.alc.gpd),
                              exp(GPD*microsim.init.alc.gpd))),
           RR = ifelse(formerdrinker==1, FORMERDRINKER, RR))

  return(data)
}
