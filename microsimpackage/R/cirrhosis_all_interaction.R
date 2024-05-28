#' Calculates RR of liver cirrhosis through all type pathway - with SES interaction effects
#'
#' @param
#' @keywords cirrhosis all type pathway interaction effects
#' @export
#' @examples
#' LC all type pathway pathway including SES interaction effects (alcohol use as continuous displayed in the unit of 20 grams per day)
CirrhosisAllInteraction <- function(data,lhs){
  data <- data %>%
    mutate(ageCAT = cut(microsim.init.age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           cat = paste0(microsim.init.sex, ageCAT, microsim.init.education)) %>%
    dplyr::select(-ageCAT)
    B_LIVER_LEHS <- as.numeric(lhs["B_LIVER_LEHS"])
    B_LIVER_SomeC <- as.numeric(lhs["B_LIVER_SomeC"])
    B_LIVER_GPD <- as.numeric(lhs["B_LIVER_GPD"])
    B_LIVER_LEHSxGPD <- as.numeric(lhs["B_LIVER_LEHSxGPD"])
    B_LIVER_SomeCxGPD <- as.numeric(lhs["B_LIVER_SomeCxGPD"])
  
  # B_LIVER_LEHS_MEN <- as.numeric(lhs["B_LIVER_LEHS_MEN"])
  # B_LIVER_SomeC_MEN <- as.numeric(lhs["B_LIVER_SomeC_MEN"])
  # B_LIVER_LEHS_WOMEN <- as.numeric(lhs["B_LIVER_LEHS_WOMEN"])
  # B_LIVER_SomeC_WOMEN <- as.numeric(lhs["B_LIVER_SomeC_WOMEN"])
  # B_LIVER_GPD_MEN <- as.numeric(lhs["B_LIVER_GPD_MEN"])
  # B_LIVER_GPD_WOMEN <- as.numeric(lhs["B_LIVER_GPD_WOMEN"])
  # B_LIVER_LEHSxGPD_MEN <- as.numeric(lhs["B_LIVER_LEHSxGPD_MEN"])
  # B_LIVER_SomeCxGPD_MEN <- as.numeric(lhs["B_LIVER_SomeCxGPD_MEN"])
  # B_LIVER_LEHSxGPD_WOMEN <- as.numeric(lhs["B_LIVER_LEHSxGPD_WOMEN"])
  # B_LIVER_SomeCxGPD_WOMEN <- as.numeric(lhs["B_LIVER_SomeCxGPD_WOMEN"])
  
    #Former drinkers
    LIVER_FD_LEHS <- as.numeric(lhs["LIVER_FD_LEHS"])
    LIVER_FD_SomeC <- as.numeric(lhs["LIVER_FD_SomeC"])
    LIVER_FD <- as.numeric(lhs["LIVER_FD"])
    LIVER_LEHSxFD <- as.numeric(lhs["LIVER_LEHSxFD"])
    LIVER_SomeCxFD <- as.numeric(lhs["LIVER_SomeCxFD"])
  
  # HighSchool <- log(2.0269)
  # SomeC <- log(1.4872)
  # GPD <- log(1.0032)
  # HighSchoolGPD <- log(1)
  # SomeCGPD <- log(1.0023)
  # FORMERDRINKER <- as.numeric(lhs["LIVER_FORMERDRINKER"])

    data <- data %>%
    mutate(RR_LVDC = ifelse(microsim.init.education=="LEHS",
                                exp(B_LIVER_LEHS + B_LIVER_GPD*(microsim.init.alc.gpd/20) + B_LIVER_LEHSxGPD*(microsim.init.alc.gpd/20)),
                                ifelse(microsim.init.education=="SomeC",
                                      exp(B_LIVER_SomeC + B_LIVER_GPD*(microsim.init.alc.gpd/20) + B_LIVER_SomeCxGPD*(microsim.init.alc.gpd/20)),
                                      exp(B_LIVER_GPD*(microsim.init.alc.gpd/20)))),
           RR_LVDC = ifelse(formerdrinker==1 & microsim.init.education=="LEHS", 
                                  exp(LIVER_FD_LEHS + LIVER_FD + LIVER_LEHSxFD),
                                  ifelse(formerdrinker==1 & microsim.init.education=="SomeC",
                                         exp(LIVER_FD_SomeC + LIVER_FD + LIVER_SomeCxFD),
                                         ifelse(formerdrinker==1 & microsim.init.education=="College",
                                                exp(LIVER_FD), RR_LVDC))))
  return(data)
}

#  mutate(RR_LVDC = ifelse(microsim.init.education=="LEHS" & microsim.init.sex=="m",
#                              exp(B_LIVER_LEHS_MEN + B_LIVER_GPD_MEN*microsim.init.alc.gpd + B_LIVER_LEHSxGPD_MEN*microsim.init.alc.gpd),
#                              ifelse(microsim.init.education=="SomeC" & microsim.init.sex=="m",
#                                    exp(B_LIVER_SomeC_MEN + B_LIVER_GPD_MEN*microsim.init.alc.gpd + B_LIVER_SomeCxGPD_MEN*microsim.init.alc.gpd),
#                                    ifelse(microsim.init.education=="College" & microsim.init.sex=="m",
#                                           exp(B_LIVER_GPD_MEN*microsim.init.alc.gpd),
#                                           ifelse(microsim.init.education=="LEHS" & microsim.init.sex=="f",
#                                                  exp(B_LIVER_LEHS_WOMEN + B_LIVER_GPD_WOMEN*microsim.init.alc.gpd + B_LIVER_LEHSxGPD_WOMEN*microsim.init.alc.gpd),
#                                                  ifelse(microsim.init.education=="SomeC" & microsim.init.sex=="f",
#                                                        exp(B_LIVER_SomeC_WOMEN + B_LIVER_GPD_WOMEN*microsim.init.alc.gpd + B_LIVER_SomeCxGPD_WOMEN*microsim.init.alc.gpd), 
#                                                        ifelse(microsim.init.education=="College" & microsim.init.sex=="f",
#                                                               exp(B_LIVER_GPD_WOMEN*microsim.init.alc.gpd), NA)))))),
