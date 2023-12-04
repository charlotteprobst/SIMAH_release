#' Calculates RR of IHD - with SES interaction effects
#'
#' @param
#' @keywords IHD interaction effects
#' @export
#' @examples
#' IHD including SES interaction effects
IHDInteraction <- function(data,lhs){
  data <- data %>%
    mutate(ageCAT = cut(microsim.init.age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           cat = paste0(microsim.init.sex, ageCAT, microsim.init.education)) %>%
    dplyr::select(-ageCAT)
  B_IHD_LEHS_MEN <- as.numeric(lhs["B_IHD_LEHS_MEN"])
  B_IHD_SomeC_MEN <- as.numeric(lhs["B_IHD_SomeC_MEN"])
  B_IHD_LEHS_WOMEN <- as.numeric(lhs["B_IHD_LEHS_WOMEN"])
  B_IHD_SomeC_WOMEN <- as.numeric(lhs["B_IHD_SomeC_WOMEN"])
  B_IHD_CAT1_MEN <- as.numeric(lhs["B_IHD_CAT1_MEN"])
  B_IHD_CAT2_MEN <- as.numeric(lhs["B_IHD_CAT2_MEN"])
  B_IHD_CAT3_MEN <- as.numeric(lhs["B_IHD_CAT3_MEN"])
  B_IHD_CAT4_MEN <- as.numeric(lhs["B_IHD_CAT4_MEN"])
  B_IHD_CAT1_WOMEN <- as.numeric(lhs["B_IHD_CAT1_WOMEN"])
  B_IHD_CAT2_WOMEN <- as.numeric(lhs["B_IHD_CAT2_WOMEN"])
  B_IHD_LEHSxCAT1_MEN <- as.numeric(lhs["B_IHD_LEHSxCAT1_MEN"])
  B_IHD_LEHSxCAT2_MEN <- as.numeric(lhs["B_IHD_LEHSxCAT2_MEN"])
  B_IHD_LEHSxCAT3_MEN <- as.numeric(lhs["B_IHD_LEHSxCAT3_MEN"])
  B_IHD_LEHSxCAT4_MEN <- as.numeric(lhs["B_IHD_LEHSxCAT4_MEN"])
  B_IHD_LEHSxCAT1_WOMEN <- as.numeric(lhs["B_IHD_LEHSxCAT1_WOMEN"])
  B_IHD_LEHSxCAT2_WOMEN <- as.numeric(lhs["B_IHD_LEHSxCAT2_WOMEN"])
  B_IHD_SomeCxCAT1_MEN <- as.numeric(lhs["B_IHD_SomeCxCAT1_MEN"])
  B_IHD_SomeCxCAT2_MEN <- as.numeric(lhs["B_IHD_SomeCxCAT2_MEN"])
  B_IHD_SomeCxCAT3_MEN <- as.numeric(lhs["B_IHD_SomeCxCAT3_MEN"])
  B_IHD_SomeCxCAT4_MEN <- as.numeric(lhs["B_IHD_SomeCxCAT4_MEN"])
  B_IHD_SomeCxCAT1_WOMEN <- as.numeric(lhs["B_IHD_SomeCxCAT1_WOMEN"])
  B_IHD_SomeCxCAT2_WOMEN <- as.numeric(lhs["B_IHD_SomeCxCAT2_WOMEN"])

  #Former drinkers
  IHD_FD_MEN <- as.numeric(lhs["IHD_FD_MEN"])
  IHD_FD_WOMEN <- as.numeric(lhs["IHD_FD_WOMEN"])
  IHD_LEHSxFD_MEN <- as.numeric(lhs["IHD_LEHSxFD_MEN"])
  IHD_SomeCxFD_MEN <- as.numeric(lhs["IHD_SomeCxFD_MEN"])
  IHD_LEHSxFD_WOMEN <- as.numeric(lhs["IHD_LEHSxFD_WOMEN"])
  IHD_SomeCxFD_WOMEN <- as.numeric(lhs["IHD_SomeCxFD_WOMEN"])
  
  data <- data %>%
    mutate(RR_IHD_INT = ifelse(microsim.init.education=="LEHS" & microsim.init.sex=="m" & microsim.init.alc.gpd<=20,
                                exp(B_IHD_LEHS_MEN + B_IHD_CAT1_MEN*microsim.init.alc.gpd + B_IHD_LEHSxCAT1_MEN*microsim.init.alc.gpd),
                                ifelse(microsim.init.education=="LEHS" & microsim.init.sex=="m" & microsim.init.alc.gpd<=40,
                                      exp(B_IHD_LEHS_MEN + B_IHD_CAT2_MEN*microsim.init.alc.gpd + B_IHD_LEHSxCAT2_MEN*microsim.init.alc.gpd),
                                      ifelse(microsim.init.education=="LEHS" & microsim.init.sex=="m" & microsim.init.alc.gpd<=60,
                                             exp(B_IHD_LEHS_MEN + B_IHD_CAT3_MEN*microsim.init.alc.gpd + B_IHD_LEHSxCAT3_MEN*microsim.init.alc.gpd),
                                             ifelse(microsim.init.education=="LEHS" & microsim.init.sex=="m",
                                                    exp(B_IHD_LEHS_MEN + B_IHD_CAT4_MEN*microsim.init.alc.gpd + B_IHD_LEHSxCAT4_MEN*microsim.init.alc.gpd),
                                                    ifelse(microsim.init.education=="SomeC" & microsim.init.sex=="m" & microsim.init.alc.gpd<=20,
                                                          exp(B_IHD_SomeC_MEN + B_IHD_CAT1_MEN*microsim.init.alc.gpd + B_IHD_SomeCxCAT1_MEN*microsim.init.alc.gpd),
                                                          ifelse(microsim.init.education=="SomeC" & microsim.init.sex=="m" & microsim.init.alc.gpd<=40,
                                                                 exp(B_IHD_SomeC_MEN + B_IHD_CAT2_MEN*microsim.init.alc.gpd + B_IHD_SomeCxCAT2_MEN*microsim.init.alc.gpd),
                                                                 ifelse(microsim.init.education=="SomeC" & microsim.init.sex=="m" & microsim.init.alc.gpd<=60,
                                                                        exp(B_IHD_SomeC_MEN + B_IHD_CAT3_MEN*microsim.init.alc.gpd + B_IHD_SomeCxCAT3_MEN*microsim.init.alc.gpd),
                                                                        ifelse(microsim.init.education=="SomeC" & microsim.init.sex=="m",
                                                                               exp(B_IHD_SomeC_MEN + B_IHD_CAT4_MEN*microsim.init.alc.gpd + B_IHD_SomeCxCAT4_MEN*microsim.init.alc.gpd),
                                                                               ifelse(microsim.init.education=="College" & microsim.init.sex=="m" & microsim.init.alc.gpd<=20,
                                                                                      exp(B_IHD_CAT1_MEN*microsim.init.alc.gpd),
                                                                                      ifelse(microsim.init.education=="College" & microsim.init.sex=="m" & microsim.init.alc.gpd<=40,
                                                                                             exp(B_IHD_CAT2_MEN*microsim.init.alc.gpd),
                                                                                             ifelse(microsim.init.education=="College" & microsim.init.sex=="m" & microsim.init.alc.gpd<=60,
                                                                                                    exp(B_IHD_CAT3_MEN*microsim.init.alc.gpd),
                                                                                                    ifelse(microsim.init.education=="College" & microsim.init.sex=="m",
                                                                                                           exp(B_IHD_CAT4_MEN*microsim.init.alc.gpd),
                                                                                                           ifelse(microsim.init.education=="LEHS" & microsim.init.sex=="f" & microsim.init.alc.gpd<=20,
                                                                                                                  exp(B_IHD_LEHS_WOMEN + B_IHD_CAT1_WOMEN*microsim.init.alc.gpd + B_IHD_LEHSxCAT1_WOMEN*microsim.init.alc.gpd),
                                                                                                                  ifelse(microsim.init.education=="LEHS" & microsim.init.sex=="f" & microsim.init.alc.gpd<=40,
                                                                                                                         exp(B_IHD_LEHS_WOMEN + B_IHD_CAT2_WOMEN*microsim.init.alc.gpd + B_IHD_LEHSxCAT2_WOMEN*microsim.init.alc.gpd),
                                                                                                                         ifelse(microsim.init.education=="LEHS" & microsim.init.sex=="f" & microsim.init.alc.gpd<=60,
                                                                                                                                exp(B_IHD_LEHS_WOMEN + B_IHD_CAT3_WOMEN*microsim.init.alc.gpd + B_IHD_LEHSxCAT3_WOMEN*microsim.init.alc.gpd),
                                                                                                                                ifelse(microsim.init.education=="LEHS" & microsim.init.sex=="f",
                                                                                                                                       exp(B_IHD_LEHS_WOMEN + B_IHD_CAT4_WOMEN*microsim.init.alc.gpd + B_IHD_LEHSxCAT4_WOMEN*microsim.init.alc.gpd),
                                                                                                                                       ifelse(microsim.init.education=="SomeC" & microsim.init.sex=="f" & microsim.init.alc.gpd<=20,
                                                                                                                                              exp(B_IHD_SomeC_WOMEN + B_IHD_CAT1_WOMEN*microsim.init.alc.gpd + B_IHD_SomeCxCAT1_WOMEN*microsim.init.alc.gpd),
                                                                                                                                              ifelse(microsim.init.education=="SomeC" & microsim.init.sex=="f" & microsim.init.alc.gpd<=40,
                                                                                                                                                     exp(B_IHD_SomeC_WOMEN + B_IHD_CAT2_WOMEN*microsim.init.alc.gpd + B_IHD_SomeCxCAT2_WOMEN*microsim.init.alc.gpd),
                                                                                                                                                     ifelse(microsim.init.education=="SomeC" & microsim.init.sex=="f" & microsim.init.alc.gpd<=60,
                                                                                                                                                            exp(B_IHD_SomeC_WOMEN + B_IHD_CAT3_WOMEN*microsim.init.alc.gpd + B_IHD_SomeCxCAT3_WOMEN*microsim.init.alc.gpd),
                                                                                                                                                            ifelse(microsim.init.education=="SomeC" & microsim.init.sex=="f",
                                                                                                                                                                   exp(B_IHD_SomeC_WOMEN + B_IHD_CAT4_WOMEN*microsim.init.alc.gpd + B_IHD_SomeCxCAT4_WOMEN*microsim.init.alc.gpd),
                                                                                                                                                                   ifelse(microsim.init.education=="College" & microsim.init.sex=="f" & microsim.init.alc.gpd<=20,
                                                                                                                                                                          exp(B_IHD_CAT1_WOMEN*microsim.init.alc.gpd),
                                                                                                                                                                          ifelse(microsim.init.education=="College" & microsim.init.sex=="f" & microsim.init.alc.gpd<=40,
                                                                                                                                                                                 exp(B_IHD_CAT2_WOMEN*microsim.init.alc.gpd),
                                                                                                                                                                                 ifelse(microsim.init.education=="College" & microsim.init.sex=="f" & microsim.init.alc.gpd<=60,
                                                                                                                                                                                        exp(B_IHD_CAT3_WOMEN*microsim.init.alc.gpd),
                                                                                                                                                                                        ifelse(microsim.init.education=="College" & microsim.init.sex=="f",
                                                                                                                                                                                               exp(B_IHD_CAT4_WOMEN*microsim.init.alc.gpd), NA)))))))))))))))))))))))),
                                                          
           RR_IHD_INT = ifelse(formerdrinker==1 & microsim.init.education=="LEHS" & microsim.init.sex=="m", 
                                 exp(B_IHD_LEHS_MEN + IHD_FD_MEN*microsim.init.alc.gpd + IHD_LEHSxFD_MEN*microsim.init.alc.gpd),
                                 ifelse(formerdrinker==1 & microsim.init.education=="SomeC" & microsim.init.sex=="m",
                                        exp(B_IHD_SomeC_MEN + IHD_FD_MEN*microsim.init.alc.gpd + IHD_SomeCxFD_MEN*microsim.init.alc.gpd),
                                        ifelse(formerdrinker==1 & microsim.init.education=="College" & microsim.init.sex=="m",
                                               exp(IHD_FD_MEN*microsim.init.alc.gpd),
                                               ifelse(formerdrinker==1 & microsim.init.education=="LEHS" & microsim.init.sex=="f",
                                                     exp(B_IHD_LEHS_WOMEN + IHD_FD_WOMEN*microsim.init.alc.gpd + IHD_LEHSxFD_WOMEN*microsim.init.alc.gpd), 
                                                     ifelse(formerdrinker==1 & microsim.init.education=="SomeC" & microsim.init.sex=="f",
                                                           exp(B_IHD_SomeC_WOMEN + IHD_FD_WOMEN*microsim.init.alc.gpd + IHD_SomeCxFD_WOMEN*microsim.init.alc.gpd),
                                                           ifelse(formerdrinker==1 & microsim.init.education=="College" & microsim.init.sex=="f",
                                                                  exp(IHD_FD_WOMEN*microsim.init.alc.gpd), RR_IHD_INT)))))))
  return(data)
}
