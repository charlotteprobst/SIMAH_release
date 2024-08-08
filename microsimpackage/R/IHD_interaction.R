#' Calculates RR of IHD - with SES interaction effects
#'
#' @param
#' @keywords IHD interaction effects
#' @export
#' @examples
#' IHD including SES interaction effects
IHDInteraction <- function(data,lhs){
  data <- data %>%
    mutate(ageCAT = cut(age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           cat = paste0(sex, ageCAT, education)) %>%
    dplyr::select(-ageCAT)
  B_IHD_LEHS <- as.numeric(lhs["B_IHD_LEHS"])
  B_IHD_SomeC <- as.numeric(lhs["B_IHD_SomeC"])
  B_IHD_CAT1 <- as.numeric(lhs["B_IHD_CAT1"])
  B_IHD_CAT2 <- as.numeric(lhs["B_IHD_CAT2"])
  B_IHD_CAT3 <- as.numeric(lhs["B_IHD_CAT3"])
  B_IHD_CAT4 <- as.numeric(lhs["B_IHD_CAT4"])
  B_IHD_LEHSxCAT1 <- as.numeric(lhs["B_IHD_LEHSxCAT1"])
  B_IHD_LEHSxCAT2 <- as.numeric(lhs["B_IHD_LEHSxCAT2"])
  B_IHD_LEHSxCAT3 <- as.numeric(lhs["B_IHD_LEHSxCAT3"])
  B_IHD_LEHSxCAT4 <- as.numeric(lhs["B_IHD_LEHSxCAT4"])
  B_IHD_SomeCxCAT1 <- as.numeric(lhs["B_IHD_SomeCxCAT1"])
  B_IHD_SomeCxCAT2 <- as.numeric(lhs["B_IHD_SomeCxCAT2"])
  B_IHD_SomeCxCAT3 <- as.numeric(lhs["B_IHD_SomeCxCAT3"])
  B_IHD_SomeCxCAT4 <- as.numeric(lhs["B_IHD_SomeCxCAT4"])

  #Former drinkers
  IHD_FD <- as.numeric(lhs["IHD_FD"])
  IHD_LEHSxFD <- as.numeric(lhs["IHD_LEHSxFD"])
  IHD_SomeCxFD <- as.numeric(lhs["IHD_SomeCxFD"])

  data <- data %>%
    mutate(RR_IHD = ifelse(education=="LEHS" & alc_gpd<=20,
                                exp(B_IHD_LEHS + B_IHD_CAT1 + B_IHD_LEHSxCAT1),
                                ifelse(education=="LEHS" & alc_gpd<=40,
                                      exp(B_IHD_LEHS + B_IHD_CAT2 + B_IHD_LEHSxCAT2),
                                      ifelse(education=="LEHS" & alc_gpd<=60,
                                             exp(B_IHD_LEHS + B_IHD_CAT3 + B_IHD_LEHSxCAT3),
                                             ifelse(education=="LEHS" & alc_gpd>60,
                                                    exp(B_IHD_LEHS + B_IHD_CAT4 + B_IHD_LEHSxCAT4),
                                                    ifelse(education=="SomeC" & alc_gpd<=20,
                                                          exp(B_IHD_SomeC + B_IHD_CAT1 + B_IHD_SomeCxCAT1),
                                                          ifelse(education=="SomeC" & alc_gpd<=40,
                                                                 exp(B_IHD_SomeC + B_IHD_CAT2 + B_IHD_SomeCxCAT2),
                                                                 ifelse(education=="SomeC" & alc_gpd<=60,
                                                                        exp(B_IHD_SomeC + B_IHD_CAT3 + B_IHD_SomeCxCAT3),
                                                                        ifelse(education=="SomeC" & alc_gpd>60,
                                                                               exp(B_IHD_SomeC + B_IHD_CAT4 + B_IHD_SomeCxCAT4),
                                                                               ifelse(education=="College" & alc_gpd<=20,
                                                                                      exp(B_IHD_CAT1),
                                                                                      ifelse(education=="College" & alc_gpd<=40,
                                                                                             exp(B_IHD_CAT2),
                                                                                             ifelse(education=="College" & alc_gpd<=60,
                                                                                                    exp(B_IHD_CAT3),
                                                                                                    ifelse(education=="College" & alc_gpd>60,
                                                                                                           exp(B_IHD_CAT4), NA)))))))))))),

           RR_IHD = ifelse(formerdrinker==1 & education=="LEHS",
                                 exp(B_IHD_LEHS + IHD_FD + IHD_LEHSxFD),
                                 ifelse(formerdrinker==1 & education=="SomeC",
                                        exp(B_IHD_SomeC + IHD_FD + IHD_SomeCxFD),
                                        ifelse(formerdrinker==1 & education=="College",
                                               exp(IHD_FD), RR_IHD))))
  return(data)
}

# B_IHD_LEHS_MEN <- as.numeric(lhs["B_IHD_LEHS_MEN"])
# B_IHD_SomeC_MEN <- as.numeric(lhs["B_IHD_SomeC_MEN"])
# B_IHD_LEHS_WOMEN <- as.numeric(lhs["B_IHD_LEHS_WOMEN"])
# B_IHD_SomeC_WOMEN <- as.numeric(lhs["B_IHD_SomeC_WOMEN"])
# B_IHD_CAT1_MEN <- as.numeric(lhs["B_IHD_CAT1_MEN"])
# B_IHD_CAT2_MEN <- as.numeric(lhs["B_IHD_CAT2_MEN"])
# B_IHD_CAT3_MEN <- as.numeric(lhs["B_IHD_CAT3_MEN"])
# B_IHD_CAT4_MEN <- as.numeric(lhs["B_IHD_CAT4_MEN"])
# B_IHD_CAT1_WOMEN <- as.numeric(lhs["B_IHD_CAT1_WOMEN"])
# B_IHD_CAT2_WOMEN <- as.numeric(lhs["B_IHD_CAT2_WOMEN"])
# B_IHD_LEHSxCAT1_MEN <- as.numeric(lhs["B_IHD_LEHSxCAT1_MEN"])
# B_IHD_LEHSxCAT2_MEN <- as.numeric(lhs["B_IHD_LEHSxCAT2_MEN"])
# B_IHD_LEHSxCAT3_MEN <- as.numeric(lhs["B_IHD_LEHSxCAT3_MEN"])
# B_IHD_LEHSxCAT4_MEN <- as.numeric(lhs["B_IHD_LEHSxCAT4_MEN"])
# B_IHD_LEHSxCAT1_WOMEN <- as.numeric(lhs["B_IHD_LEHSxCAT1_WOMEN"])
# B_IHD_LEHSxCAT2_WOMEN <- as.numeric(lhs["B_IHD_LEHSxCAT2_WOMEN"])
# B_IHD_SomeCxCAT1_MEN <- as.numeric(lhs["B_IHD_SomeCxCAT1_MEN"])
# B_IHD_SomeCxCAT2_MEN <- as.numeric(lhs["B_IHD_SomeCxCAT2_MEN"])
# B_IHD_SomeCxCAT3_MEN <- as.numeric(lhs["B_IHD_SomeCxCAT3_MEN"])
# B_IHD_SomeCxCAT4_MEN <- as.numeric(lhs["B_IHD_SomeCxCAT4_MEN"])
# B_IHD_SomeCxCAT1_WOMEN <- as.numeric(lhs["B_IHD_SomeCxCAT1_WOMEN"])
# B_IHD_SomeCxCAT2_WOMEN <- as.numeric(lhs["B_IHD_SomeCxCAT2_WOMEN"])
#
# #Former drinkers
# IHD_FD_MEN <- as.numeric(lhs["IHD_FD_MEN"])
# IHD_FD_WOMEN <- as.numeric(lhs["IHD_FD_WOMEN"])
# IHD_LEHSxFD_MEN <- as.numeric(lhs["IHD_LEHSxFD_MEN"])
# IHD_SomeCxFD_MEN <- as.numeric(lhs["IHD_SomeCxFD_MEN"])
# IHD_LEHSxFD_WOMEN <- as.numeric(lhs["IHD_LEHSxFD_WOMEN"])
# IHD_SomeCxFD_WOMEN <- as.numeric(lhs["IHD_SomeCxFD_WOMEN"])

# mutate(RR_IHD = ifelse(education=="LEHS" & sex=="m" & alc_gpd<=20,
#                        exp(B_IHD_LEHS_MEN + B_IHD_CAT1_MEN*alc_gpd + B_IHD_LEHSxCAT1_MEN*alc_gpd),
#                        ifelse(education=="LEHS" & sex=="m" & alc_gpd<=40,
#                               exp(B_IHD_LEHS_MEN + B_IHD_CAT2_MEN*alc_gpd + B_IHD_LEHSxCAT2_MEN*alc_gpd),
#                               ifelse(education=="LEHS" & sex=="m" & alc_gpd<=60,
#                                      exp(B_IHD_LEHS_MEN + B_IHD_CAT3_MEN*alc_gpd + B_IHD_LEHSxCAT3_MEN*alc_gpd),
#                                      ifelse(education=="LEHS" & sex=="m",
#                                             exp(B_IHD_LEHS_MEN + B_IHD_CAT4_MEN*alc_gpd + B_IHD_LEHSxCAT4_MEN*alc_gpd),
#                                             ifelse(education=="SomeC" & sex=="m" & alc_gpd<=20,
#                                                    exp(B_IHD_SomeC_MEN + B_IHD_CAT1_MEN*alc_gpd + B_IHD_SomeCxCAT1_MEN*alc_gpd),
#                                                    ifelse(education=="SomeC" & sex=="m" & alc_gpd<=40,
#                                                           exp(B_IHD_SomeC_MEN + B_IHD_CAT2_MEN*alc_gpd + B_IHD_SomeCxCAT2_MEN*alc_gpd),
#                                                           ifelse(education=="SomeC" & sex=="m" & alc_gpd<=60,
#                                                                  exp(B_IHD_SomeC_MEN + B_IHD_CAT3_MEN*alc_gpd + B_IHD_SomeCxCAT3_MEN*alc_gpd),
#                                                                  ifelse(education=="SomeC" & sex=="m",
#                                                                         exp(B_IHD_SomeC_MEN + B_IHD_CAT4_MEN*alc_gpd + B_IHD_SomeCxCAT4_MEN*alc_gpd),
#                                                                         ifelse(education=="College" & sex=="m" & alc_gpd<=20,
#                                                                                exp(B_IHD_CAT1_MEN*alc_gpd),
#                                                                                ifelse(education=="College" & sex=="m" & alc_gpd<=40,
#                                                                                       exp(B_IHD_CAT2_MEN*alc_gpd),
#                                                                                       ifelse(education=="College" & sex=="m" & alc_gpd<=60,
#                                                                                              exp(B_IHD_CAT3_MEN*alc_gpd),
#                                                                                              ifelse(education=="College" & sex=="m",
#                                                                                                     exp(B_IHD_CAT4_MEN*alc_gpd),
#                                                                                                     ifelse(education=="LEHS" & sex=="f" & alc_gpd<=20,
#                                                                                                            exp(B_IHD_LEHS_WOMEN + B_IHD_CAT1_WOMEN*alc_gpd + B_IHD_LEHSxCAT1_WOMEN*alc_gpd),
#                                                                                                            ifelse(education=="LEHS" & sex=="f",
#                                                                                                                   exp(B_IHD_LEHS_WOMEN + B_IHD_CAT2_WOMEN*alc_gpd + B_IHD_LEHSxCAT2_WOMEN*alc_gpd),
#                                                                                                                   ifelse(education=="SomeC" & sex=="f" & alc_gpd<=20,
#                                                                                                                          exp(B_IHD_SomeC_WOMEN + B_IHD_CAT1_WOMEN*alc_gpd + B_IHD_SomeCxCAT1_WOMEN*alc_gpd),
#                                                                                                                          ifelse(education=="SomeC" & sex=="f",
#                                                                                                                                 exp(B_IHD_SomeC_WOMEN + B_IHD_CAT2_WOMEN*alc_gpd + B_IHD_SomeCxCAT2_WOMEN*alc_gpd),
#                                                                                                                                 ifelse(education=="College" & sex=="f" & alc_gpd<=20,
#                                                                                                                                        exp(B_IHD_CAT1_WOMEN*alc_gpd),
#                                                                                                                                        ifelse(education=="College" & sex=="f",
#                                                                                                                                               exp(B_IHD_CAT2_WOMEN*alc_gpd), NA)))))))))))))))))),
#
#        RR_IHD = ifelse(formerdrinker==1 & education=="LEHS" & sex=="m",
#                        exp(B_IHD_LEHS_MEN + IHD_FD_MEN*alc_gpd + IHD_LEHSxFD_MEN*alc_gpd),
#                        ifelse(formerdrinker==1 & education=="SomeC" & sex=="m",
#                               exp(B_IHD_SomeC_MEN + IHD_FD_MEN*alc_gpd + IHD_SomeCxFD_MEN*alc_gpd),
#                               ifelse(formerdrinker==1 & education=="College" & sex=="m",
#                                      exp(IHD_FD_MEN*alc_gpd),
#                                      ifelse(formerdrinker==1 & education=="LEHS" & sex=="f",
#                                             exp(B_IHD_LEHS_WOMEN + IHD_FD_WOMEN*alc_gpd + IHD_LEHSxFD_WOMEN*alc_gpd),
#                                             ifelse(formerdrinker==1 & education=="SomeC" & sex=="f",
#                                                    exp(B_IHD_SomeC_WOMEN + IHD_FD_WOMEN*alc_gpd + IHD_SomeCxFD_WOMEN*alc_gpd),
#                                                    ifelse(formerdrinker==1 & education=="College" & sex=="f",
#                                                           exp(IHD_FD_WOMEN*alc_gpd), RR_IHD)))))))
