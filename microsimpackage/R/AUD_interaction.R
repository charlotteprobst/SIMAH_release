#' Calculates RR of AUD - with SES interaction effects
#'
#' @param
#' @keywords AUD interaction effects
#' @export
#' @examples
#' AUD including SES interaction effects
#' Results given in standard drinks per day (14 grams per day)
AUDInteraction <- function(data,lhs){
  data$microsim.init.alc.gpd <- ifelse(data$microsim.init.alc.gpd>=150, 150,
                                       data$microsim.init.alc.gpd)
  data <- data %>%
    mutate(ageCAT = cut(microsim.init.age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           cat = paste0(microsim.init.sex, ageCAT, microsim.init.education)) %>%
    dplyr::select(-ageCAT)
    B_AUD_LEHS <- as.numeric(lhs["B_AUD_LEHS"])
    B_AUD_SomeC <- as.numeric(lhs["B_AUD_SomeC"])
    B_AUD_GPD <- as.numeric(lhs["B_AUD_GPD"])
    B_AUD_LEHSxGPD <- as.numeric(lhs["B_AUD_LEHSxGPD"])
    B_AUD_SomeCxGPD <- as.numeric(lhs["B_AUD_SomeCxGPD"])
  # B_AUD_LEHS_MEN <- as.numeric(lhs["B_AUD_LEHS_MEN"])
  # B_AUD_SomeC_MEN <- as.numeric(lhs["B_AUD_SomeC_MEN"])
  # B_AUD_LEHS_WOMEN <- as.numeric(lhs["B_AUD_LEHS_WOMEN"])
  # B_AUD_SomeC_WOMEN <- as.numeric(lhs["B_AUD_SomeC_WOMEN"])
  # B_AUD_GPD_MEN <- as.numeric(lhs["B_AUD_GPD_MEN"])
  # B_AUD_GPD_WOMEN <- as.numeric(lhs["B_AUD_GPD_WOMEN"])
  # B_AUD_LEHSxGPD_MEN <- as.numeric(lhs["B_AUD_LEHSxGPD_MEN"])
  # B_AUD_SomeCxGPD_MEN <- as.numeric(lhs["B_AUD_SomeCxGPD_MEN"])
  # B_AUD_LEHSxGPD_WOMEN <- as.numeric(lhs["B_AUD_LEHSxGPD_WOMEN"])
  # B_AUD_SomeCxGPD_WOMEN <- as.numeric(lhs["B_AUD_SomeCxGPD_WOMEN"])

  #Former drinkers
    AUD_FD_LEHS <- as.numeric(lhs["AUD_FD_LEHS"])
    AUD_FD_SomeC <- as.numeric(lhs["AUD_FD_SomeC"])
    AUD_FD <- as.numeric(lhs["AUD_FD"])
    AUD_LEHSxFD <- as.numeric(lhs["AUD_LEHSxFD"])
    AUD_SomeCxFD <- as.numeric(lhs["AUD_SomeCxFD"])
  # AUD_FD_LEHS_MEN <- as.numeric(lhs["AUD_FD_LEHS_MEN"])
  # AUD_FD_SomeC_MEN <- as.numeric(lhs["AUD_FD_SomeC_MEN"])
  # AUD_FD_LEHS_WOMEN <- as.numeric(lhs["AUD_FD_LEHS_WOMEN"])
  # AUD_FD_SomeC_WOMEN <- as.numeric(lhs["AUD_FD_SomeC_WOMEN"])
  # AUD_FD_MEN <- as.numeric(lhs["AUD_FD_MEN"])
  # AUD_FD_WOMEN <- as.numeric(lhs["AUD_FD_WOMEN"])
  # AUD_LEHSxFD_MEN <- as.numeric(lhs["AUD_LEHSxFD_MEN"])
  # AUD_SomeCxFD_MEN <- as.numeric(lhs["AUD_SomeCxFD_MEN"])
  # AUD_LEHSxFD_WOMEN <- as.numeric(lhs["AUD_LEHSxFD_WOMEN"])
  # AUD_SomeCxFD_WOMEN <- as.numeric(lhs["AUD_SomeCxFD_WOMEN"])

  data <- data %>%
    mutate(RR_AUD = ifelse(microsim.init.education=="LEHS",
                           exp(B_AUD_LEHS + B_AUD_GPD*(microsim.init.alc.gpd/14) + B_AUD_LEHSxGPD*(microsim.init.alc.gpd/14)),
                           ifelse(microsim.init.education=="SomeC",
                                  exp(B_AUD_SomeC + B_AUD_GPD*(microsim.init.alc.gpd/14) + B_AUD_SomeCxGPD*(microsim.init.alc.gpd/14)),
                                  exp(B_AUD_GPD*(microsim.init.alc.gpd/14)))),
           RR_AUD = ifelse(formerdrinker==1 & microsim.init.education=="LEHS",
                           exp(AUD_FD_LEHS + AUD_FD + AUD_LEHSxFD),
                           ifelse(formerdrinker==1 & microsim.init.education=="SomeC",
                                  exp(AUD_FD_SomeC + AUD_FD + AUD_SomeCxFD),
                                  ifelse(formerdrinker==1 & microsim.init.education=="College",
                                         exp(AUD_FD), RR_AUD))))
      return(data)
}
   
    # mutate(RR_AUD = ifelse(microsim.init.education=="LEHS" & microsim.init.sex=="m",
    #                             exp(B_AUD_LEHS_MEN + B_AUD_GPD_MEN*microsim.init.alc.gpd + B_AUD_LEHSxGPD_MEN*microsim.init.alc.gpd),
    #                             ifelse(microsim.init.education=="SomeC" & microsim.init.sex=="m",
    #                                   exp(B_AUD_SomeC_MEN + B_AUD_GPD_MEN*microsim.init.alc.gpd + B_AUD_SomeCxGPD_MEN*microsim.init.alc.gpd),
    #                                   ifelse(microsim.init.education=="College" & microsim.init.sex=="m",
    #                                          exp(B_AUD_GPD_MEN*microsim.init.alc.gpd),
    #                                          ifelse(microsim.init.education=="LEHS" & microsim.init.sex=="f",
    #                                                 exp(B_AUD_LEHS_WOMEN + B_AUD_GPD_WOMEN*microsim.init.alc.gpd + B_AUD_LEHSxGPD_WOMEN*microsim.init.alc.gpd),
    #                                                 ifelse(microsim.init.education=="SomeC" & microsim.init.sex=="f",
    #                                                       exp(B_AUD_SomeC_WOMEN + B_AUD_GPD_WOMEN*microsim.init.alc.gpd + B_AUD_SomeCxGPD_WOMEN*microsim.init.alc.gpd),
    #                                                       ifelse(microsim.init.education=="College" & microsim.init.sex=="f",
    #                                                              exp(B_AUD_GPD_WOMEN*microsim.init.alc.gpd), NA)))))),
    #        RR_AUD = ifelse(formerdrinker==1 & microsim.init.education=="LEHS" & microsim.init.sex=="m",
    #                              exp(AUD_FD_LEHS_MEN + AUD_FD_MEN*microsim.init.alc.gpd + AUD_LEHSxFD_MEN*microsim.init.alc.gpd),
    #                              ifelse(formerdrinker==1 & microsim.init.education=="SomeC" & microsim.init.sex=="m",
    #                                     exp(AUD_FD_SomeC_MEN + AUD_FD_MEN*microsim.init.alc.gpd + AUD_SomeCxFD_MEN*microsim.init.alc.gpd),
    #                                     ifelse(formerdrinker==1 & microsim.init.education=="College" & microsim.init.sex=="m",
    #                                            exp(AUD_FD_MEN*microsim.init.alc.gpd),
    #                                            ifelse(formerdrinker==1 & microsim.init.education=="LEHS" & microsim.init.sex=="f",
    #                                                   exp(AUD_FD_LEHS_WOMEN + AUD_FD_WOMEN*microsim.init.alc.gpd + AUD_LEHSxFD_WOMEN*microsim.init.alc.gpd),
    #                                                   ifelse(formerdrinker==1 & microsim.init.education=="SomeC" & microsim.init.sex=="f",
    #                                                         exp(AUD_FD_SomeC_WOMEN + AUD_FD_WOMEN*microsim.init.alc.gpd + AUD_SomeCxFD_WOMEN*microsim.init.alc.gpd),
    #                                                         ifelse(formerdrinker==1 & microsim.init.education=="College" & microsim.init.sex=="f",
    #                                                                exp(AUD_FD_WOMEN*microsim.init.alc.gpd), RR_AUD)))))))


# test for exploring the RR dose response curve
#test <- basepop %>% dplyr::select(microsim.init.sex, microsim.init.education, microsim.init.alc.gpd, RR_AUD) %>% distinct()
#ggplot(test, aes(x=microsim.init.alc.gpd, y=RR_AUD)) + geom_point() + facet_grid(cols=vars(microsim.init.education), rows=vars(microsim.init.sex), scales="free")
