#' Calculates RR of AUD - with SES interaction effects
#'
#' @param
#' @keywords AUD interaction effects
#' @export
#' @examples
#' AUD including SES interaction effects
#' Results given in standard drinks per day (14 grams per day)
AUDInteraction <- function(data,lhs){
  data$alc_gpd <- ifelse(data$alc_gpd>=150, 150,
                                       data$alc_gpd)
  data <- data %>%
    mutate(ageCAT = cut(age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           cat = paste0(sex, ageCAT, education)) %>%
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
    mutate(RR_AUD = ifelse(education=="LEHS",
                           exp(B_AUD_LEHS + B_AUD_GPD*(alc_gpd/14) + B_AUD_LEHSxGPD*(alc_gpd/14)),
                           ifelse(education=="SomeC",
                                  exp(B_AUD_SomeC + B_AUD_GPD*(alc_gpd/14) + B_AUD_SomeCxGPD*(alc_gpd/14)),
                                  exp(B_AUD_GPD*(alc_gpd/14)))),
           RR_AUD = ifelse(formerdrinker==1 & education=="LEHS",
                           exp(AUD_FD_LEHS + AUD_FD + AUD_LEHSxFD),
                           ifelse(formerdrinker==1 & education=="SomeC",
                                  exp(AUD_FD_SomeC + AUD_FD + AUD_SomeCxFD),
                                  ifelse(formerdrinker==1 & education=="College",
                                         exp(AUD_FD), RR_AUD))))
      return(data)
}

    # mutate(RR_AUD = ifelse(education=="LEHS" & sex=="m",
    #                             exp(B_AUD_LEHS_MEN + B_AUD_GPD_MEN*alc_gpd + B_AUD_LEHSxGPD_MEN*alc_gpd),
    #                             ifelse(education=="SomeC" & sex=="m",
    #                                   exp(B_AUD_SomeC_MEN + B_AUD_GPD_MEN*alc_gpd + B_AUD_SomeCxGPD_MEN*alc_gpd),
    #                                   ifelse(education=="College" & sex=="m",
    #                                          exp(B_AUD_GPD_MEN*alc_gpd),
    #                                          ifelse(education=="LEHS" & sex=="f",
    #                                                 exp(B_AUD_LEHS_WOMEN + B_AUD_GPD_WOMEN*alc_gpd + B_AUD_LEHSxGPD_WOMEN*alc_gpd),
    #                                                 ifelse(education=="SomeC" & sex=="f",
    #                                                       exp(B_AUD_SomeC_WOMEN + B_AUD_GPD_WOMEN*alc_gpd + B_AUD_SomeCxGPD_WOMEN*alc_gpd),
    #                                                       ifelse(education=="College" & sex=="f",
    #                                                              exp(B_AUD_GPD_WOMEN*alc_gpd), NA)))))),
    #        RR_AUD = ifelse(formerdrinker==1 & education=="LEHS" & sex=="m",
    #                              exp(AUD_FD_LEHS_MEN + AUD_FD_MEN*alc_gpd + AUD_LEHSxFD_MEN*alc_gpd),
    #                              ifelse(formerdrinker==1 & education=="SomeC" & sex=="m",
    #                                     exp(AUD_FD_SomeC_MEN + AUD_FD_MEN*alc_gpd + AUD_SomeCxFD_MEN*alc_gpd),
    #                                     ifelse(formerdrinker==1 & education=="College" & sex=="m",
    #                                            exp(AUD_FD_MEN*alc_gpd),
    #                                            ifelse(formerdrinker==1 & education=="LEHS" & sex=="f",
    #                                                   exp(AUD_FD_LEHS_WOMEN + AUD_FD_WOMEN*alc_gpd + AUD_LEHSxFD_WOMEN*alc_gpd),
    #                                                   ifelse(formerdrinker==1 & education=="SomeC" & sex=="f",
    #                                                         exp(AUD_FD_SomeC_WOMEN + AUD_FD_WOMEN*alc_gpd + AUD_SomeCxFD_WOMEN*alc_gpd),
    #                                                         ifelse(formerdrinker==1 & education=="College" & sex=="f",
    #                                                                exp(AUD_FD_WOMEN*alc_gpd), RR_AUD)))))))


# test for exploring the RR dose response curve
#test <- basepop %>% dplyr::select(sex, education, alc_gpd, RR_AUD) %>% distinct()
#ggplot(test, aes(x=alc_gpd, y=RR_AUD)) + geom_point() + facet_grid(cols=vars(education), rows=vars(sex), scales="free")
