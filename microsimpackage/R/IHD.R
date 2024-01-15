#' Calculates RR of ischaemic heart disease
#'
#' @param
#' @keywords IHD
#' @export
#' @examples
#' Ischaemic heart disease mortality risk
IHD <- function(data,lhs){
  data <- data %>%
    mutate(ageCAT = cut(microsim.init.age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           cat = paste0(microsim.init.sex, ageCAT, microsim.init.education)) %>%
    dplyr::select(-ageCAT)
  B_IHD1_MEN <- as.numeric(lhs["B_IHD1_MEN"])
  B_IHD2_MEN <- as.numeric(lhs["B_IHD2_MEN"])
  B_IHD3_MEN <- as.numeric(lhs["B_IHD3_MEN"])
  B_IHD4_MEN <- as.numeric(lhs["B_IHD4_MEN"])
  B_IHD5_MEN <- as.numeric(lhs["B_IHD5_MEN"])
  B_IHD1_WOMEN <- as.numeric(lhs["B_IHD1_WOMEN"])
  B_IHD2_WOMEN <- as.numeric(lhs["B_IHD2_WOMEN"])
  B_IHD3_WOMEN <- as.numeric(lhs["B_IHD3_WOMEN"])
  B_IHD4_WOMEN <- as.numeric(lhs["B_IHD4_WOMEN"])
  B_IHD5_WOMEN <- as.numeric(lhs["B_IHD5_WOMEN"])
  IHD_FORMERDRINKER_MEN <- as.numeric(lhs["IHD_FORMERDRINKER_MEN"])
  IHD_FORMERDRINKER_WOMEN <- as.numeric(lhs["IHD_FORMERDRINKER_WOMEN"])  
  
  data <- data %>%
    mutate(RR_IHD = ifelse(microsim.init.alc.gpd<1.3 & microsim.init.sex=="m", 
                           exp(B_IHD1_MEN), 
                           ifelse(microsim.init.alc.gpd<24.99 & microsim.init.sex=="m", 
                                  exp(B_IHD2_MEN), 
                                  ifelse(microsim.init.alc.gpd<44.99 & microsim.init.sex=="m",
                                         exp(B_IHD3_MEN), 
                                         ifelse(microsim.init.alc.gpd<65 & microsim.init.sex=="m", 
                                                exp(B_IHD4_MEN), 
                                                ifelse(microsim.init.alc.gpd>=65 & microsim.init.sex=="m",
                                                       exp(B_IHD5_MEN),
                                                       ifelse(microsim.init.alc.gpd<1.3 & microsim.init.sex=="f", 
                                                              exp(B_IHD1_WOMEN), 
                                                              ifelse(microsim.init.alc.gpd<24.99 & microsim.init.sex=="f", 
                                                                     exp(B_IHD2_WOMEN), 
                                                                     ifelse(microsim.init.alc.gpd<44.99 & microsim.init.sex=="f",
                                                                            exp(B_IHD3_WOMEN), 
                                                                            ifelse(microsim.init.alc.gpd<65 & microsim.init.sex=="f", 
                                                                                   exp(B_IHD4_WOMEN), 
                                                                                   ifelse(microsim.init.alc.gpd>=65 & microsim.init.sex=="f",
                                                                                          exp(B_IHD5_WOMEN), NA)))))))))),
           RR_IHD = ifelse(formerdrinker==1 & microsim.init.sex=="m",
                           exp(IHD_FORMERDRINKER_MEN), 
                           ifelse(formerdrinker==1 & microsim.init.sex=="f", 
                                  exp(IHD_FORMERDRINKER_WOMEN), RR_IHD)))
  
  #age-specific: mean 19-55 year old   
  # B_IHD1 <- as.numeric(lhs["B_IHD1"])
  # B_IHD2 <- as.numeric(lhs["B_IHD2"])
  # B_IHD3 <- as.numeric(lhs["B_IHD3"])
  # B_IHD4 <- as.numeric(lhs["B_IHD4"])
  # B_IHD5 <- as.numeric(lhs["B_IHD5"])
  # IHD_FORMERDRINKER <- as.numeric(lhs["IHD_FORMERDRINKER"])
  #   data <- data %>%
  #   mutate(RR_IHD = ifelse(microsim.init.alc.gpd<= 1.3, exp(B_IHD1), 
  #                                  ifelse(microsim.init.alc.gpd<=24.99, exp(B_IHD2), 
  #                                         ifelse(microsim.init.alc.gpd<=44.99, exp(B_IHD3), 
  #                                                ifelse(microsim.init.alc.gpd<=64.99, exp(B_IHD4), exp(B_IHD5) )))),
  #         RR_IHD = ifelse(formerdrinker==1,
  #                         exp(IHD_FORMERDRINKER), RR_IHD))
  return(data)
}
