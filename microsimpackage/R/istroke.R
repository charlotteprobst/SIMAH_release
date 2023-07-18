#' Calculates RR of ischaemic stroke 
#'
#' @param
#' @keywords STROKE
#' @export
#' @examples
#' ISCHAEMIC STROKE mortality risk ##NOT CORRECT - function is for haemorrhagic stroke
ISTR <- function(data,lhs){
  data <- data %>%
    mutate(ageCAT = cut(microsim.init.age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           cat = paste0(microsim.init.sex, ageCAT, microsim.init.education)) %>%
    dplyr::select(-ageCAT)
  B_ISTR_MEN <- as.numeric(lhs["B_ISTR_MEN"])
  B_ISTR_WOMEN <- as.numeric(lhs["B_ISTR_WOMEN"])
  ISTR_FORMERDRINKER <- as.numeric(lhs["ISTR_FORMERDRINKER"])
  data <- data %>%
    mutate(RR_ISTR = ifelse(microsim.init.alc.gpd<=1 & microsim.init.sex=="m",
                          1-microsim.init.alc.gpd*(1-exp(B_ISTR_MEN*((1+0.0028572082519531)/100))),
                          ifelse(microsim.init.alc.gpd>1 & microsim.init.sex=="m",
                                 exp(0 + B_ISTR_MEN*((microsim.init.alc.gpd+0.0028572082519531)/100)),
                                 ifelse(microsim.init.alc.gpd<=1 & microsim.init.sex=="f",
                                        1-microsim.init.alc.gpd*(1-exp(B_ISTR_WOMEN*((1+0.0028572082519531)/100))),
                                        ifelse(microsim.init.alc.gpd>1 & microsim.init.sex=="f",
                                               exp(0 + B_ISTR_WOMEN*((microsim.init.alc.gpd+0.0028572082519531)/100)), NA)))),
            RR_ISTR = ifelse(formerdrinker==1, exp(ISTR_FORMERDRINKER), RR_ISTR))
  return(data)
}
