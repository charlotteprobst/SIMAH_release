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
  B_ISTR1 <- as.numeric(lhs["B_ISTR1"])
  B_ISTR2 <- as.numeric(lhs["B_ISTR2"])
  B_ISTR3 <- as.numeric(lhs["B_ISTR3"])
  B_ISTR4 <- as.numeric(lhs["B_ISTR4"])
  ISTR_FORMERDRINKER <- as.numeric(lhs["ISTR_FORMERDRINKER"])
  data <- data %>%
    mutate(RR_ISTR = ifelse(microsim.init.alc.gpd<= 1.3, exp(B_ISTR1), 
                            ifelse(microsim.init.alc.gpd<=24, exp(B_ISTR2), 
                                   ifelse(microsim.init.alc.gpd<=48, exp(B_ISTR3), exp(B_ISTR4)))),
            RR_ISTR = ifelse(formerdrinker==1, exp(ISTR_FORMERDRINKER), RR_ISTR))
  return(data)
}
