#' Calculates RR of liver cirrhosis through hepatitis pathway
#'
#' @param
#' @keywords cirrhosis hepatitis pathway
#' @export
#' @examples
#' hepatitis pathway
CirrhosisHepatitis <- function(data,lhs){
  data <- basepop %>%
    mutate(ageCAT = cut(microsim.init.age,
                        breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,79),
                        labels=c("18-24","25-29","30-34","35-39", "40-44",
                                 "45-49","50-54","55-59","60-64","65-69",
                                 "70-74","75-79")),
           cat = paste0(microsim.init.sex, ageCAT, microsim.init.education)) %>%
    dplyr::select(-ageCAT)
  BETA_HEPATITIS <- as.numeric(lhs["BETA_HEPATITIS"])
  # data$chronicHep <- ifelse(data$chronicB==1 | data$chronicC ==1, 1,0)
  # data$RRHep <- ifelse(data$chronicHep==1,
  #                      (data$gpd*BETA_HEPATITIS),
  #                      0)
  data$RRHep <- data$microsim.init.alc.gpd*BETA_HEPATITIS
  # data$gpd <- NULL
  # data$chronicHep <- NULL
  data$RRHep <- exp(data$RRHep)
  return(data)
}
