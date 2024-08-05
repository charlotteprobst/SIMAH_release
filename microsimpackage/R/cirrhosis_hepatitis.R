#' Calculates RR of liver cirrhosis through hepatitis pathway
#'
#' @param
#' @keywords cirrhosis hepatitis pathway
#' @export
#' @examples
#' hepatitis pathway
CirrhosisHepatitis <- function(data,lhs){
  data <- data %>%
    mutate(ageCAT = cut(age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           cat = paste0(sex, ageCAT, education)) %>%
    dplyr::select(-ageCAT)
  B_HEPATITIS1 <- as.numeric(lhs["B_HEPATITIS1"])
  B_HEPATITIS2 <- as.numeric(lhs["B_HEPATITIS2"])
  data$RR_HLVDC <- ifelse(data$alc_gpd<146.3, exp(0 + B_HEPATITIS1*data$alc_gpd +
                                                               B_HEPATITIS2*(data$alc_gpd^2)),
                       exp(0 + B_HEPATITIS1*146.3 + B_HEPATITIS2*146.3))


  # data$gpd <- NULL
  # data$chronicHep <- NULL
  return(data)
}
