#' Calculates RR of liver cirrhosis through hepatitis pathway
#'
#' @param
#' @keywords cirrhosis hepatitis pathway
#' @export
#' @examples
#' hepatitis pathway
CirrhosisHepatitis <- function(data,lhs){
  data$gpd <- ifelse(data$microsim.init.alc.gpd>200, 200, data$microsim.init.alc.gpd)
  BETA_HEPATITIS <- as.numeric(lhs["BETA_HEPATITIS"])
  data$chronicHep <- ifelse(data$chronicB==1 | data$chronicC ==1, 1,0)
  data$RRHep <- ifelse(data$chronicHep==1,
                       (data$gpd*BETA_HEPATITIS),
                       0)
  data$gpd <- NULL
  data$chronicHep <- NULL
  data$RRHep <- exp(data$RRHep)
  return(data)
}
