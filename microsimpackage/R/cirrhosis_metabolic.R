#' Calculates RR of liver cirrhosis through metabolic pathway
#'
#' @param
#' @keywords cirrhosis metabolic pathway
#' @export
#' @examples
#' cirrhosis_metabolic pathway
cirrhosis_metabolic <- function(data,lhsSample){
  data$gpd <- ifelse(data$microsim.init.alc.gpd>200, 200, data$microsim.init.alc.gpd)

    METABOLIC_BETA1_MALE <- as.numeric(lhsSample["METABOLIC_BETA1_MALE"])
    METABOLIC_BETA2_MALE <- as.numeric(lhsSample["METABOLIC_BETA2_MALE"])
    METABOLIC_BETA1_FEMALE <- as.numeric(lhsSample["METABOLIC_BETA1_FEMALE"])
    METABOLIC_BETA2_FEMALE <- as.numeric(lhsSample["METABOLIC_BETA2_FEMALE"])
    data$RRMetabolic <- ifelse(data$microsim.init.BMI>=30 & data$microsim.init.sex=="m" &
                                 data$Cirrhosis_risk==1,
                               (METABOLIC_BETA1_MALE*((((data$gpd+2)/1000)^-.5)-9.537024026) +
                                  METABOLIC_BETA2_MALE*((((data$gpd+2)/1000)^-.5)*
                                                          log((data$gpd+2)/1000)+43.0154401)),
                               ifelse(data$microsim.init.BMI>=30 & data$microsim.init.sex=="f" &
                                        data$Cirrhosis_risk==1,
                                      (METABOLIC_BETA1_FEMALE*((data$gpd+2)/100)^3-.0000696286 +
                                         METABOLIC_BETA2_FEMALE*((data$gpd+2)/100)^3*
                                         log((data$gpd+2)/100)+.0002221693), 0))
  data$RRMetabolic <- ifelse(data$RRMetabolic<0, 0,data$RRMetabolic)
  data$RRMetabolic <- exp(data$RRMetabolic)
  return(data)
}
