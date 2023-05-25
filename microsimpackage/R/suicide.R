#' Calculates RR of suicide mortality
#'
#' @param
#' @keywords SUICIDE
#' @export
#' @examples
#' SUICIDE mortality risk
SUICIDE <- function(data,lhs){
  data <- data %>%
    mutate(ageCAT = cut(microsim.init.age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           cat = paste0(microsim.init.sex, ageCAT, microsim.init.education)) %>%
    dplyr::select(-ageCAT)
  B_SUICIDE_MEN <- as.numeric(lhs["B_SUICIDE_MEN"])
  B_SUICIDE_WOMEN <- as.numeric(lhs["B_SUICIDE_WOMEN"])
  SUICIDE_FORMERDRINKER_MEN <- as.numeric(lhs["SUICIDE_FORMERDRINKER_MEN"])
  SUICIDE_FORMERDRINKER_WOMEN <- as.numeric(lhs["SUICIDE_FORMERDRINKER_WOMEN"])
  data <- data %>%
    mutate(RR_IJ = ifelse(microsim.init.alc.gpd<50 & microsim.init.sex=="m",
                           exp(0 + B_SUICIDE_MEN*microsim.init.alc.gpd),
                           ifelse(microsim.init.alc.gpd>=50 & microsim.init.sex=="m",
                                  exp(0 + B_SUICIDE_MEN*50),
                                  ifelse(microsim.init.alc.gpd<50 & microsim.init.sex=="f",
                                         exp(0 + B_SUICIDE_WOMEN*microsim.init.alc.gpd),
                                         ifelse(microsim.init.alc.gpd>=50 & microsim.init.sex=="f",
                                                exp(0 + B_SUICIDE_WOMEN*50), NA)))),
           RR_IJ = ifelse(formerdrinker==1 & microsim.init.sex=="m",
                               exp(SUICIDE_FORMERDRINKER_MEN),
                               ifelse(formerdrinker==1 & microsim.init.sex=="f",
                                      exp(SUICIDE_FORMERDRINKER_WOMEN), RR_IJ)))
  return(data)
}

# notes:
# no upper limit for GPD indicated in shared doc but figures stop by 50 GPD so I have used this limit
# can either have (1) no threshold or (2) set to the maximum that was available in the data - 36.6gpd for women and 82.5gpd for men
# not sure why the RR for former drinker is exponentiated; I have used the code from the cirrhosis_all.R where this was done but it's not clear to me why
