#' Updates baseline mortality rates over time for different diseases according to parameter values
#'
#' @param
#' @keywords update base rate
#' @export
#' @examples
#' updating base rates
update_base_rate <- function(rates, lhs, y){
  data <- rates
  for(i in unique(diseases)){
    update <- ifelse(round(as.numeric(lhs["BASERATE_YEAR"]))<=y,1,0)
    multiplier_men <- as.numeric(lhs["BASERATEFACTOR_MEN"])
    multiplier_women <- as.numeric(lhs["BASERATEFACTOR_WOMEN"])
    data <- data %>%
      mutate(sex = substr(cat, 1,1),
             educ = substr(cat,7,10),
        !!paste0("rate_", i):= ifelse(update==1 & sex=="m" & educ=="LEHS"|educ=="Some",
                                      !!as.name(paste0("rate_",i)) + (!!as.name(paste0("rate_",i))*multiplier_men),
                                      ifelse(update==1 & sex=="f" & educ=="LEHS" | educ=="Some",
                                             !!as.name(paste0("rate_",i)) + (!!as.name(paste0("rate_",i))*multiplier_women),
                                      !!as.name(paste0("rate_", i)))),
        ) %>%
      dplyr::select(-c(sex,educ))

  }
  return(data)
}
