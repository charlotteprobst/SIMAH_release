#' Updates alcohol category of individuals based on new gpd and new former drinker status
#' @param
#' @keywords microsimulation, alcohol
#' @export
#' @examples
#' update_alcohol_cat
update_alcohol_cat <- function(data){
  data <-
    data %>%
    mutate(formerdrinker = ifelse(formerdrinker==1 & microsim.init.alc.gpd==0, 1,
                                  ifelse(formerdrinker==1 & microsim.init.alc.gpd>0, 0,
                                         formerdrinker)),
           AlcCAT = ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>0 &
                                         microsim.init.alc.gpd<=40, "Low risk",
                                       ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>0 &
                                                microsim.init.alc.gpd<=20, "Low risk",
                                              ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>40 &
                                                       microsim.init.alc.gpd<=60, "Medium risk",
                                                     ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>20 &
                                                              microsim.init.alc.gpd<=40, "Medium risk",
                                                            ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>60,
                                                                   "High risk",
                                                                   ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>40,
                                                                          "High risk",
                                                                          ifelse(microsim.init.alc.gpd==0, "Non-drinker",NA))))))))

  return(data)
}
