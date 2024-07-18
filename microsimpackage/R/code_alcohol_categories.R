#' Codes alcohol categories for the model
#'
#' @param
#' @keywords alcohol categories
#' @export
#' @examples
#' code alcohol categories
code_alcohol_categories <- function(data){
# set up the grams per day categories for the baseline population and brfss donor populations
data <- data %>%
  mutate(AlcCAT = ifelse(formerdrinker==1, "Former drinker",
                         ifelse(formerdrinker!=1 & microsim.init.alc.gpd==0, "Lifetime abstainer",
                                ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>0 &
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
                                                                          "High risk", NA)))))))),
         AlcCAT = ifelse(AlcCAT=="Former drinker", "Non-drinker",
                         ifelse(AlcCAT=="Lifetime abstainer", "Non-drinker",AlcCAT))
         )
return(data)
}
