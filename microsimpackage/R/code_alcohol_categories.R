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
  mutate(alc_cat = ifelse(formerdrinker==1, "Former drinker",
                         ifelse(formerdrinker!=1 & alc_gpd==0, "Lifetime abstainer",
                                ifelse(sex=="m" & alc_gpd>0 &
                                         alc_gpd<=40, "Low risk",
                                       ifelse(sex=="f" & alc_gpd>0 &
                                                alc_gpd<=20, "Low risk",
                                              ifelse(sex=="m" & alc_gpd>40 &
                                                       alc_gpd<=60, "Medium risk",
                                                     ifelse(sex=="f" & alc_gpd>20 &
                                                              alc_gpd<=40, "Medium risk",
                                                            ifelse(sex=="m" & alc_gpd>60,
                                                                   "High risk",
                                                                   ifelse(sex=="f" & alc_gpd>40,
                                                                          "High risk", NA)))))))),
         alc_cat = ifelse(alc_cat=="Former drinker", "Non-drinker",
                         ifelse(alc_cat=="Lifetime abstainer", "Non-drinker",alc_cat))
         )
return(data)
}
