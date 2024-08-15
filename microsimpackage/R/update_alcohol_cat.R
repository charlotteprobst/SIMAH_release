#' Updates alcohol category of individuals based on new gpd and new former drinker status
#' @param
#' @keywords microsimulation, alcohol
#' @export
#' @examples
#' update_alcohol_cat
update_alcohol_cat <- function(data){
  data <-
    data %>%
    mutate(alc_cat = ifelse(sex=="m" & alc_gpd>0 &
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
                                                                          "High risk",
                                                                          ifelse(alc_gpd==0, "Non-drinker",NA))))))))

  return(data)
}
