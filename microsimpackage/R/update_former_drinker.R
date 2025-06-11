#' Updates new former drinker status
#' @param
#' @keywords microsimulation, alcohol
#' @export
#' @examples
#' update_former_drinker
update_former_drinker <- function(data){
  # lifetime abstainer category that is being updated
  # then calculate the former drinker category

# Old method
#   data <-
#     data %>%
#     mutate(formerdrinker = ifelse(formerdrinker==1 & newgpd==0, 1, #if former drinker in previous year and still gpd = 0 former drinker
#                                   ifelse(formerdrinker==1 & newgpd>0, 0, #if former drinker in prev year and now gpd >0 not former drinker
#                                          ifelse(alc_gpd==0 & newgpd>1, 0, #if gpd in prev year = 0 and now gpd>0 not former drinker
#                                                 ifelse(alc_gpd>1 & newgpd==0, 1, #if gpd in prev year >1 and now gpd==0 former drinker
#                                          formerdrinker)))))

# New method
  temp <- data %>% filter(drinkingstatus==0) %>%
    group_by(agecat, sex,formerdrinker) %>%
    tally %>%
    ungroup() %>%
    group_by(agecat, sex) %>%
    mutate(prop_former_drinker=n/sum(n))

  temp2 <- temp %>% filter(formerdrinker==1) %>%
    dplyr::select(-(formerdrinker))

  temp3 <- left_join(data, temp2, by=c('sex','agecat'))

  temp3$prob <- runif(nrow(temp3))

  data <- temp3 %>% mutate(
      formerdrinker = ifelse(alc_gpd==0 & prob<=prop_former_drinker, 1, 0)) %>% dplyr::select(-c(prob,n,prop_former_drinker))

  return(data)
}
