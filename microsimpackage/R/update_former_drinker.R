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
#                                          ifelse(microsim.init.alc.gpd==0 & newgpd>1, 0, #if gpd in prev year = 0 and now gpd>0 not former drinker
#                                                 ifelse(microsim.init.alc.gpd>1 & newgpd==0, 1, #if gpd in prev year >1 and now gpd==0 former drinker
#                                          formerdrinker)))))

# New method
  temp <- data %>% filter(microsim.init.drinkingstatus==0) %>%
    group_by(agecat, microsim.init.sex,formerdrinker) %>%
    tally %>%
    ungroup() %>%
    group_by(agecat, microsim.init.sex) %>%
    mutate(prop_former_drinker=n/sum(n))

  temp2 <- temp %>% filter(formerdrinker==1) %>%
    dplyr::select(-(formerdrinker))

  temp3 <- left_join(data, temp2)

  temp3$prob <- runif(nrow(temp3))

  data <- temp3 %>% mutate(
      formerdrinker = ifelse(newgpd==0 & prob<=prop_former_drinker, 1, 0)) %>% dplyr::select(-prob)

  return(data)
}
