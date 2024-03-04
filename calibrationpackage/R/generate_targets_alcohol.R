#' function to generate alcohol use calibration targets
#' @param
#' @keywords microsimulation markov model targest
#' @export
#' @examples
#' generate_targets_alcohol
generate_targets_alcohol <- function(data){
  targets <- data %>%
    mutate(agecat = cut(microsim.init.age,
                          breaks=c(0,24,34,44,54,64,100),
                          labels=c("18-24","25-34","35-44","45-54",
                                   "55-64","65+"))) %>%
    rename(year=YEAR) %>%
    group_by(year, microsim.init.sex,microsim.init.race,agecat, microsim.init.education,AlcCAT, .drop=FALSE) %>% tally() %>%
    ungroup() %>%
    group_by(year, microsim.init.sex,microsim.init.race, agecat, microsim.init.education) %>%
    mutate(proptarget = n/sum(n),
           se = sqrt(proptarget * (1 - proptarget) / sum(n)),
           agecat = as.character(agecat)) %>%
    dplyr::select(-n)
  return(targets)
}
