#' Sets up baseline mortality counts
#'
#' This function sets up baseline mortality counts for COD modelled in simulation and inflates by a factor
#' @param
#' @keywords baseline mortality
#' @export
#' @examples
#' setup_base_counts
setup_base_counts <- function(death_counts, diseases, inflation_factors, age_categories){

base_counts <- death_counts %>% pivot_longer(LVDCmort:RESTmort) %>%
    separate(cat, into=c("sex","agecat","race","education"), sep=c(1,6,9,13)) %>%
    mutate(agecat = ifelse(agecat=="25-29" | agecat=="30-34","25-34",
                           ifelse(agecat=="35-39" | agecat=="40-44","35-44",
                                  ifelse(agecat=="45-49" | agecat=="50-54", "45-54",
                                         ifelse(agecat=="55-59" | agecat=="60-64", "55-64",
                                                ifelse(agecat=="65-69" | agecat=="70-74", "65-74",
                                                       agecat)))))) %>%
    group_by(year, sex, agecat, education, name) %>%
    summarise(value=sum(value),
              ) %>%
    mutate(name = gsub("mort", "", name)) %>%
    filter(name %in% diseases) %>% filter(year==2000) %>%
    mutate(inflation_factor = ifelse(agecat %in% age_categories, inflation_factors[1], inflation_factors[2]),
      value = value*inflation_factor, #inflate mortality rate
           education = ifelse(education=="Some", "SomeC",
                              ifelse(education=="Coll","College",education)),
           cat = paste0(sex, agecat, education)) %>% ungroup() %>%
  dplyr::select(year, cat, name, value) %>%
    pivot_wider(names_from=name, values_from=value)
return(base_counts)
}
