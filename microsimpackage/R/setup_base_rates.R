#' Sets up baseline mortality rates
#'
#' This function sets up baseline mortality rates for COD modelled in simulation
#' @param
#' @keywords baseline mortality
#' @export
#' @examples
#' setup_base_rates
setup_base_rates <- function(death_rates, diseases){
  # by age, sex and education initially
base_rates <- death_rates %>% pivot_longer(LVDCmort:RESTmort) %>%
    separate(cat, into=c("sex","agecat","race","education"), sep=c(1,6,9,13)) %>%
    group_by(year, sex, agecat, education, name) %>%
    summarise(value=sum(value)) %>%
    mutate(name = gsub("mort", "", name)) %>%
    filter(name %in% diseases) %>% filter(year==2000) %>%
    mutate(
      value = value*100, #inflate mortality rate by 100
           education = ifelse(education=="Some", "SomeC",
                              ifelse(education=="Coll","College",education)),
           cat = paste0(sex, agecat, education)) %>% ungroup() %>%
  dplyr::select(year, cat, name, value) %>%
    pivot_wider(names_from=name, values_from=value)
return(base_rates)
}
