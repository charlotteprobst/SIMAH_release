#' Sets up baseline mortality counts
#'
#' This function sets up baseline mortality counts for COD modelled in simulation and inflates by a factor
#' @param
#' @keywords baseline mortality
#' @export
#' @examples
#' setup_base_counts
setup_base_counts <- function(death_counts, diseases, inflation_factors, age_inflated){

base_counts <- death_counts %>% pivot_longer(LVDCmort:RESTmort) %>%
    mutate(agecat = case_when(
      grepl("18-24", cat) ~ "18-24",
      grepl("25-29", cat) ~ "25-34",
      grepl("30-34", cat) ~ "25-34",
      grepl("35-39", cat) ~ "35-44",
      grepl("40-44", cat) ~ "35-44",
      grepl("45-49", cat) ~ "45-54",
      grepl("50-54", cat) ~ "45-54",
      grepl("55-59", cat) ~ "55-64",
      grepl("60-64", cat) ~ "55-64",
      grepl("65-69", cat) ~ "65-74",
      grepl("70-74", cat) ~ "65-74",
      grepl("75-79", cat) ~ "75-79"),
      sex = case_when(
        grepl("m",cat) ~ "m",
        grepl("f",cat) ~ "f"),
      education = case_when(
        grepl("LEHS",cat) ~ "LEHS",
        grepl("SomeC",cat) ~ "SomeC",
        grepl("College",cat) ~ "College"
      ),
      year = as.factor(year),
      sex = as.factor(sex),
      agecat = as.factor(agecat),
      education=as.factor(education),
      name=as.factor(name)
      ) %>%
  group_by(year, sex, agecat, education, name) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(year, sex, agecat, education, name, fill = list(value = 0)) %>%
    mutate(name = gsub("mort", "", name)) %>%
    filter(name %in% diseases) %>% filter(year==2000) %>%
    mutate(inflation_factor = ifelse(agecat %in% age_inflated[[1]], inflation_factors[1],
                                     ifelse(agecat %in% age_inflated[[2]], inflation_factors[2], NA)),
      value = value*inflation_factor, #inflate mortality rate
           cat = paste0(sex, agecat, education)) %>% ungroup() %>%
  dplyr::select(year, cat, name, value) %>%
    pivot_wider(names_from=name, values_from=value)
return(base_counts)
}
