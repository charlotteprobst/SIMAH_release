#' Calculates baseline mortality rates for different diseases
#'
#' @param
#' @keywords calculate base rate
#' @export
#' @examples
#' base rates
postprocess_mortality <- function(DiseaseSummary, diseases, death_counts){
  disease <- unique(diseases)
  Diseases <- do.call(rbind, DiseaseSummary)
  death_counts_new <- death_counts %>% pivot_longer(cols = contains("mort")) %>%
    # separate(cat, into=c("sex","agecat","race","education"), sep=c(1,6,9,13)) %>%
    mutate(agecat = ifelse(grepl("25-29",cat) | grepl("30-34",cat), "25-34",
                           ifelse(grepl("35-39",cat) | grepl("40-44",cat),"35-44",
                                  ifelse(grepl("45-49",cat) | grepl("50-54",cat), "45-54",
                                         ifelse(grepl("55-59",cat) | grepl("60-64",cat), "55-64",
                                                ifelse(grepl("65-69",cat) | grepl("70-74",cat), "65-74",
                                                       ifelse(grepl("18-24",cat), "18-24",
                                                              ifelse(grepl("75-79",cat), "75-79",NA))))))),
           sex = case_when(
             grepl("f",cat) ~ "f",
             grepl("m",cat) ~ "m"),
           education=case_when(
             grepl("LEHS",cat) ~ "LEHS",
             grepl("SomeC",cat) ~ "SomeC",
             grepl("College",cat) ~ "College"),
           race = case_when(
             grepl("White", cat) ~ "White",
             grepl("Black", cat) ~ "Black",
             grepl("Hispanic",cat) ~ "Hispanic",
             grepl("Others", cat) ~ "Others")
           ) %>%
    group_by(year, sex, agecat, race, education, name) %>%
    summarise(value=sum(value)) %>%
    mutate(name = gsub("mort", "", name)) %>%
    filter(name %in% diseases) %>%
    mutate(
      education = ifelse(education=="Some", "SomeC",
                         ifelse(education=="Coll","College",education)),
      cat = paste0(sex, agecat, education)) %>% ungroup() %>%
    dplyr::select(year, agecat, sex, race, education, name, value) %>%
    pivot_wider(names_from=name, values_from=value)

    Diseases <- left_join(Diseases, death_counts_new, by=c("agecat","sex","race",
                                                           "education","year"))

  Diseases <- Diseases %>%
    rename(popcount = n)


  first <- paste0("mort_",diseases[1])
  last <- paste0(tail(names(Diseases), n=1))

  long_format <- Diseases %>% pivot_longer(all_of(first):all_of(last)) %>%
    mutate(type =
             case_when(grepl("mort", name) ~ "simulated",
                       grepl("yll", name) ~ "yll", .default="observed"),
           cause = gsub("yll_", "", name),
           cause = gsub("mort_", "", cause)) %>%
    dplyr::select(-name) %>%
    pivot_wider(names_from=type, values_from=value) %>%
    dplyr::select(year, sex,race, agecat, education, cause, popcount, simulated, observed, yll, max_risk) %>%
    rename(simulated_mortality_n = simulated, observed_mortality_n = observed) %>%
             mutate(sex = ifelse(sex=="f","Women","Men"))
  return(long_format)
}
