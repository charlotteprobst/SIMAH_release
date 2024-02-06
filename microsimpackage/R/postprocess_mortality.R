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
    separate(cat, into=c("sex","agecat","race","education"), sep=c(1,6,9,13)) %>%
    mutate(agecat = ifelse(agecat=="25-29" | agecat=="30-34","25-34",
                           ifelse(agecat=="35-39" | agecat=="40-44","35-44",
                                  ifelse(agecat=="45-49" | agecat=="50-54", "45-54",
                                         ifelse(agecat=="55-59" | agecat=="60-64", "55-64",
                                                ifelse(agecat=="65-69" | agecat=="70-74", "65-74",
                                                       agecat)))))) %>%
    group_by(year, sex, agecat, race, education, name) %>%
    summarise(value=sum(value)) %>%
    mutate(name = gsub("mort", "", name)) %>%
    filter(name %in% diseases) %>%
    mutate(
      education = ifelse(education=="Some", "SomeC",
                         ifelse(education=="Coll","College",education)),
      cat = paste0(sex, agecat, education)) %>% ungroup() %>%
    dplyr::select(year, agecat, sex, race, education, name, value) %>%
    rename(microsim.init.sex=sex, microsim.init.race=race, microsim.init.education=education) %>%
    pivot_wider(names_from=name, values_from=value)

    Diseases <- left_join(Diseases, death_counts_new, by=c("agecat","microsim.init.sex","microsim.init.race",
                                                           "microsim.init.education","year"))

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
    dplyr::select(year, microsim.init.sex,microsim.init.race, agecat, microsim.init.education, cause, popcount, simulated, observed, yll) %>%
    rename(simulated_mortality_n = simulated, observed_mortality_n = observed,
           sex = microsim.init.sex, race=microsim.init.race, education=microsim.init.education) %>%
    mutate(sex = ifelse(sex=="f","Women","Men"),
           race = ifelse(race=="WHI","White",
                         ifelse(race=="BLA","Black",
                                ifelse(race=="SPA","Hispanic",
                                       ifelse(race=="OTH","Others", NA)))))
  return(long_format)
}
