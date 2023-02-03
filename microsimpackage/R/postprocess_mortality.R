#' Calculates baseline mortality rates for different diseases
#'
#' @param
#' @keywords calculate base rate
#' @export
#' @examples
#' base rates
postprocess_mortality <- function(DiseaseSummary,diseases, death_counts, inflation_factor){
  disease <- unique(diseases)
  Diseases <- do.call(rbind, DiseaseSummary)
  death_counts <- death_counts %>% pivot_longer(LVDCmort:RESTmort) %>%
    separate(cat, into=c("sex","agecat","race","education"), sep=c(1,6,9,13)) %>%
    mutate(agecat = ifelse(agecat=="25-29" | agecat=="30-34","25-34",
                           ifelse(agecat=="35-39" | agecat=="40-44","35-44",
                                  ifelse(agecat=="45-49" | agecat=="50-54", "45-54",
                                         ifelse(agecat=="55-59" | agecat=="60-64", "55-64",
                                                ifelse(agecat=="65-69" | agecat=="70-74", "65-74",
                                                       agecat)))))) %>%
    group_by(year, sex, agecat, education, name) %>%
    summarise(value=sum(value)) %>%
    mutate(name = gsub("mort", "", name)) %>%
    filter(name %in% diseases) %>%
    mutate(
      value = value*inflation_factor, #inflate mortality rate by 100 for HLVDC
      education = ifelse(education=="Some", "SomeC",
                         ifelse(education=="Coll","College",education)),
      cat = paste0(sex, agecat, education)) %>% ungroup() %>%
    dplyr::select(year, cat, name, value) %>%
    pivot_wider(names_from=name, values_from=value)
  Diseases <- left_join(Diseases, death_counts)
  Diseases <- Diseases %>%
    separate(cat, into=c("sex","agecat","education"), sep=c(1,6,9)) %>%
    mutate(education = ifelse(education=="LEH", "LEHS",
                              ifelse(education=="Som","SomeC","College"))) %>%
    rename(popcount = n, simulated = !!as.name(paste0('mort',quo_name(disease))), observed = !!as.name(paste0(quo_name(disease))))
  return(Diseases)


  # Summary <- do.call(rbind, DeathSummary) %>%
  #   mutate(agecat = as.factor(agecat),
  #          microsim.init.sex=as.factor(microsim.init.sex),
  #          microsim.init.race=as.factor(microsim.init.race),
  #          microsim.init.education = as.factor(microsim.init.education),
  #          year = as.factor(year),
  #          cause=as.factor(cause)) %>%
  #   group_by(year, agecat, microsim.init.sex, microsim.init.race, microsim.init.education,
  #            cause, .drop=FALSE) %>% tally(name="ndeaths")
  #
  # PopSummary <- do.call(rbind,PopPerYear) %>%
  #   mutate(agecat = cut(microsim.init.age,
  #                       breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,100),
  #                       labels=c("18-24","25-29","30-34","35-39",
  #                                "40-44","45-49","50-54","55-59","60-64","65-69",
  #                                "70-74","75-79")),
  #          microsim.init.sex=as.factor(microsim.init.sex),
  #          microsim.init.race=as.factor(microsim.init.race),
  #          microsim.init.education = as.factor(microsim.init.education),
  #          year = as.factor(year)) %>%
  #   group_by(year, agecat, microsim.init.sex, microsim.init.race, microsim.init.education, .drop=FALSE) %>%
  #   tally(name="totalpop")
  # Summary <- list(Summary,PopSummary)
  # return(Summary)
}
