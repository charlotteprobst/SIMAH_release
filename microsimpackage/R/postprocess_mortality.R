#' Calculates baseline mortality rates for different diseases
#'
#' @param
#' @keywords calculate base rate
#' @export
#' @examples
#' base rates
postprocess_mortality <- function(DiseaseSummary,diseases, death_rates){
  Diseases <- do.call(rbind, DiseaseSummary)
  death_rates <- death_rates %>% pivot_longer(LVDCmort:RESTmort) %>%
    separate(cat, into=c("sex","agecat","race","education"), sep=c(1,6,9,13)) %>%
    group_by(year, sex, agecat, education, name) %>%
    summarise(value=sum(value)) %>%
    mutate(name = gsub("mort", "", name)) %>%
    filter(name %in% diseases) %>%
    mutate(
      value = value*100, #inflate mortality rate by 100 for HLVDC
      education = ifelse(education=="Some", "SomeC",
                         ifelse(education=="Coll","College",education)),
      cat = paste0(sex, agecat, education)) %>% ungroup() %>%
    dplyr::select(year, cat, name, value) %>%
    pivot_wider(names_from=name, values_from=value)
  Diseases <- left_join(Diseases, death_rates)
  Diseases <- Diseases %>%
    separate(cat, into=c("sex","agecat","education"), sep=c(1,6,9)) %>%
    mutate(education = ifelse(education=="LEH", "LEHS",
                              ifelse(education=="Som","SomeC","College"))) %>%
    rename(popcount = n, simulated = mortHLVDC, observed = HLVDC)
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
