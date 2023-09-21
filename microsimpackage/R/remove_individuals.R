#' Removes (uninflated) individuals from simulation
#'
#' @param
#' @keywords mortality remove
#' @export
#' @examples
#' remove_individuals
remove_individuals <- function(data = basepop, disease, age_inflated, inflation_factors){
# first filter on those who were staged to die from that disease

  # and calculate N to be removed from the simulation by category (de-inflated by age category)
  toremove <- data %>%
    filter(!!sym(paste0('mort_', disease)) == 1) %>%
    group_by(cat) %>%
    add_tally() %>%
    mutate(ageCAT = cut(microsim.init.age,
                        breaks=c(0,24,34,44,54,64,74,79),
                        labels=c("18-24","25-34","35-44", "45-54",
                                 "55-64","65-74","75-79")),
           inflation_factor = ifelse(ageCAT %in% age_inflated[[1]], inflation_factors[1],
                                     ifelse(ageCAT %in% age_inflated[[2]], inflation_factors[2], NA)),
           toremove = round(n / inflation_factor))

  # function to remove individuals based on RR and category
  removingfunction <- function(toremove, disease){
    # extract N to remove
    N <- unique(toremove$toremove)
    RRs <- toremove %>% ungroup() %>% dplyr::select(!!sym(paste0('RR_', disease)))
    RRs <- as.numeric(unlist(RRs))
    # sample the individuals to remove - using stochastic universal sampling
    samples <- sus(RRs, N)
    toremove <- toremove[samples,]
    return(toremove)
  }

  # now apply that function to each category
  removed <- toremove %>%
    ungroup() %>%
    filter(toremove>=1) %>%
    group_by(cat) %>%
    do(removingfunction(., disease=disease))

    # conditional on the original risk
  # Get the IDs to be removed
  ids <- removed$microsim.init.id
  # Remove the individuals with the specified IDs
  data <- data %>%
    filter(!microsim.init.id %in% ids)

  # Remove the unnecessary columns for the current disease
  data <- data %>%
    dplyr::select(-c(!!sym(paste0('mort_', disease)),
                     !!sym(paste0("RR_", disease)),
                     !!sym(paste0("risk_", disease)),
                     !!sym(paste0("rate_", disease))))
  return(data)
}

