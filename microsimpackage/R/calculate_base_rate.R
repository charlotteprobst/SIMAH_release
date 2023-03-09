#' Calculates baseline mortality rates for different diseases
#'
#' @param
#' @keywords calculate base rate
#' @export
#' @examples
#' base rates
calculate_base_rate <- function(data,base_rates,diseases){
  disease <- unique(diseases)
    rates <- data %>%
      group_by(cat) %>% add_tally() %>%
      summarise(sumrisk = sum(RR),
                .groups='drop') %>% ungroup() %>% distinct()
    rates <- left_join(rates, base_rates, by=c("cat"))
    rates <- rates %>%
      mutate(rate = !!as.name(paste0(disease))/sumrisk) %>%
      dplyr::select(cat, rate)
    return(rates)
}
