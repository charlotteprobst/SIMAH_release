#' Calculates baseline mortality rates for different diseases
#'
#' @param
#' @keywords calculate base rate
#' @export
#' @examples
#' base rates
calculate_base_rate <- function(data,base_counts,diseases){
  rates <- list()
  for(i in unique(diseases)){
    rates[[paste(i)]] <- data %>%
      group_by(cat) %>% add_tally() %>%
      summarise(
        !!paste0("sumrisk_", i):= sum(!!as.name(paste0("RR_", i))),
                .groups='drop') %>% ungroup() %>% distinct()
    rates[[paste(i)]] <- left_join(rates[[paste(i)]], base_counts, by=c("cat"))
    rates[[paste(i)]] <- rates[[paste(i)]] %>%
      mutate(!!paste0("rate_", i):= !!as.name(paste0(i))/!!as.name(paste0("sumrisk_", i)))

  }
  names(rates) <- NULL
  base_rates <- do.call(cbind, rates) %>%
    dplyr::select(any_of(paste0("rate_",diseases)))

  base_rates$cat <- rates[[1]]$cat

  return(base_rates)
}
