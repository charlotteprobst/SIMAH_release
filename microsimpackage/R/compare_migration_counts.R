#' Compares population counts and adjusts migration counts for any given year
#'
#' @param
#' @keywords migration
#' @export
#' @examples
#' compare_migration_count
compare_migration_count <- function(Output, year, migration_counts){

  compare <- Output %>%
    mutate(agecat = cut(microsim.init.age, breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,80),
                        labels=c("18","19-24","25-29","30-34","35-39",
                                 "40-44","45-49","50-54","55-59",
                                 "60-64","65-69","70-74","75-79"))) %>%
    group_by(year,microsim.init.sex,microsim.init.race,agecat) %>%
    summarise(n=sum(n)) %>% mutate(data="microsim",
                                   year = as.numeric(as.character(year))) %>%
    rename(Year=year)

  # for comparison
  ACS <- read.csv("SIMAH_workplace/microsim/census_data/ACS_population_constraints.csv") %>%
    mutate(data = "ACS") %>% mutate(n=round(TotalPop*proportion)) %>%
    dplyr::select(-TotalPop)

  compare <- rbind(compare, ACS) %>% distinct()

  pct_diff <- compare %>%
    pivot_wider(names_from=data, values_from=n) %>%
    mutate(pct_diff = (microsim-ACS)/ACS) %>% filter(Year==year)


  ggplot(data=subset(compare,agecat=="25-29"), aes(x=Year, y=n, colour=data)) + geom_line() +
    facet_grid(cols=vars(microsim.init.race), rows=vars(microsim.init.sex))
  return(plot)
}
