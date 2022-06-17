#' Loads migration rate data for SIMAH or CASCADE versions
#'
#' @param
#' @keywords migration rates
#' @export
#' @examples
#' load_migration_rates
load_migration_rates <- function(SelectedState, WorkingDirectory){
  Rates <- readRDS(paste0(WorkingDirectory,"SIMAH_workplace/microsim/1_input_data/migration_rates/final_rates",SelectedState,".RDS"))
  Rates$agecat <- as.character(Rates$agecat)

  datatopredict <- expand.grid(Year=c(2019:2025),microsim.init.sex=unique(Rates$microsim.init.sex),
                               agecat = unique(Rates$agecat), microsim.init.race=unique(Rates$microsim.init.race)) %>%
    mutate(MigrationInN = NA,
           MigrationOutN = NA)
  Rates <- rbind(Rates,datatopredict)

  Rates <- Rates %>% group_by(microsim.init.sex, agecat, microsim.init.race) %>%
    fill(c(MigrationInN,MigrationOutN), .direction=c("downup"))
  return(Rates)
}
