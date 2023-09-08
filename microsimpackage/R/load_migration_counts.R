#' Loads migration count data for SIMAH or CASCADE versions
#'
#' @param
#' @keywords migration counts
#' @export
#' @examples
#' load_migration_counts
load_migration_counts <- function(SelectedState, DataDirectory){
  Counts <- readRDS(paste0(DataDirectory,"/final_rates",SelectedState,".RDS"))
  Counts$agecat <- as.character(Counts$agecat)

  datatopredict <- expand.grid(Year=c(2019:2025),microsim.init.sex=unique(Counts$microsim.init.sex),
                               agecat = unique(Counts$agecat), microsim.init.race=unique(Counts$microsim.init.race)) %>%
    mutate(MigrationInN = NA,
           MigrationOutN = NA)
  Counts <- rbind(Counts,datatopredict)

  Counts <- Counts %>% group_by(microsim.init.sex, agecat, microsim.init.race) %>%
    fill(c(MigrationInN,MigrationOutN), .direction=c("downup"))
  return(Counts)
}
