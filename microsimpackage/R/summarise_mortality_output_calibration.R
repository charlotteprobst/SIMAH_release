#' Compares mortality consumption by subgroup to brfss data
#'
#' @param
#' @keywords mortality postprocessing
#' @export
#' @examples
#' summarise_mortality_output
summarise_mortality_output_calibration <- function(Output, SelectedState, inflation_factor, agestyear){
Output <- do.call(rbind,Output)

age2010 <- Output %>% filter(year==agestyear) %>%
  ungroup() %>% filter(seed==1 & samplenum==1) %>%
  group_by(sex, agecat, education) %>%
  summarise(totalpop = sum(popcount)) %>% ungroup() %>%
  group_by(sex) %>%
  mutate(percent = totalpop / sum(totalpop)) %>% ungroup() %>% dplyr::select(sex,
                                                                             agecat, education,
                                                                             percent)
simulation <- left_join(Output, age2010) %>%
  group_by(seed, samplenum, year, sex, agecat, education) %>%
  mutate(
    simulated = simulated / inflation_factor,
         observed = observed / inflation_factor,
         rate_simulation = (simulated / popcount) * 100000,
         rate_observed = (observed / popcount) * 100000,
         weightedrate_simulation = rate_simulation*percent,
         weightedrate_observed = rate_observed*percent) %>%
  ungroup() %>%
  group_by(seed, samplenum, year, sex, education) %>%
  summarise(simulated=sum(simulated),
            observed=sum(observed)) %>%
  mutate(sex = ifelse(sex=="f","Women","Men"),
         education = factor(education, levels=c("LEHS","SomeC","College"))
         )

return(simulation)
}

