#' Compares mortality consumption by subgroup to brfss data
#'
#' @param
#' @keywords mortality postprocessing
#' @export
#' @examples
#' summarise_mortality_output_calibration
summarise_mortality_output_calibration <- function(Output, SelectedState, WorkingDirectory, inflation_factor, diseases){

age2010 <- Output %>% filter(year=="2010") %>%
  ungroup() %>%
  filter(samplenum==1) %>%
  group_by(year, sex, agecat, education) %>%
  summarise(totalpop = sum(popcount)) %>% ungroup() %>%
  group_by(year, sex, education) %>%
  mutate(percent = totalpop / sum(totalpop)) %>% ungroup() %>% dplyr::select(sex,
                                                                             agecat, education,
                                                                             percent)
summary_list <- list()
for (disease in diseases) {
  # Generate and add the summary to the list with automatic naming
  summary_list[[paste0(disease)]] <- Output %>%
    group_by(year, samplenum, sex, agecat, education) %>%
    left_join(.,age2010) %>%
    summarise(!!paste0("observed_", disease) := (sum(!!sym(paste0(disease)))/inflation_factor),
              !!paste0("simulated_",disease) := (sum(!!sym(paste0("mort_", disease)))/inflation_factor),
              !!paste0("weightedrate_observed",disease) := ((!!sym(paste0("observed_", disease))/popcount*100000)*percent),
              !!paste0("weightedrate_simulated",disease) := ((!!sym(paste0("simulated_", disease))/popcount*100000)*percent)) %>%
    ungroup() %>%
    group_by(year, samplenum, sex, education) %>%
    summarise(!!paste0("observed_",disease) := round(sum(!!sym(paste0("weightedrate_observed", disease))),digits=2),
              !!paste0("simulated_",disease) := round(sum(!!sym(paste0("weightedrate_simulated", disease))),digits=2))

}

summary <- Output %>% dplyr::select(year, samplenum, sex,education) %>% distinct()

# now join together to make a diseases dataframe for that year
for(disease in diseases){
  summary <-
    left_join(summary, summary_list[[paste0(disease)]], by=c("year","sex","education","samplenum"))
}

first <- paste0("observed_",diseases[1])
last <- paste0("simulated_", tail(diseases, n=1))

summary <- summary %>%
  mutate(sex = ifelse(sex=="f","Women","Men"),
         education = recode(education, "LEHS"="High school or less",
                            "SomeC"="Some College","College"="College+"),
         education = factor(education, levels=c("High school or less",
                                                "Some College","College+"))
         ) %>%
  pivot_longer(first:last) %>%
  separate(name, into=c("type","cause")) %>%
  mutate(         cause = recode(cause, "LVDC"="Liver cirrhosis",
                                 "IJ"="Suicide","AUD"="Alcohol use disorder"))


summary <- summary %>% pivot_wider(names_from=type, values_from=value)

scaleFUN <- function(x) sprintf("%.1f", x)

write.csv(summary, "SIMAH_workplace/microsim/2_output_data/SIMAH_calibration/full_calibration_summary.csv",
          row.names=F)

# find best fitting sample number for each cause
error <- summary %>%
  mutate(rangemin = 0,
        rangemax = observed*(1+0.1),
                    v_m = (0.1 * (rangemax- rangemin)),
        implausibility = ifelse(abs(simulated-observed)<=1, 0,
                                    abs(simulated-observed)/sqrt(v_m))) %>%
  ungroup() %>%
  group_by(samplenum) %>%
  summarise(implausibility=max(implausibility)) %>%
  slice(which.min(implausibility))


lhs <- read_csv("SIMAH_workplace/microsim/2_output_data/SIMAH_calibration/lhsSamples.csv") %>%
  dplyr::select(-`...1`) %>% filter(samplenum %in% unique(error$samplenum))

write.csv(lhs, "SIMAH_workplace/microsim/2_output_data/SIMAH_calibration/best_lhs.csv",
          row.names=F)

summary <- summary %>% filter(samplenum %in% unique(error$samplenum))
write.csv(summary, "SIMAH_workplace/microsim/2_output_data/SIMAH_calibration/best_calibration_summary.csv",
          row.names=F)

return(summary)
}

