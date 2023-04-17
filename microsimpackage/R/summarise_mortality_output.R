#' Compares mortality consumption by subgroup to brfss data
#'
#' @param
#' @keywords mortality postprocessing
#' @export
#' @examples
#' summarise_mortality_output
summarise_mortality_output <- function(Output, SelectedState, WorkingDirectory, inflation_factor){

age2010 <- Output %>% filter(year=="2010") %>%
  ungroup() %>%
  group_by(year, sex, agecat, education) %>%
  summarise(totalpop = sum(popcount)) %>% ungroup() %>%
  group_by(year, sex) %>%
  mutate(percent = totalpop / sum(totalpop)) %>% ungroup() %>% dplyr::select(sex,
                                                                             agecat, education,
                                                                             percent)
simulation <- left_join(Output, age2010) %>%
  group_by(year, sex, agecat, education) %>%
  mutate(simulated = simulated / inflation_factor,
         observed = observed / inflation_factor,
         weightedrate_simulation = (simulated / popcount*100000)*percent,
         weightedrate_observed = (observed / popcount*100000)*percent) %>%
  ungroup() %>%
  group_by(year, sex, education) %>%
  summarise(simulated=sum(simulated),
            observed=sum(observed)) %>%
  pivot_longer(cols=simulated:observed) %>%
  mutate(sex = ifelse(sex=="f","Women","Men"),
         education = factor(education, levels=c("LEHS","SomeC","College"))
         )

plot <- ggplot(data=simulation, aes(x=year, y=value, colour=name)) + geom_line(size=2) +
  facet_grid(cols=vars(education),
             rows=vars(sex), scales="free") +
  ylab("Age-st Mortality per 100,000") +
  xlab("") + theme_bw() + theme(legend.position = "bottom",
                                legend.title=element_blank(),
                                strip.background=element_rect(fill="white"),
                                text = element_text(size=18)) +
  # geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, colour=name, fill=name)) +
  scale_colour_manual(values=c("#93aebf","#132268"))
plot
list <- list(simulation, plot)
return(list)
}

