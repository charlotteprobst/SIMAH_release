#' Compares mortality consumption by subgroup to brfss data
#'
#' @param
#' @keywords mortality postprocessing
#' @export
#' @examples
#' summarise_mortality_output
summarise_mortality_output <- function(Output, SelectedState, WorkingDirectory){

age2010 <- Output %>% filter(year=="2019") %>%
  ungroup() %>%
  group_by(year, sex, education, agecat) %>%
  summarise(totalpop = sum(popcount)) %>% ungroup() %>%
  group_by(year, sex, education) %>%
  mutate(percent = totalpop / sum(totalpop)) %>% ungroup() %>% dplyr::select(sex, education,
                                                                             agecat, percent)
simulation <- left_join(Output, age2010) %>%
  group_by(year, sex, education, agecat) %>%
  mutate(simulated = simulated / 100,
         observed = observed / 100,
         weightedrate_simulation = (simulated / popcount*100000)*percent,
         weightedrate_observed = (observed / popcount*100000)*percent) %>%
  ungroup() %>%
  group_by(year, sex, education) %>%
  summarise(simulated=sum(simulated),
            observed=sum(observed)) %>%
  pivot_longer(cols=simulated:observed) %>%
  mutate(sex = ifelse(sex=="f","Women","Men"),
         education = factor(education, levels=c("LEHS","SomeC","College")))

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

