#' Compares alcohol consumption by subgroup to brfss data
#'
#' @param
#' @keywords alcohol postprocessing
#' @export
#' @examples
#' summarise_alcohol_output
summarise_alcohol_output_continuous <- function(Output, SelectedState, WorkingDirectory){
targetdata <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_1984_2020_final.RDS") %>%
  filter(State==SelectedState) %>% filter(YEAR>=2000) %>%
  filter(drinkingstatus==1) %>% group_by(YEAR, sex_recode) %>%
  summarise(meangpd = mean(gramsperday)) %>%
  rename(microsim.init.sex = sex_recode, year = YEAR) %>%
  mutate(microsim.init.sex = ifelse(microsim.init.sex=="Male","Men","Women"),
         type = "BRFSS")

output <- Output %>%
  mutate(microsim.init.sex= ifelse(microsim.init.sex=="m","Men","Women"),
         type = "Microsimulation",
         year = as.numeric(as.character(year)))


summary <- rbind(output, targetdata)

plot <- ggplot(data=summary, aes(x=year, y=meangpd, colour=type)) + geom_line(size=2) +
  facet_grid(cols=vars(microsim.init.sex), scales="free") + ylab("Proportion (%)") +
  xlab("") + theme_bw() + theme(legend.position = "bottom",
                                legend.title=element_blank(),
                                strip.background=element_rect(fill="white"),
                                text = element_text(size=18)) +
  # geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, colour=name, fill=name)) +
  scale_colour_manual(values=c("#93aebf","#132268"))
plot
list <- list(summary, plot)
return(list)
}

