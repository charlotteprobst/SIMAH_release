#' Compares mortality consumption by subgroup to brfss data
#'
#' @param
#' @keywords mortality postprocessing
#' @export
#' @examples
#' summarise_mortality_output
summarise_mortality_output <- function(Output, SelectedState, WorkingDirectory){



plot <- ggplot(data=summary, aes(x=year, y=value, colour=name)) + geom_line(size=2) +
  facet_grid(cols=vars(AlcCAT),
             rows=vars(sex), scales="free") +
  scale_y_continuous(labels=scales::percent, limits=c(0,0.7)) + ylab("Proportion (%)") +
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

