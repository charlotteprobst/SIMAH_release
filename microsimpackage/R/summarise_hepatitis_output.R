#' Summarises microsimulation output for hepatitis B and C
#'
#' This function computes summary statistics for hepatitis and plots the data
#' @param
#' @keywords hepatitis
#' @export
#' @examples
#' summarise_hepatitis_output
summarise_hepatitis_output <- function(outputdata){
  Hepatitis <- outputdata %>% pivot_longer(prevalenceChronicB:prevalenceChronicC) %>%
    mutate(name = ifelse(grepl("B", name), "Chronic Hepatitis B", "Chronic Hepatitis C"),
           microsim.init.sex = ifelse(microsim.init.sex=="m","Men","Women"))
  scaleFUN <- function(x) sprintf("%.2f", x)
  plot <- ggplot(data=Hepatitis, aes(x=year, y=value)) + geom_line() +
    facet_grid(cols=vars(microsim.init.sex), rows=vars(name)) + theme_bw() +
    scale_y_continuous(labels=scaleFUN, limits=c(0,NA)) +
    ylab("Prevalence (%)")
  plot
  return(plot)
}
