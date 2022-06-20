#' Summarises microsimulation output by education and compares to target data
#'
#' This function computes summary statistics for educational attainment with optional plot
#' @param
#' @keywords education
#' @export
#' @examples
#' summarise_education_output
summarise_education_output <- function(outputdata, SelectedState, WorkingDirectory){
  target <- read.csv(paste0(WorkingDirectory,"SIMAH_workplace/microsim/1_input_data/ACS_popcounts_unweighted_indage.csv")) %>%
    rename(microsim.init.sex=SEX,
           microsim.init.race=RACE,
           year=YEAR,
           microsim.init.education=EDUC) %>%
    mutate(microsim.init.sex = recode(microsim.init.sex, "M"="m","F"="f")) %>%
    group_by(STATE, year, microsim.init.sex, microsim.init.education) %>%
    summarise(n=sum(n)) %>% ungroup() %>%
    group_by(STATE, year, microsim.init.sex) %>%
    mutate(percent=n/sum(n)) %>% ungroup() %>%
    dplyr::select(-n) %>% filter(STATE==SelectedState) %>%
    dplyr::select(-STATE) %>% mutate(type="ACS/Census")
  summary <- outputdata %>%
    group_by(year, microsim.init.sex, microsim.init.education) %>%
    summarise(n=sum(n)) %>% ungroup() %>%
    group_by(year, microsim.init.sex) %>%
    mutate(percent=n/sum(n), type="Microsimulation",
           microsim.init.sex = as.character(microsim.init.sex),
           year = as.integer(as.character(year)),
           microsim.init.education = as.character(microsim.init.education)) %>%
    dplyr::select(-n)
  summary <- rbind(summary, target) %>%
    mutate(microsim.init.sex = ifelse(microsim.init.sex=="f","Women","Men"),
           microsim.init.education = recode(microsim.init.education,
                                            "LEHS"="High school degree or less",
                                            "SomeC"="Some college",
                                            "College"="College degree or more"),
           microsim.init.education = factor(microsim.init.education,
                                            levels=c("High school degree or less",
                                                     "Some college",
                                                     "College degree or more")))
  plot <- ggplot(data=summary, aes(x=year, y=percent, colour=type)) + geom_line(size=2) +
    facet_grid(cols=vars(microsim.init.education),
               rows=vars(microsim.init.sex)) +
    scale_y_continuous(labels=scales::percent) + ylab("Proportion (%)") +
    xlab("") + theme_bw() + theme(legend.position = "bottom",
                                  legend.title=element_blank(),
                                  strip.background=element_rect(fill="white"),
                                  text = element_text(size=18)) +
    scale_colour_manual(values=c("#93aebf","#132268"))

  list <- list(summary, plot)
  return(list)
}
