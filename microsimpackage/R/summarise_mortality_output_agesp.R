#' Compares mortality consumption by subgroup to brfss data
#'
#' @param
#' @keywords mortality postprocessing
#' @export
#' @examples
#' summarise_mortality_output_agest
summarise_mortality_output_agest <- function(Output, SelectedState, WorkingDirectory, inflation_factor, diseases){

summary_list <- list()
for (disease in diseases) {
  # Generate and add the summary to the list with automatic naming
  summary_list[[paste0(disease)]] <- Output %>%
    group_by(samplenum, year, sex, agecat, education) %>%
    # left_join(.,age2010) %>%
    summarise(!!paste0("observed_", disease) := round(sum(!!sym(paste0(disease))),digits=2),
              !!paste0("simulated_",disease) := round(sum(!!sym(paste0("mort_", disease))),digits=2),
              popcount=sum(popcount)) %>% distinct() %>% ungroup() %>%
    mutate(agecat_new = ifelse(agecat=="18-24" | agecat=="25-34", "18-34",
                               ifelse(agecat=="35-44" | agecat=="45-54" | agecat=="55-64", "35-64",
                                      "65-79"))) %>%
    ungroup() %>% group_by(samplenum, year, sex, agecat_new, education) %>%
              summarise(!!paste0("observed_",disease) := sum(!!sym(paste0("observed_", disease))),
                        !!paste0("simulated_",disease) := sum(!!sym(paste0("simulated_", disease))),
                        popcount = sum(popcount),
                        !!paste0("observed_",disease) := round((!!sym(paste0("observed_", disease))/popcount*100000),digits=2),
                        !!paste0("simulated_",disease) := round((!!sym(paste0("simulated_", disease))/popcount*100000),digits=2)
                        ) %>%

    ungroup() %>% dplyr::select(-popcount)
  # %>%
    # group_by(samplenum, year, sex, education) %>%
    # summarise(!!paste0("observed_",disease) := round(sum(!!sym(paste0("weightedrate_observed", disease))),digits=2),
    #           !!paste0("simulated_",disease) := round(sum(!!sym(paste0("weightedrate_simulated", disease))),digits=2))

}



summary <- Output %>%
  mutate(agecat_new = ifelse(agecat=="18-24" | agecat=="25-34", "18-34",
                             ifelse(agecat=="35-44" | agecat=="45-54" | agecat=="55-64", "35-64",
                                    "65-79"))) %>%
  dplyr::select(samplenum,year,agecat_new, sex,education) %>% distinct()

# now join together to make a diseases dataframe for that year
for(disease in diseases){
  summary <-
    left_join(summary, summary_list[[paste0(disease)]], by=c("samplenum","year","sex","agecat_new","education"))
}

first <- paste0("observed_",diseases[1])
last <- paste0("simulated_", tail(diseases, n=1))

summary <- summary %>%
  mutate(sex = ifelse(sex=="f","Women","Men"),
         education = factor(education, levels=c("LEHS","SomeC","College"))
         ) %>%
  pivot_longer(first:last) %>%
  separate(name, into=c("type","cause"))

scaleFUN <- function(x) sprintf("%.1f", x)

plot1 <- ggplot(data=subset(summary, cause=="DM"),
                aes(x=year, y=value, colour=sex, linetype=type)) + geom_line(size=2) +
  facet_grid(cols=vars(education),
             rows=vars(agecat_new), scales="free") +
  ylab("Age-st Mortality per 100,000") +
  xlab("") + theme_bw() + theme(legend.position = "bottom",
                                legend.title=element_blank(),
                                strip.background=element_rect(fill="white"),
                                text = element_text(size=18)) +
  ggtitle("DM") +
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA))
  # geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, colour=name, fill=name)) +
  # scale_colour_manual(values=c("#93aebf","#132268"))
plot1
plot2 <- ggplot(data=subset(summary, sex=="Men"), aes(x=year, y=value, colour=type)) + geom_line(size=2) +
  facet_grid(cols=vars(education),
             rows=vars(cause), scales="free") +
  ylab("Age-st Mortality per 100,000") +
  xlab("") + theme_bw() + theme(legend.position = "bottom",
                                legend.title=element_blank(),
                                strip.background=element_rect(fill="white"),
                                text = element_text(size=18)) +
  ggtitle("Men") +
  # geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, colour=name, fill=name)) +
  scale_colour_manual(values=c("#93aebf","#132268")) +
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA))
plot <- grid.arrange(plot1, plot2, ncol=2)

list <- list(summary, plot)
return(list)
}

