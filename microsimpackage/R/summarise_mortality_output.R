#' Compares mortality consumption by subgroup to brfss data
#'
#' @param
#' @keywords mortality postprocessing
#' @export
#' @examples
#' summarise_mortality_output
summarise_mortality_output <- function(Output, SelectedState, WorkingDirectory, inflation_factor, diseases){

age2010 <- Output %>% filter(year=="2010") %>%
  filter(samplenum==1) %>%
  ungroup() %>%
  group_by(year, sex, agecat, education) %>%
  summarise(totalpop = sum(popcount)) %>% ungroup() %>%
  group_by(year, sex) %>%
  mutate(percent = totalpop / sum(totalpop)) %>% ungroup() %>% dplyr::select(sex,
                                                                             agecat, education,
                                                                             percent)
summary_list <- list()
for (disease in diseases) {
  # Generate and add the summary to the list with automatic naming
  summary_list[[paste0(disease)]] <- Output %>%
    group_by(samplenum, year, sex, agecat, education) %>%
    left_join(.,age2010) %>%
    summarise(!!paste0("observed_", disease) := ifelse(agecat=="65-74" | agecat=="75-79", round(sum(!!sym(paste0(disease)))), round(sum(!!sym(paste0(disease)))/inflation_factor,digits=2)),
              !!paste0("simulated_",disease) := ifelse(agecat=="65-74" | agecat=="75-79", round(sum(!!sym(paste0("mort_", disease)))), round(sum(!!sym(paste0("mort_", disease)))/inflation_factor,digits=2)),
              !!paste0("weightedrate_observed",disease) := round((!!sym(paste0("observed_", disease))/popcount*100000)*percent,digits=2),
              !!paste0("weightedrate_simulated",disease) := round((!!sym(paste0("simulated_", disease))/popcount*100000)*percent,digits=2)) %>%
    ungroup() %>%
    group_by(samplenum, year, sex, education) %>%
    summarise(!!paste0("observed_",disease) := round(sum(!!sym(paste0("weightedrate_observed", disease))),digits=2),
              !!paste0("simulated_",disease) := round(sum(!!sym(paste0("weightedrate_simulated", disease))),digits=2))

}

summary <- Output %>% dplyr::select(samplenum,year,sex,education) %>% distinct()

# now join together to make a diseases dataframe for that year
for(disease in diseases){
  summary <-
    left_join(summary, summary_list[[paste0(disease)]], by=c("samplenum","year","sex","education"))
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

plot1 <- ggplot(data=subset(summary, sex=="Women"), aes(x=year, y=value, colour=type)) + geom_line(size=2) +
  facet_grid(cols=vars(education),
             rows=vars(cause), scales="free") +
  ylab("Age-st Mortality per 100,000") +
  xlab("") + theme_bw() + theme(legend.position = "bottom",
                                legend.title=element_blank(),
                                strip.background=element_rect(fill="white"),
                                text = element_text(size=18)) +
  ggtitle("Women") +
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA)) +
  # geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, colour=name, fill=name)) +
  scale_colour_manual(values=c("#93aebf","#132268"))
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


