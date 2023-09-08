#' Compares mortality consumption by subgroup to brfss data
#'
#' @param
#' @keywords mortality postprocessing
#' @export
#' @examples
#' summarise_mortality_output_policy
summarise_mortality_output_policy <- function(Output, SelectedState, WorkingDirectory, inflation_factor, diseases){

summary_list <- list()
for (disease in diseases) {
  # Generate and add the summary to the list with automatic naming
  summary_list[[paste0(disease)]] <- Output %>%
    ungroup() %>%
    group_by(samplenum, year, sex, education) %>%
    # left_join(.,age2010) %>%
    summarise(!!paste0("observed_", disease) := (sum(!!sym(paste0(disease)))/inflation_factor),
              !!paste0("simulated_",disease) := (sum(!!sym(paste0("mort_", disease)))/inflation_factor))

}

summary <- Output %>% dplyr::select(samplenum,year,sex,education) %>%
  distinct()

# now join together to make a diseases dataframe for that year
for(disease in diseases){
  summary <-
    left_join(summary, summary_list[[paste0(disease)]], by=c("samplenum","year","sex","education"))
}

first <- paste0("observed_",diseases[1])
last <- paste0("simulated_", tail(diseases, n=1))

popcount <- Output %>%
  filter(samplenum==1) %>%
  group_by(year, sex, education) %>%
  summarise(popcount = sum(popcount)* (1/proportion))

summary <- left_join(summary, popcount)

summary <- summary %>%
  mutate(sex = ifelse(sex=="f","Women","Men"),
         education = factor(education, levels=c("LEHS","SomeC","College"))
  ) %>%
  pivot_longer(first:last) %>%
  separate(name, into=c("type","cause"))

summary$value <- summary$value * 1/proportion


summary <- summary %>%
  pivot_wider(names_from=type, values_from=value)

scaleFUN <- function(x) sprintf("%.1f", x)

tojoin <- data.frame(samplenum=1:length(percentreductions), percentreduction=percentreductions)
summary <- left_join(summary, tojoin)

# calculate a crude rate
summary$observed <- (summary$observed/summary$popcount)*100000
summary$simulated <- (summary$simulated/summary$popcount)*100000

summary <- summary %>% filter(samplenum==1 |
                                samplenum==2 |
                                samplenum==3 |
                                samplenum==4)
# summary <- summary %>%
#   pivot_wider(names_from=samplenum, values_from=simulated)

plot1 <- ggplot(data=subset(summary, sex=="Women"), aes(x=year, y=simulated, colour=as.factor(percentreduction))) +
  geom_line(size=1) +
  geom_line(aes(x=year, y=observed), colour="black") +
  facet_grid(cols=vars(education),
             rows=vars(cause), scales="free") +
  ylab("Age-st Mortality per 100,000") +
  xlab("") + theme_bw() + theme(legend.position = "bottom",
                                legend.title=element_blank(),
                                strip.background=element_rect(fill="white"),
                                text = element_text(size=18)) +
  ggtitle("Women") +
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

