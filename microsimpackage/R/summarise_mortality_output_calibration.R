#' Compares mortality consumption by subgroup to brfss data
#'
#' @param
#' @keywords mortality postprocessing
#' @export
#' @examples
#' summarise_mortality_output_calibration
summarise_mortality_output_calibration <- function(Output, SelectedState, WorkingDirectory, inflation_factor, diseases){

Output <- do.call(rbind, Output)


age2010 <- Output %>% filter(year=="2010") %>%
  ungroup() %>%
  group_by(year, samplenum, sex, agecat, education) %>%
  summarise(totalpop = sum(popcount)) %>% ungroup() %>%
  group_by(year, sex, samplenum) %>%
  mutate(percent = totalpop / sum(totalpop)) %>% ungroup() %>% dplyr::select(sex, samplenum,
                                                                             agecat, education,
                                                                             percent)

summary_list <- list()
for (disease in diseases) {
  # Generate and add the summary to the list with automatic naming
  summary_list[[paste0(disease)]] <- Output %>%
    group_by(year, samplenum, sex, agecat, education) %>%
    left_join(.,age2010) %>%
    summarise(!!paste0("observed_", disease) := round(sum(!!sym(paste0(disease)))/inflation_factor,digits=2),
              !!paste0("simulated_",disease) := round(sum(!!sym(paste0("mort_", disease)))/inflation_factor,digits=2),
              !!paste0("weightedrate_observed",disease) := round((!!sym(paste0("observed_", disease))/popcount*100000)*percent,digits=2),
              !!paste0("weightedrate_simulated",disease) := round((!!sym(paste0("simulated_", disease))/popcount*100000)*percent,digits=2)) %>%
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
                                                "Some College","College+")),
         cause = recode(cause, "LVDC"="Liver cirrhosis",
                        "IJ"="Suicide","AUD"="Alcohol use disorder")
         ) %>%
  pivot_longer(first:last) %>%
  separate(name, into=c("type","cause"))


summary <- summary %>% pivot_wider(names_from=type, values_from=value)

scaleFUN <- function(x) sprintf("%.1f", x)




plot1 <- ggplot(data=subset(summary, sex=="Women"), aes(x=year, y=simulated, colour=as.factor(samplenum))) +
  geom_line(linewidth=1) +
  geom_line(aes(x=year, y=observed),colour="black", linewidth=2) +
  facet_grid(cols=vars(education),
             rows=vars(cause), scales="free",
             labeller = labeller(education = education.labs, cause = cause.labs)) +
  ylab("Age-st Mortality per 100,000") +
  xlab("") + theme_bw() + theme(legend.position = "none",
                                legend.title=element_blank(),
                                strip.background=element_rect(fill="white"),
                                text = element_text(size=18)) +
  ggtitle("Women") +
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA))
  # geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, colour=name, fill=name)) +
  # scale_colour_manual(values=c("#93aebf","#132268"))
plot1
plot2 <- ggplot(data=subset(summary, sex=="Men"), aes(x=year, y=simulated, colour=as.factor(samplenum))) +
  geom_line(linewidth=1) +
  geom_line(aes(x=year, y=observed),colour="black", linewidth=2) +
  facet_grid(cols=vars(education),
             rows=vars(cause), scales="free") +
  ylab("Age-st Mortality per 100,000") +
  xlab("") + theme_bw() + theme(legend.position = "none",
                                legend.title=element_blank(),
                                strip.background=element_rect(fill="white"),
                                text = element_text(size=18)) +
  ggtitle("Men") +
  # geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, colour=name, fill=name)) +
  # scale_colour_manual(values=c("#93aebf","#132268")) +
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA))
plot2
plot <- grid.arrange(plot1, plot2, ncol=2)

list <- list(summary, plot)

# find best fitting sample number for each cause
error <- summary %>%
  mutate(sqerror = abs(observed-simulated)^2) %>%
  ungroup() %>%
  group_by(samplenum) %>%
  summarise(rmse=sqrt(mean(sqerror))) %>%
  ungroup() %>%
  mutate(min = ifelse(rmse==min(rmse),1,0)) %>%
  filter(min==1)

lhs <- readRDS("SIMAH_workplace/microsim/2_output_data/lhs_samples.RDS")

for(i in 1:length(lhs)){
  lhs[[i]]$samplenum <- i
}

lhs <- do.call(rbind,lhs)

lhs <- lhs %>% filter(samplenum %in% unique(error$samplenum))

summary <- summary %>% filter(samplenum %in% unique(error$samplenum))


plot1 <- ggplot(data=subset(summary, sex=="Women"), aes(x=year, y=simulated, colour=as.factor(samplenum))) +
  geom_line(aes(x=year, y=observed),colour="black", linewidth=2, alpha=0.7) +
  geom_line(linewidth=1) +
  facet_grid(cols=vars(education),
             rows=vars(cause), scales="free") +
  ylab("Age-st Mortality per 100,000") +
  xlab("") + theme_bw() + theme(legend.position = "none",
                                legend.title=element_blank(),
                                strip.background=element_rect(fill="white"),
                                text = element_text(size=18)) +
  ggtitle("Women") +
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA))
# geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, colour=name, fill=name)) +
# scale_colour_manual(values=c("#93aebf","#132268"))
plot1
plot2 <- ggplot(data=subset(summary, sex=="Men"), aes(x=year, y=simulated, colour=as.factor(samplenum))) +
  geom_line(aes(x=year, y=observed),colour="black", linewidth=2, alpha=0.7) +
  geom_line(linewidth=1) +
  facet_grid(cols=vars(education),
             rows=vars(cause), scales="free") +
  ylab("Age-st Mortality per 100,000") +
  xlab("") + theme_bw() + theme(legend.position = "none",
                                legend.title=element_blank(),
                                strip.background=element_rect(fill="white"),
                                text = element_text(size=18)) +
  ggtitle("Men") +
  # geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, colour=name, fill=name)) +
  # scale_colour_manual(values=c("#93aebf","#132268")) +
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA))
plot2
plot <- grid.arrange(plot1, plot2, ncol=2)



return(list)
}

