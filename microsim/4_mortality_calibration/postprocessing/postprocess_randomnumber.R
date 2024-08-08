# explore the impact of random number sampling + different education + alcohol models
OutputDirectory <- paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/mortality_calibration/random_number_sampling")

# which model
outputname <- "output-1"

# read from file if necessary
Output <- read_csv(paste0(OutputDirectory, "/", outputname, ".csv")) %>% 
  mutate(agecat_new = ifelse(agecat=="18-24", "18-24",
                             ifelse(agecat=="25-34" | agecat=="35-44", "25-44",
                                    ifelse(agecat=="45-54" | agecat=="55-64", "45-64", "65-79")))) %>% 
  group_by(year, sex, race, agecat_new, education, cause, seed, samplenum) %>% 
  summarise(simulated_mortality_n=sum(simulated_mortality_n),
            observed_mortality_n=sum(observed_mortality_n),
            popcount = sum(popcount),
            simulated_mortality_rate = (simulated_mortality_n / popcount) * 100000,
            observed_mortality_rate = (observed_mortality_n / popcount) *100000) %>% 
  group_by(samplenum) %>% 
  mutate(seed_recode = ntile(seed, 20))

meanbyseed <- function(data, selected_seed){
  mean <- data %>% filter(year>=2001) %>% 
    filter(seed_recode<=selected_seed) %>% 
    group_by(year, samplenum, sex,race, agecat_new, education, cause) %>% 
    summarise(mean = mean(simulated_mortality_rate)) %>% 
    mutate(nsamples=selected_seed)
  return(mean)
}

means <- list() 
for(i in 1:max(Output$seed_recode)){
  means[[i]] <- meanbyseed(Output,i)
}
means <- do.call(rbind,means)

ggplot(data=subset(means, year==2014 & cause=="UIJ"), aes(x=nsamples, y=mean, colour=agecat_new)) + 
  geom_line(linewidth=1) +
  facet_grid(cols=vars(sex, race), rows=vars(education), scales="free") + 
  theme_bw() + ylab("Mean mortality rate per 100,000") + 
  ggtitle("UIJ") + 
  xlab("N replications") + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        strip.background = element_rect(fill="white")) + 
  geom_vline(aes(xintercept=10), linetype="dashed")
ggsave(paste0(OutputDirectory, "/replications_LVDC.png"), dpi=300, width=33, height=19, units="cm")

