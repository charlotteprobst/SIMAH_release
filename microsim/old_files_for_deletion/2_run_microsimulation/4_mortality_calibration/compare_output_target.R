# where to save the outputs 
OutputDirectory <- paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/mortality_calibration/version2")

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
            popcount = sum(popcount))

Output$agecat <- Output$agecat_new

data <- calculate_implausibility_mortality(Output, agest=0, agestyear=2010, 0.05)
  
processed_data <- data[[2]]

implausibility <- data[[1]]

processed_data <- processed_data %>% 
  pivot_longer(simulated_mortality_rate:observed_mortality_rate)
  
ggplot(subset(test, education=="LEHS" & samplenum==62), aes(x=year, y=agest_simulated_mortality_rate, 
                                                      colour=as.factor(seed))) + 
  geom_line() + 
  geom_line(aes(x=year, y=agest_observed_mortality_rate), colour="black", linewidth=1) + 
  facet_grid(cols=vars(cause), rows=vars(sex), scales="free") +
  theme_bw() + 
  theme(legend.position="none") + ggtitle("education = LEHS") + ylim(0,NA)
ggsave(paste0(OutputDirectory, "/plot_LEHS_best.png"), dpi=300, width=33, height=19, units="cm")
