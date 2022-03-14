# script to compare data by rates and plot results
Cirrhosis <- readRDS("SIMAH_workplace/microsim/2_output_data/calibration_output/Cirrhosis_output_wave1.RDS")

simulation_output <- Cirrhosis %>% do.call(rbind,.) %>% 
  dplyr::select(samplenum, seed, year, microsim.init.sex, agegroup, cirrhosistotal) %>% 
  mutate(cirrhosistotal = ifelse(is.na(cirrhosistotal),0,cirrhosistotal)) %>% 
  # mutate(rateper100000 = ifelse(is.na(rateper100000),0,rateper100000)) %>% 
  rename(sex=microsim.init.sex) %>% group_by(samplenum, year, sex, agegroup) %>% 
  summarise(cirrhosistotal=mean(cirrhosistotal))

cirrhosismortality <- cirrhosismortality %>% 
  dplyr::select(Year, sex, agegroup, count,Population) %>% 
  mutate(count = count/100) %>% 
  rename(year=Year)

simulation_output <- left_join(simulation_output, cirrhosismortality) %>% 
  rename(simulation=cirrhosistotal, target=count) %>% 
  pivot_longer(cols=simulation:target) %>% 
  mutate(rate = (value/Population)*100000,
         sex=ifelse(sex=="f","Women","Men"))

ggplot(data=simulation_output, aes(x=year, y=rate, colour=name, linetype=as.factor(samplenum))) + 
  geom_line(size=1) + facet_grid(cols=vars(agegroup),rows=vars(sex)) + theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(fill="white"),
        text=element_text(size=18)) +
  ylab("Mortality rate per 100,000 population") +
  scale_linetype_discrete(guide="none") + 
  scale_colour_manual(values=c("red","black"))

ggsave("SIMAH_workplace/microsim/2_output_data/calibration_output/mortality_rate_plot_calibration.png",
       dpi=300, width=33, height=19, units="cm")


# now age standardise 
age2010 <- cirrhosismortality %>% filter(year==2010) %>% group_by(agegroup) %>% 
  summarise(n=sum(Population)) %>% ungroup() %>% 
  mutate(percent=n/sum(n))

# join summary data with weights 
simulation_output <- left_join(simulation_output,age2010)
simulation_output$rate <- ifelse(is.na(simulation_output$rate), 0, simulation_output$rate)
simulation_output$weighted_rate <- simulation_output$rate*simulation_output$percent

age_standardised <- simulation_output %>% 
  group_by(samplenum, year, sex, name) %>% 
  summarise(rate = sum(weighted_rate))

ggplot(data=age_standardised, aes(x=year, y=rate, colour=name, linetype=as.factor(samplenum))) + 
  geom_line(size=1) + facet_grid(rows=vars(sex)) + theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(fill="white"),
        text = element_text(size=18)) +
  ylab("Age-standardised mortality rate per 100,000 population") + ylim(0,NA) +
  scale_linetype_discrete(guide="none") +
  scale_colour_manual(values=c("red","black"))

ggsave("SIMAH_workplace/microsim/2_output_data/calibration_output/mortality_standardised_rate_plot_calibration.png",
       dpi=300, width=33, height=19, units="cm")

# work out best fit 
fit <- age_standardised %>% pivot_wider(names_from=name, values_from=rate) %>% 
  mutate(error=abs(simulation-target)) %>% 
  group_by(samplenum) %>% summarise(max=max(error))

topsample <- subset(fit, max==min(fit$max))$samplenum

ggplot(data=subset(age_standardised,samplenum==topsample), aes(x=year, y=rate, colour=name, linetype=as.factor(samplenum))) + 
  geom_line(size=1) + facet_grid(rows=vars(sex)) + theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(fill="white"),
        text = element_text(size=18)) +
  ylab("Age-standardised mortality rate per 100,000 population") + ylim(0,NA) +
  scale_linetype_discrete(guide="none") +
  scale_colour_manual(values=c("red","black"))

ggsave("SIMAH_workplace/microsim/2_output_data/calibration_output/mortality_standardised_rate_plot_bestfit.png",
       dpi=300, width=33, height=19, units="cm")

ggplot(data=subset(simulation_output,samplenum==topsample), aes(x=year, y=rate, colour=name, linetype=as.factor(samplenum))) + 
  geom_line(size=1) + facet_grid(cols=vars(agegroup),rows=vars(sex)) + theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(fill="white"),
        text=element_text(size=18)) +
  ylab("Mortality rate per 100,000 population") +
  scale_linetype_discrete(guide="none") + 
  scale_colour_manual(values=c("red","black"))

ggsave("SIMAH_workplace/microsim/2_output_data/calibration_output/mortality_rate_plot_topsample.png",
       dpi=300, width=33, height=19, units="cm")


