# where to save the outputs 
OutputDirectory <- paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/ordinal_calibration")

# which model
outputname <- "output_determ_twint"

# process output from simulation and save
Output <- do.call(rbind, CatSummary) %>% mutate(samplenum=1)
write.csv(Output, paste0(OutputDirectory, "/", outputname, ".csv"), row.names=F)
max(Output$implausibility, na.rm=T)
mean(Output$implausibility, na.rm=T)

# baseline model implausibility = 18, sex education interaction = 16, sex age interaction = 18

# read from file if necessary
Output <- read_csv(paste0(OutputDirectory, "/", outputname, ".csv")) %>% 
  mutate(lower = proptarget - 1.96*se, upper = proptarget + 1.96*se) %>% 
  group_by(year, samplenum, microsim.init.sex, microsim.init.race, agecat, 
           microsim.init.education, AlcCAT) %>% 
  summarise(propsimulation=mean(propsimulation),
         proptarget=mean(proptarget),
         lower=mean(lower),
         upper=mean(upper),
         implausibility=mean(implausibility))

scaleFUN <- function(x) sprintf("%.2f", x)

Output$microsim.init.education <- factor(Output$microsim.init.education,
                                         levels=c("LEHS","SomeC","College"))

for(i in unique(Output$AlcCAT)){
  for(j in unique(Output$microsim.init.race)){
    race <- ifelse(j=="WHI","White",
                   ifelse(j=="BLA","Black",
                          ifelse(j=="SPA", "Hispanic","Others")))
ggplot(subset(Output, AlcCAT==i & microsim.init.race==j), aes(x=year, y=propsimulation, colour=as.factor(samplenum))) + 
  geom_line(linewidth=1) + 
  # geom_point() +
  geom_line(aes(x=year, y=proptarget), colour="black", linewidth=1) +
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey", colour=NA, alpha=0.6) + 
  theme_bw() + 
    # ylim(0,NA)+ 
  theme(legend.position="none",
        legend.title=element_blank()) + 
  scale_y_continuous(limits=c(0,NA), labels=scaleFUN) + 
  scale_x_continuous(breaks=c(2000,2004,2008,2012,2016,2020)) + 
  # ylim(0,NA) +
  facet_grid(cols=vars(microsim.init.sex, agecat), rows=vars(microsim.init.education)) +
  ggtitle(paste0("Deterministic method", i, "-", race))
ggsave(paste0(OutputDirectory, "/plots/newint",outputname, i, j, ".png"), dpi=300, width=33, height=19, units='cm')
  }
}

regression <- read_csv(paste0(OutputDirectory, "/lhs_regression-4.csv")) %>% 
  group_by(name) %>% summarise(mean = mean(Value), se =std.error(Value),
                               lower = mean - (1.96*se), upper = mean + (1.96*se),
                               PE = exp(mean), lowerCI = exp(lower), upperCI=exp(upper))

test <- Output %>% group_by(samplenum) %>% 
  filter(microsim.init.race!="OTH") %>% 
  summarise(mean = mean(implausibility,na.rm=T), max=max(implausibility, na.rm=T))

# Pop <- do.call(rbind, PopPerYear) %>% 
#   dplyr::select(microsim.init.id, year, AlcCAT) %>% 
#   pivot_wider(names_from=year, values_from=AlcCAT) %>% 
#   drop_na() %>% group_by(`2000`,`2001`) %>% tally() %>% ungroup() %>% group_by(`2000`) %>% mutate(prop=n/sum(n))
# 
# test <- Pop %>% group_by(``)
# 
# basetarget <- baseorig %>% mutate(agecat=cut(microsim.init.age, breaks=c(0,24,64,100), labels=c("18-24","25-64","65+"))) %>% 
#                                     group_by(microsim.init.sex, agecat, microsim.init.race, microsim.init.education, AlcCAT) %>% 
#                                     tally() %>% 
#                                     ungroup() %>% 
#                                     group_by(microsim.init.sex, agecat, microsim.init.race, microsim.init.education) %>% 
#                                     mutate(prop=n/sum(n))
# 
#                                   