# where to save the outputs 
OutputDirectory <- paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/multinom_int_calibration")

Pop <- do.call(rbind,PopPerYear)

meanbrfss <- brfss %>% 
  mutate(agecat=cut(microsim.init.age,
                    breaks=c(0,24,64,100),
                    labels=c("18-24","25-64","65+"))) %>% 
  group_by(YEAR, microsim.init.sex, agecat, microsim.init.education, microsim.init.race) %>% 
  filter(microsim.init.alc.gpd!=0) %>% 
  summarise(meanbrfss = mean(microsim.init.alc.gpd),
            se = std.error(microsim.init.alc.gpd)) %>% 
  rename(year=YEAR)

meansimulation <- Pop %>% 
  mutate(agecat=cut(microsim.init.age,
                    breaks=c(0,24,64,100),
                    labels=c("18-24","25-64","65+"))) %>% 
  group_by(year, microsim.init.sex, agecat, microsim.init.education, microsim.init.race) %>% 
  filter(microsim.init.alc.gpd!=0) %>% 
  summarise(meansimulation = mean(microsim.init.alc.gpd))

meansimulation <- left_join(meansimulation, meanbrfss) %>% 
  pivot_longer(meansimulation:meanbrfss)

meansimulation <- meansimulation %>% 
  mutate(microsim.init.race = case_when(microsim.init.race=="BLA" ~ "Black",
                                        microsim.init.race=="WHI" ~ "White",
                                        microsim.init.race=="SPA" ~ "Hispanic",
                                        microsim.init.race=="OTH" ~ "Others"),
         microsim.init.education = factor(microsim.init.education, 
                                          levels=c("LEHS","SomeC","College")))
meansimulation$se <- ifelse(meansimulation$name=="meansimulation", NA, meansimulation$se)

ggplot(data=subset(meansimulation, microsim.init.race=="White" | microsim.init.race=="Black"), aes(x=year, y=value, colour=name, fill=name)) + 
  geom_line(linewidth=1) + geom_ribbon(aes(ymin=value-(1.96*se), max=value+(1.96*se)), colour=NA, alpha=0.6) + 
  facet_grid(cols=vars(microsim.init.sex,microsim.init.education), rows=vars(microsim.init.race,agecat),scales="free") + ylim(0,NA)
ggsave(paste0(OutputDirectory, "/compare_mean_drinking_betadistributions_byeducationraceagewhiteblack_byyear.png"), width=33, height=19, units="cm")



trajectories <- Pop %>% 
  dplyr::select(microsim.init.id, microsim.init.race, microsim.init.sex,
                microsim.init.alc.gpd, year) %>% pivot_wider(names_from=year, values_from=microsim.init.alc.gpd) %>% 
  filter(
    `2000` != `2001` | `2001` != `2002` | `2002` != `2003` | 
      `2003` != `2004` | `2004` != `2005` | `2005` != `2006` | 
      `2006` != `2007` | `2007` != `2008` | `2008` != `2009` | 
      `2009` != `2010`
  )

samples <- sample(trajectories$microsim.init.id, 20, replace=F)

selected <- Pop %>% filter(microsim.init.id %in% samples) 

library(ggplot2)
ggplot(data=selected, aes(x=year, y=microsim.init.alc.gpd, colour=as.factor(microsim.init.id))) + geom_point() +
  geom_line() + facet_grid(rows=vars(microsim.init.sex)) +
  theme(legend.position="none")




# which model
outputname <- "output-4"

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

implausibility <- read_csv(paste0(OutputDirectory, "/implausibility-4.csv"))

for(i in unique(Output$AlcCAT)){
  for(j in unique(Output$microsim.init.race)){
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
  scale_x_continuous(breaks=c(2000,2002,2004,2006,2008,2010)) + 
  # ylim(0,NA) +
  facet_grid(cols=vars(microsim.init.sex, agecat), rows=vars(microsim.init.education)) +
  ggtitle(paste0("Deterministic method", i, "-", j))
ggsave(paste0(OutputDirectory, "/plots/",outputname, i, j, ".png"), dpi=300, width=33, height=19, units='cm')
  }
}

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