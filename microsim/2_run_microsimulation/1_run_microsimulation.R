#####SIMAH project 2022 - script for running SIMAH microsimulation model
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(devtools)
library(roxygen2)
library(dplyr)
library(tidyverse)
options(dplyr.summarise.inform = FALSE)

###set working directory to the main "SIMAH" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(paste(WorkingDirectory))

# load in microsim R package
setwd("SIMAH_code")
install("microsimpackage", dep=T)

setwd(paste(WorkingDirectory))

source("SIMAH_code/microsim/2_run_microsimulation/0_model_settings.R")

Output <- list()
Output <- run_microsim(1,1,basepop,brfss,
                       death_rates, apply_death_rates,
                       updatingeducation, education_setup,education_transitions,
                       outward_migration, inward_migration, migration_rates,
                       updatingalcohol, alcohol_transitions, transition_alcohol,
                       2000, 2019, "demographics")

summary <- summarise_education_output(Output, SelectedState)

source("SIMAH_code/microsim/2_run_microsimulation/2_postprocessing_scripts/postprocess_alcohol.R")

# calculate error from target data 
Output <- do.call(rbind,Output)
Output <- Output %>% group_by(samplenum, year, microsim.init.sex, microsim.init.education,
                              AlcCAT, .drop=FALSE) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% 
  group_by(samplenum, year, microsim.init.sex, microsim.init.education) %>% 
  mutate(microsimpercent = n/sum(n),
         year=as.numeric(as.character(year))) %>% dplyr::select(-n)
Output <- left_join(Output, target)


# saveRDS(Output, "output_fullpop.RDS")
saveRDS(Output[[1]], "SIMAH_workplace/microsim/2_output_data/output_fullpop.RDS")
saveRDS(Output[[2]], "SIMAH_workplace/microsim/2_output_data/output_deaths.RDS")

source("SIMAH_code/microsim/2_run_microsimulation/1_functions/compare_output_target.R")

compare <- compare_output_target(Output[[1]])
# compare <- compare %>% filter(Year<=2018)
# compare$percentdiff <- (abs(compare$microsim-compare$target) / compare$target)*100
# compare$target <- ifelse(compare$Year>=2020, NA, compare$target)
compare <- compare %>% pivot_longer(cols=c(target,microsim)) %>% 
  mutate(microsim.init.sex = recode(microsim.init.sex, "m"="Men","f"="Women"),
         microsim.init.race = recode(microsim.init.race, "BLA"="Black","WHI"="White","SPA"="Hispanic",
                                     "OTH"="Others"),
         agecat = as.character(agecat),
         agecat = ifelse(agecat=="18","18-24",
                         ifelse(agecat=="19-24","18-24",agecat))) %>% 
  group_by(Year, microsim.init.sex, microsim.init.race, agecat, name) %>% 
  summarise(value=sum(value),
            lower_ci = lower_ci,
            upper_ci = upper_ci) %>% 
  mutate(microsim.init.race = factor(microsim.init.race, levels=c("Black","White","Hispanic","Others")),
         name = recode(name, "microsim"="Microsim","target"="ACS / Census"))
compare$Year <- as.numeric(as.character(compare$Year))

men <- compare %>% filter(microsim.init.sex=="Men")
bars <- men %>% 
  filter(name=="ACS / Census")
points <- men %>% 
  filter(name=="Microsim")
p <- ggplot(men, aes(value))
p

p1 <- p + geom_bar(data=bars, aes(fill=name, x=Year, y=value, group=name), colour="darkblue", stat="identity", position="dodge") +
  geom_errorbar(data=bars, aes(x=Year, group=name, ymin=lower_ci, ymax=upper_ci)) + 
  geom_point(data=points, aes(x=Year, y=value, group=name, shape=name), colour="darkblue", size=2) + 
  facet_grid(cols=vars(agecat),rows=vars(microsim.init.race),scales="free") + 
  geom_line(data=points, aes(x=Year, y=value, group=name), colour="darkblue") + 
  scale_fill_manual(name="",values="white") + scale_shape_manual(name="", values=18) +
  theme_classic() + ylab("Total Population") + xlab("Year") +theme(legend.title=NULL, legend.margin = margin(-1,0,0,0, "cm")) + 
  ggtitle("population comparison - men")
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 13000000),
  #                    labels=function(x) 
  #                      format(x, big.mark=",", scientific=FALSE))
p1
ggsave("SIMAH_workplace/microsim/2_output_data/plots/compare_pop_2020_M.png",
       dpi=300, width=33, height=19, units="cm")

women <- compare %>% filter(microsim.init.sex=="Women")
bars <- women %>% 
  filter(name=="ACS / Census")
points <- women %>% 
  filter(name=="Microsim")
p <- ggplot(men, aes(value))
p

p1 <- p + geom_bar(data=bars, aes(fill=name, x=Year, y=value, group=name), colour="darkblue", stat="identity", position="dodge") +
  geom_errorbar(data=bars, aes(x=Year, group=name, ymin=lower_ci, ymax=upper_ci)) + 
  geom_point(data=points, aes(x=Year, y=value, group=name, shape=name), colour="darkblue", size=2) + 
  facet_grid(cols=vars(agecat),rows=vars(microsim.init.race),scales="free") + 
  geom_line(data=points, aes(x=Year, y=value, group=name), colour="darkblue") + 
  scale_fill_manual(name="",values="white") + scale_shape_manual(name="", values=18) +
  theme_classic() + ylab("Total Population") + xlab("Year") +theme(legend.title=NULL, legend.margin = margin(-1,0,0,0, "cm")) + 
  ggtitle("population comparison - women")
# scale_y_continuous(expand = c(0, 0), limits = c(0, 13000000),
#                    labels=function(x) 
#                      format(x, big.mark=",", scientific=FALSE))
p1
ggsave("SIMAH_workplace/microsim/2_output_data/plots/compare_pop_2020_F.png",
       dpi=300, width=33, height=19, units="cm")





percentdifference <- compare %>% dplyr::select(Year, microsim.init.sex, microsim.init.education, data,
                                               value) %>% pivot_wider(names_from=data, values_from=value) %>% 
  mutate(percentdiff = abs(microsim-ACS)/ACS)


percentdifference <- compare %>% dplyr::select(Year, microsim.init.sex, microsim.init.education, data,
                                               percent) %>% pivot_wider(names_from=data, values_from=percent) %>% 
  mutate(diff = abs(microsim - ACS)) %>% 
  group_by(microsim.init.sex, microsim.init.education) %>% 
  filter(Year<=2018)






