# SIMAH project Sep 2023 
# analysing the run to run variability of education transitions
library(tidyverse)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/education_calibration")

# load in microsim R package
setwd(paste(WorkingDirectory))

data <- read.csv(paste0(DataDirectory, "/education_transitions_range_reps-1.csv"))

meanbyseed <- function(data, selected_seed){
  mean <- data %>% filter(year>=2001) %>% 
    filter(microsim.init.age<=34) %>% 
    filter(seed<=selected_seed) %>% 
    group_by(year, samplenum, microsim.init.sex,microsim.init.race, microsim.init.education) %>% 
    summarise(mean = mean(n)) %>% 
    ungroup() %>% 
    group_by(year, samplenum, microsim.init.sex, microsim.init.race) %>% 
    mutate(prop = mean/sum(mean), nsamples=selected_seed)
  return(mean)
}

means <- list() 

for(i in 1:max(data$seed)){
  means[[i]] <- meanbyseed(data, i)
}

means <- do.call(rbind, means)

means <- means %>% 
  mutate(microsim.init.race = recode(microsim.init.race, "BLA"="Black",
                                     "WHI"="White","SPA"="Hispanic","OTH"="Others"),
         microsim.init.education = factor(microsim.init.education,
                                          levels=c("LEHS","SomeC","College")))
  

scaleFUN <- function(x) sprintf("%.2f", x)

# check for 2003, 2010 and 2016
ggplot(data=subset(means, year==2016 & samplenum==3), aes(x=nsamples, y=prop, colour=microsim.init.sex)) + 
  geom_line(linewidth=1) +
  facet_grid(cols=vars(microsim.init.race), rows=vars(microsim.init.education), scales="free") + 
  scale_y_continuous(labels=scales::percent) +
  theme_bw() + ylab("Proportion") +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        strip.background = element_rect(fill="white")) + ylim(0,NA)
ggsave(paste0(DataDirectory, "/replication_plots_2016.png"), dpi=300, width=33, height=19, units="cm")

means <- means %>% filter(year==2003 | year==2010 | year==2016)

ggplot(data=subset(means, microsim.init.race=="Others" & microsim.init.education=="SomeC"), 
       aes(x=nsamples, y=prop, colour=microsim.init.sex)) + 
  geom_line(linewidth=1) +
  facet_grid(cols=vars(samplenum), rows=vars(year), scales="free") + 
  scale_y_continuous(labels=scales::percent) +
  theme_bw() + ylab("Proportion") +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        strip.background = element_rect(fill="white"))

ggsave(paste0(DataDirectory, "/replication_plots_zoom_others.png"), dpi=300, width=33, height=19, units="cm")


variability <- means %>% 
  group_by(year, samplenum, microsim.init.sex, microsim.init.race, microsim.init.education) %>% 
  summarise(min=min(prop),
            max=max(prop),
            range = abs(max-min))
