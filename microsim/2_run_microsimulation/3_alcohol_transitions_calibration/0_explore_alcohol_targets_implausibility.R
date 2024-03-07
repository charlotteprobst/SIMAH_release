# exploring alcohol target data - to try to narrow down
library(tidyverse)

targets <- brfss %>%
  mutate(agecat = cut(microsim.init.age,
                      breaks=c(0,24,34,44,54,64,100),
                      labels=c("18-24","25-34","35-44","45-54",
                               "55-64","65+")),
         year_cat = cut(YEAR,
                        breaks=c(0,2003,2006,2009,2012,2015,2018,2021),
                        labels=c("2000-2003","2004-2006","2007-2009","2010-2012","2013-2015",
                                 "2016-2018","2019-2021"))) %>%
  rename(year=YEAR) %>%
  group_by(year_cat, microsim.init.sex,microsim.init.race,agecat, microsim.init.education,AlcCAT, .drop=FALSE) %>% tally() %>%
  ungroup() %>%
  group_by(year_cat, microsim.init.sex,microsim.init.race, agecat, microsim.init.education) %>%
  mutate(proptarget = n/sum(n),
         se = sqrt(proptarget * (1 - proptarget) / sum(n)),
         agecat = as.character(agecat)) %>% 
         
         # microsim.init.race = recode(microsim.init.race,
         #                             "BLA"="Black","SPA"="Hispanic",
         #                             "WHI"="White","OTH"="Others"),
         # microsim.init.education = factor(microsim.init.education,
         #                                  levels=c("LEHS","SomeC","College"))) %>%
  dplyr::select(-n) %>% drop_na()

Output <- read_csv(paste0(OutputDirectory, "/output-1.csv"))
  mutate(year_cat = cut(year,
                         breaks=c(0,2003,2006,2009,2012,2015,2018,2021),
                         labels=c("2000-2003","2004-2006","2007-2009","2010-2012","2013-2015",
                                  "2016-2018","2019-2021"))) %>%
  group_by(year_cat, samplenum, microsim.init.sex,microsim.init.race,agecat,
           microsim.init.education, AlcCAT) %>%
  summarise(n=mean(n)) %>%
  ungroup() %>%
  group_by(year_cat, samplenum, microsim.init.sex, microsim.init.race,agecat,
           microsim.init.education) %>%
  mutate(propsimulation = n/sum(n))
  


# loop to draw plots
categories <- expand.grid(sex = unique(targets$microsim.init.sex), alcoholcat = unique(targets$AlcCAT))
categories$cat <- paste0(categories$sex, categories$alcoholcat)

forplot <- compare %>% filter(samplenum==68) %>% 
  pivot_longer(cols=c(propsimulation,proptarget))

for(i in 1:nrow(categories)){
sex <- categories$sex[i]
alcoholcat <- categories$alcoholcat[i]
scaleFUN <- function(x) sprintf("%.2f", x)
title = sprintf(paste0(sex, " - ", alcoholcat))
ggplot(subset(forplot, microsim.init.sex==sex & AlcCAT==alcoholcat), 
       aes(x=year_cat, y=value, colour=as.factor(microsim.init.education),
           group=interaction(microsim.init.education, name),
           linetype=name)) +
  geom_line(linewidth=1) +
  facet_grid(cols=vars(agecat), rows=vars(microsim.init.race), scales="fixed") +
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA)) + 
  scale_linetype_manual(values=c("dashed","solid")) + 
# facet_wrap(~microsim.init.education+microsim.init.race+microsim.init.sex) +
# geom_ribbon(aes(ymin=proptarget-1.96*se, ymax=proptarget+1.96*se, fill=microsim.init.education),
#             colour=NA, alpha=0.4) +
  theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  ggtitle(title) + xlab("")
# save plot 
ggsave(paste0("SIMAH_workplace/microsim/2_output_data/alcohol_calibration/target_plots/", sex, alcoholcat, "comparetarget.png"),
       dpi=300, width=33, height=19, units="cm")
}
