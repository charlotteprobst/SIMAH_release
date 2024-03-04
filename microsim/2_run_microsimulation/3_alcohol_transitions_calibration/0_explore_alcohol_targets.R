# exploring alcohol target data - to try to narrow down
targets <- brfss %>%
  mutate(agecat = cut(microsim.init.age,
                      breaks=c(0,24,34,44,54,64,100),
                      labels=c("18-24","25-34","35-44","45-54",
                               "55-64","65+")),
         year_cat = cut(YEAR,
                        breaks=c(0,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020,2022),
                        labels=c("2000-2002","2003-2004","2005-2006","2007-2008","2009-2010",
                                 "2011-2012","2013-2014","2015-2016","2017-2018","2019-2020",
                                 "2021-2022"))) %>%
  rename(year=YEAR) %>%
  group_by(year_cat, microsim.init.sex,microsim.init.race,agecat, microsim.init.education,AlcCAT, .drop=FALSE) %>% tally() %>%
  ungroup() %>%
  group_by(year_cat, microsim.init.sex,microsim.init.race, agecat, microsim.init.education) %>%
  mutate(proptarget = n/sum(n),
         se = sqrt(proptarget * (1 - proptarget) / sum(n)),
         agecat = as.character(agecat),
         
         microsim.init.race = recode(microsim.init.race,
                                     "BLA"="Black","SPA"="Hispanic",
                                     "WHI"="White","OTH"="Others"),
         microsim.init.education = factor(microsim.init.education,
                                          levels=c("LEHS","SomeC","College"))) %>%
  dplyr::select(-n)


# loop to draw plots
categories <- expand.grid(sex = unique(targets$microsim.init.sex), alcoholcat = unique(targets$AlcCAT))
categories$cat <- paste0(categories$sex, categories$alcoholcat)

for(i in 1:nrow(categories)){
sex <- categories$sex[i]
alcoholcat <- categories$alcoholcat[i]
scaleFUN <- function(x) sprintf("%.2f", x)
title = sprintf(paste0(sex, " - ", alcoholcat))
ggplot(subset(targets, microsim.init.sex==sex & AlcCAT==alcoholcat), 
       aes(x=year_cat, y=proptarget, colour=microsim.init.education, group=microsim.init.education)) +
  geom_line(linewidth=1) +
  facet_grid(cols=vars(agecat), rows=vars(microsim.init.race), scales="fixed") +
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA)) + 
# facet_wrap(~microsim.init.education+microsim.init.race+microsim.init.sex) +
# geom_ribbon(aes(ymin=proptarget-1.96*se, ymax=proptarget+1.96*se, fill=microsim.init.education),
#             colour=NA, alpha=0.4) +
  theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position="bottom") + 
  ggtitle(title)
# save plot 
ggsave(paste0("SIMAH_workplace/microsim/2_output_data/alcohol_calibration/target_plots/", sex, alcoholcat, "groupedyears.png"),
       dpi=300, width=33, height=19, units="cm")
}
