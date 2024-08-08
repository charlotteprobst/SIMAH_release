# exploring alcohol target data - to try to narrow down
Outputs <- read.csv("SIMAH_workplace/microsim/2_output_data/alcohol_calibration/calibration1/output-1.csv")

summary_output <- Outputs %>% 
  mutate(
    # agecat = cut(microsim.init.age,
    #              breaks=c(0,24,64,100),
    #              labels=c("18-24","25-64","65+")),
    agecat = ifelse(agecat=="25-34" | agecat=="35-44" | agecat=="45-54" | agecat=="55-64", "25-64",
                    agecat),
    year_cat = cut(year,
                   breaks=c(0,2003,2006,2009,2012,2015,2018,2021),
                   labels=c("2000-2003","2004-2006","2007-2009",
                            "2010-2012","2013-2015","2016-2018","2019-2021")),
    microsim.init.education = as.character(microsim.init.education),
    microsim.init.education = ifelse(agecat=="18-24" & microsim.init.education=="SomeC", "LEHS",
                                     microsim.init.education)) %>%  
  group_by(samplenum, seed,year_cat, microsim.init.sex,microsim.init.race,agecat,microsim.init.education,AlcCAT) %>% 
  summarise(n=sum(n)) %>% 
  ungroup() %>% 
  group_by(samplenum, seed, year_cat,microsim.init.sex,microsim.init.race, agecat, microsim.init.education) %>% 
  mutate(proportion=n/sum(n)) %>% 
  group_by(samplenum, year_cat,microsim.init.sex,microsim.init.race, agecat, microsim.init.education, AlcCAT) %>% 
  summarise(propsimulation=mean(proportion))

targets <- brfss %>%
  mutate(
    agecat = cut(microsim.init.age,
                 breaks=c(0,24,64,100),
                 labels=c("18-24","25-64","65+")),
    year_cat = cut(year,
                   breaks=c(0,2003,2006,2009,2012,2015,2018,2021),
                   labels=c("2000-2003","2004-2006","2007-2009",
                            "2010-2012","2013-2015","2016-2018","2019-2021")),
    microsim.init.education = ifelse(agecat=="18-24" & microsim.init.education=="SomeC", "LEHS",
                                     microsim.init.education)) %>%
  # rename(year=YEAR) %>%
  group_by(year_cat, microsim.init.sex,microsim.init.race,agecat, microsim.init.education,AlcCAT, .drop=FALSE) %>%
  tally() %>%
  ungroup() %>%
  group_by(year_cat,microsim.init.sex,microsim.init.race, agecat, microsim.init.education) %>%
  mutate(proptarget = n/sum(n),
         se = sqrt(proportion * (1 - proportion) / sum(n)),
         upper = proptarget + (1.96*se),
         lower = proptarget - (1.96*se))

compare <- left_join(summary_output, targets) %>% filter(year_cat=="2000-2003") %>% 
  dplyr::select(-se)


ggplot(subset(compare, AlcCAT=="Medium risk"), aes(x=agecat, y=propsimulation, colour=samplenum, 
                                                 group=samplenum)) + 
  geom_line() +
  geom_line(aes(x=agecat, y=proptarget),colour="black", linewidth=1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="black",colour=NA, alpha=0.4) +
  facet_grid(cols=vars(microsim.init.education, microsim.init.sex), rows=vars(microsim.init.race)) +
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA)) +
  theme(legend.position="none")


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
