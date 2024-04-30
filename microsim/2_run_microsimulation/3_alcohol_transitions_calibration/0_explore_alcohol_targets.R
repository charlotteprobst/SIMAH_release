# exploring alcohol target data - to try to narrow down
compare <- left_join(output, targets) %>% 
  # filter(samplenum ==161) %>% 
  group_by(year, samplenum, microsim.init.sex, microsim.init.race,
           microsim.init.education, agecat, AlcCAT) %>% 
  summarise(propsimulation=mean(propsimulation),
            proptarget = mean(proptarget),
            se = mean(se),
            lower = proptarget - (1.96*se),
            lower = ifelse(lower<0, 0, lower),
            upper = proptarget + (1.96*se)) %>% 
  mutate(
         microsim.init.race = recode(microsim.init.race,
                                     "BLA"="Black","SPA"="Hispanic",
                                     "WHI"="White","OTH"="Others"),
         microsim.init.education = factor(microsim.init.education,
                                          levels=c("LEHS","SomeC","College")))
  # drop_na() %>% 
  # pivot_longer(cols=c(propsimulation, proptarget))

# loop to draw plots
categories <- expand.grid(sex = unique(targets$microsim.init.sex), alcoholcat = unique(targets$AlcCAT))
categories$cat <- paste0(categories$sex, categories$alcoholcat)

bestfit <- compare %>% filter(samplenum==161) %>% 
  mutate(value = ifelse(microsim.init.sex=="f" & agecat=="65+" & 
                          microsim.init.race=="Others" & microsim.init.education=="College" & 
                          AlcCAT=="Medium risk" & name=="proptarget", NA, value),
         upper = ifelse(microsim.init.sex=="f" & agecat=="65+" & 
                          microsim.init.race=="Others" & microsim.init.education=="College" & 
                          AlcCAT=="Medium risk", NA, upper),
         lower = ifelse(microsim.init.sex=="f" & agecat=="65+" & 
                          microsim.init.race=="Others" & microsim.init.education=="College" & 
                          AlcCAT=="Medium risk", NA, lower))

for(i in 1:nrow(categories)){
sex <- categories$sex[i]
alcoholcat <- categories$alcoholcat[i]
scaleFUN <- function(x) sprintf("%.2f", x)
title = sprintf(paste0(sex, " - ", alcoholcat))
ggplot(subset(compare, microsim.init.sex==sex & AlcCAT==alcoholcat), 
       aes(x=year, y=propsimulation, colour=samplenum, group=samplenum)) +
  geom_ribbon(aes(ymin=lower, ymax=upper),fill="grey",
              colour=NA, alpha=0.4) + 
  geom_line(linewidth=1) +
  facet_grid(cols=vars(agecat, microsim.init.education), rows=vars(microsim.init.race), scales="fixed") +
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA)) + 
# facet_wrap(~microsim.init.education+microsim.init.race+microsim.init.sex) + +
  theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + 
  ggtitle(title)
# save plot 
ggsave(paste0("SIMAH_workplace/microsim/2_output_data/alcohol_calibration/first_calibration/", sex, alcoholcat, "uninformedbest.png"),
       dpi=300, width=33, height=19, units="cm")
}
