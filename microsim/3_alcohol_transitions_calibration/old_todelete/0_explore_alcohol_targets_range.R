# exploring alcohol target data - to try to narrow down
compare <- left_join(output, targets) %>% 
  # filter(samplenum<=10) %>%
  group_by(year, microsim.init.sex, microsim.init.race,
           microsim.init.education, agecat, AlcCAT) %>% 
  summarise(
    # propsimulation=mean(propsimulation),
            prop_target = mean(proptarget),
            se = mean(se),
            lower_target = prop_target - (1.96*se),
            lower_target = ifelse(lower_target<0, 0, lower_target),
            upper_target = prop_target + (1.96*se),
            lower_simulation = min(propsimulation),
            upper_simulation = max(propsimulation),
            prop_simulation = mean(propsimulation)) %>% 
  mutate(
         microsim.init.race = recode(microsim.init.race,
                                     "BLA"="Black","SPA"="Hispanic",
                                     "WHI"="White","OTH"="Others"),
         microsim.init.education = factor(microsim.init.education,
                                          levels=c("LEHS","SomeC","College"))) %>% 
  drop_na() %>% 
  dplyr::select(-se) %>% 
  pivot_longer(prop_target:prop_simulation) %>% 
  separate(name, into=c("number","type")) %>% 
  pivot_wider(names_from=number, values_from=value) %>% 
  mutate(prop = ifelse(type=="target" & microsim.init.sex=="f" & year==2000 &
                          microsim.init.race=="Others" & microsim.init.education=="College" & 
                          agecat=="65+", NA, prop),
         lower = ifelse(type=="target" & microsim.init.sex=="f" & year==2000 & 
                         microsim.init.race=="Others" & microsim.init.education=="College" & 
                         agecat=="65+", NA, lower),
         upper = ifelse(type=="target" & microsim.init.sex=="f" & year==2000 &
                         microsim.init.race=="Others" & microsim.init.education=="College" & 
                         agecat=="65+", NA, upper))
         

# loop to draw plots
categories <- expand.grid(sex = unique(targets$microsim.init.sex), alcoholcat = unique(targets$AlcCAT))
categories$cat <- paste0(categories$sex, categories$alcoholcat)

for(i in 1:nrow(categories)){
sex <- categories$sex[i]
alcoholcat <- categories$alcoholcat[i]
scaleFUN <- function(x) sprintf("%.2f", x)
title = sprintf(paste0(sex, " - ", alcoholcat))
ggplot(subset(compare, microsim.init.sex==sex & AlcCAT==alcoholcat), 
       aes(x=year, y=prop, colour=type, group=type)) +
  geom_line(linewidth=1) +
  facet_grid(cols=vars(agecat, microsim.init.education), rows=vars(microsim.init.race), scales="fixed") +
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA)) + 
# facet_wrap(~microsim.init.education+microsim.init.race+microsim.init.sex) +
geom_ribbon(aes(ymin=lower, ymax=upper, fill=type),
            colour=NA, alpha=0.4) +
  theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + 
  ggtitle(title)
# save plot 
ggsave(paste0("SIMAH_workplace/microsim/2_output_data/alcohol_calibration/first_calibration/", sex, alcoholcat, "new_ref_3cat.png"),
       dpi=300, width=33, height=19, units="cm")
}
