# compare the NESARC and BRFSS data for inflation purposes 

targets <- generate_targets_alcohol(brfss)

NESARC <- read_csv("SIMAH_workplace/nesarc/Output/NESARC_proportions.csv") %>% 
  mutate(year = ifelse(wave==1, 2002, 
                       ifelse(wave==2, 2005, NA)),
         microsim.init.sex=ifelse(female==1, "f", "m"),
         microsim.init.race = case_when(race.factor=="White, non-Hispanic" ~ "WHI",
                                        race.factor=="Black, non-Hispanic" ~ "BLA",
                                        race.factor=="Other, non-Hispanic" ~ "OTH",
                                        race.factor=="Hispanic" ~ "SPA"),
         microsim.init.education= case_when(edu3=="Low" ~ "LEHS",
                                            edu3=="Med" ~ "SomeC",
                                            edu3=="High" ~ "College"),
         microsim.init.education = ifelse(age3=="18-24" & microsim.init.education=="College",
                                          "SomeC",microsim.init.education),
         AlcCAT = case_when(alc4.factor=="Category I" ~ "Low risk",
                            alc4.factor=="Category II" ~ "Medium risk",
                            alc4.factor=="Category III" ~ "High risk",
                            alc4.factor=="Non-drinker" ~ "Non-drinker")) %>% 
  rename(agecat=age3) %>% 
  group_by(year, microsim.init.sex, microsim.init.race, agecat, microsim.init.education,
           AlcCAT) %>% 
  summarise(n=sum(n)) %>% 
  ungroup() %>% 
  group_by(year, microsim.init.sex, microsim.init.race, agecat, microsim.init.education) %>% 
  mutate(propNESARC=n/sum(n)) %>% 
  dplyr::select(-n)

targets <- left_join(targets, NESARC) %>% filter(year==2002) %>% 
  mutate(magnitude = proptarget/propNESARC,
         magnitude2 = propNESARC/proptarget)

# inflation of 16 - however there are some groups missing entirely (high and med risk 65+)
# so can probably justify further inflation
forplot <- targets %>% 
  pivot_longer(c(proptarget,propNESARC))

cats <- unique(forplot$AlcCAT)
for(i in 1:length(cats)){
  alcoholcat <- cats[i]
  scaleFUN <- function(x) sprintf("%.2f", x)
  title = sprintf(paste0(alcoholcat))
  ggplot(subset(forplot, AlcCAT==alcoholcat), 
         aes(x=microsim.init.education, y=value, colour=name, group=name)) +
    geom_point() +
    facet_grid(cols=vars(agecat, microsim.init.race), rows=vars(microsim.init.sex), scales="fixed") +
    scale_y_continuous(labels=scaleFUN, limits=c(0,NA)) + 
    # facet_wrap(~microsim.init.education+microsim.init.race+microsim.init.sex) +
    theme_bw() + 
    theme(legend.title=element_blank(),
          legend.position="bottom",
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + 
    ggtitle(title)
  # save plot 
  ggsave(paste0("SIMAH_workplace/microsim/2_output_data/alcohol_calibration/first_calibration/", alcoholcat, "target_compare.png"),
         dpi=300, width=33, height=19, units="cm")
}
  
