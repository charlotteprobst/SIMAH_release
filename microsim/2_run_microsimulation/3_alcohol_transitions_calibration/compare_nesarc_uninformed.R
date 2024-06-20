test <- implausibility %>% 
  # filter(microsim.init.sex=="m" & 
  #          microsim.init.race=="SPA" & 
  #          microsim.init.education=="LEHS" & agecat=="65+" & 
  #          AlcCAT=="Low risk") %>% 
  group_by(samplenum) %>% 
  summarise(max_implausibility=max(implausibility, na.rm=T)) %>% 
  filter(max_implausibility<5)

implausibility <- Output %>% 
  filter(samplenum ==38)
Output$samplenum <- 1

Output <- do.call(rbind, CatSummary) %>% mutate(samplenum=1)
write.csv(Output, paste0(OutputDirectory, "/outputdeterministic_model.csv"), row.names=F)
max(Output$implausibility, na.rm=T)

Output <- read_csv(paste0(OutputDirectory, "/outputdeterministic_model.csv")) %>% 
  mutate(lower = proptarget - 1.96*se, upper = proptarget + 1.96*se) %>% 
  dplyr::select(year, microsim.init.sex, microsim.init.race, agecat, microsim.init.education,
                AlcCAT, propsimulation, proptarget, lower, upper,implausibility) %>% 
  pivot_longer(propsimulation:proptarget) %>% 
  mutate(value = ifelse(name=="propsimulation" & is.na(value), 0, value))

scaleFUN <- function(x) sprintf("%.2f", x)

Output <- Output %>% filter(implausibility<5)

toremove <- Output %>% filter(implausibility>5) %>% mutate(removecat = paste0(year,microsim.init.sex,microsim.init.race,
                                                                              agecat, microsim.init.education, AlcCAT))


toremovecats <- unique(toremove$removecat)

Poptouse <- do.call(rbind,PopPerYear) %>% 
  mutate(agecat = cut(microsim.init.age, breaks=c(0,24,64,100),
                      labels=c("18-24","25-64","65+")),
         cat = paste0(year,microsim.init.sex,microsim.init.race,
                      agecat, microsim.init.education, AlcCAT))

Popfiltered <- Poptouse %>% filter(!cat %in% toremovecats)

write.csv(Popfiltered, paste0(OutputDirectory, "/full_pop_deterministic.csv"), row.names=F)


for(i in unique(Output$AlcCAT)){
ggplot(subset(Output, AlcCAT==i & microsim.init.race=="WHI"), aes(x=year, y=value, colour=as.factor(name))) + 
  geom_line(linewidth=1) + 
  # geom_point() +
  # geom_line(aes(x=year, y=proptarget), colour="black", linewidth=1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey", colour=NA, alpha=0.6) + 
  theme_bw() + 
    # ylim(0,NA)+ 
  theme(legend.position="bottom",
        legend.title=element_blank()) + 
  scale_y_continuous(limits=c(0,NA), labels=scaleFUN) + 
  scale_x_continuous(breaks=c(2000,2002,2004,2006,2008,2010)) + 
  # ylim(0,NA) +
  facet_grid(cols=vars(microsim.init.sex, agecat), rows=vars(microsim.init.education)) +
  ggtitle(paste0("Deterministic method", i, "- White"))
ggsave(paste0("SIMAH_workplace/microsim/2_output_data/regression_models_output/deterministic_model", i, ".png"), dpi=300, width=33, height=19, units='cm')
}
