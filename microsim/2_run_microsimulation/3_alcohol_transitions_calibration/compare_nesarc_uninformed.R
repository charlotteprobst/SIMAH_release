test <- implausibility %>% 
  # filter(microsim.init.sex=="m" & 
  #          microsim.init.race=="SPA" & 
  #          microsim.init.education=="LEHS" & agecat=="65+" & 
  #          AlcCAT=="Low risk") %>% 
  group_by(samplenum) %>% 
  summarise(max_implausibility=max(implausibility, na.rm=T)) %>% 
  filter(max_implausibility<5)

implausibility <- implausibility %>% 
  filter(samplenum %in% samples)

ggplot(subset(implausibility, AlcCAT=="Medium risk" & microsim.init.race=="WHI"), aes(x=year, y=propsimulation, colour=as.factor(samplenum))) + geom_line() +
  geom_line(aes(x=year, y=proptarget), colour="black", linewidth=1) + 
  geom_ribbon(aes(ymin=proptarget-1.96*se, ymax=proptarget+1.96*se), fill="grey", colour=NA, alpha=0.6) + 
  theme_bw() + 
  theme(legend.position="none") + ylim(0,NA) +
  facet_grid(cols=vars(microsim.init.sex, agecat), rows=vars(microsim.init.education)) +
  ggtitle("NESARC - Medium risk - White")
ggsave("SIMAH_workplace/WhiteMedNESARCfullunin_goodfit.png", dpi=300, width=33, height=19, units='cm')
