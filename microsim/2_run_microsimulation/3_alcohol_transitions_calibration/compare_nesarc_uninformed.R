test <- implausibility %>% filter(microsim.init.sex=="m" & microsim.init.race=="SPA" & 
                                    microsim.init.education=="LEHS" & agecat=="65+" & 
                                    AlcCAT=="Low risk") %>% 
  group_by(samplenum) %>% 
  mutate(max_implausibility=max(implausibility)) %>% 
  filter(max_implausibility<5)

ggplot(subset(implausibility_PSID, AlcCAT=="Non-drinker" & microsim.init.race=="WHI"), aes(x=year, y=propsimulation, colour=as.factor(samplenum))) + geom_line() +
  geom_line(aes(x=year, y=proptarget), colour="black") + 
  geom_ribbon(aes(ymin=proptarget-1.96*se, ymax=proptarget+1.96*se), fill="grey", colour=NA, alpha=0.6) + 
  theme_bw() + 
  theme(legend.position="none") + ylim(0,NA) +
  facet_grid(cols=vars(microsim.init.sex, agecat), rows=vars(microsim.init.education)) +
  ggtitle("PSID - Non-drinkers - White")
ggsave("SIMAH_workplace/WhiteNDPSIDprior.png", dpi=300, width=33, height=19, units='cm')
