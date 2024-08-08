
uncertainty <- read.csv("output_data/uncertainty_estimates.csv")

uncertainty$microsim.init.education <- factor(uncertainty$microsim.init.education,
                                              levels=c("High school diploma or less",
                                                       "Some college",
                                                       "College degree or more"))

uncertainty$microsim.init.sex <- factor(uncertainty$microsim.init.sex, levels=c("Males","Females"))

ggplot(data=uncertainty, aes(x=year, y=percent, colour=datatype)) + geom_line()  + geom_point(size=1) + 
  facet_grid(rows=vars(microsim.init.education), cols=vars(microsim.init.sex)) + 
  geom_ribbon(aes(ymin=min, ymax=max), alpha=0.7, fill="grey", show.legend=T) + theme_bw() +
  ylim(0,1) + scale_fill_brewer(palette = "Set2") + ylab("Percentage (%)") +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text=element_text(size=12),
        strip.background =element_rect(fill="white", colour="black"))

ggsave("plots/uncertainty.png", dpi=300, width=33, height=17, units="cm")
