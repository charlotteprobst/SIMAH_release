summary <- brfssorig %>% group_by(YEAR, microsim.init.sex) %>% filter(microsim.init.alc.gpd!=0) %>% 
  summarise(meanGPD = mean(microsim.init.alc.gpd),
            SE = std.error(microsim.init.alc.gpd)) %>% 
  mutate(microsim.init.sex=ifelse(microsim.init.sex=="m","Men","Women"))

library(plotrix)

ggplot(data=summary, aes(x=YEAR, y=meanGPD, group=microsim.init.sex)) + 
  geom_line(size=0.75, aes(colour=microsim.init.sex)) +
  ylim(0,NA) + ylab("Mean grams alcohol per day +/- 1 SE") + xlab("Year") +
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     text = element_text(size=20, family="serif")) +
  # scale_colour_manual(values=c("grey70","black")) +
  scale_colour_brewer(palette="Set1") +
  geom_ribbon(aes(ymin=meanGPD-SE, ymax=meanGPD+SE), alpha=0.3, colour=NA)

ggsave("SIMAH_workplace/microsim/2_output_data/plots/meanalcperdayNYcolour.png",
       dpi=600, width=20, height=20, units="cm")
  