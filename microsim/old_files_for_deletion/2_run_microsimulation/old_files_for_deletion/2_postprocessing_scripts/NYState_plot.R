# trends for alcohol in NY state 
library(plotrix)
summary <- brfssorig %>% group_by(YEAR, microsim.init.sex) %>% 
  filter(microsim.init.alc.gpd!=0) %>% 
  summarise(meanGPD = mean(microsim.init.alc.gpd),
            SE = std.error(microsim.init.alc.gpd)) %>% 
  mutate(microsim.init.sex=ifelse(microsim.init.sex=="m","Men","Women"))


ggplot(data=summary, aes(x=YEAR, y=meanGPD, colour=microsim.init.sex)) + 
  geom_line() + theme_bw() + xlab("Year") + 
  ylab("mean grams per day (in drinkers) +/- 1 SE") + 
  theme(legend.title=element_blank(),
        legend.position="bottom") + 
  ylim(0,NA) + 
  geom_errorbar(aes(ymin=meanGPD-SE, ymax=meanGPD+SE)) +
  scale_colour_manual(values=c("grey70","black"))
ggsave("SIMAH_workplace/microsim/2_output_data/plots/NYGPD.png",
       dpi=300, width=33, height=19, units="cm")  
