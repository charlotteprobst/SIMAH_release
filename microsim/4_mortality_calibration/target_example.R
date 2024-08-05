summary <- long_format %>% 
  group_by(year,sex,agecat,education, cause) %>% 
  summarise(popcount = sum(popcount),
            observed_mortality = sum(observed_mortality_n),
            mortalityrate = (observed_mortality/popcount)*100000) %>% 
  mutate(education = factor(education, 
                            levels=c("LEHS","SomeC","College")))

agest_weights <- long_format %>% filter(year==2010) %>% 
  filter(cause=="LVDC") %>% 
  ungroup() %>% 
  group_by(sex, agecat, education) %>% 
  summarise(popcount = sum(popcount)) %>% 
  distinct() %>% ungroup() %>% 
  group_by(sex, education) %>% 
  mutate(prop=popcount/sum(popcount)) %>% 
  dplyr::select(-popcount)

summary <- left_join(summary, agest_weights)

summary$weightedmortality <- summary$prop*summary$mortalityrate
  
agest_mortality <- summary %>% 
  group_by(year, sex, education, cause) %>% 
  summarise(mortality_rate = sum(weightedmortality))

# work out which ones are increasing and decreasing
cor(agest_mortality$year, agest_mortality$mortality_rate)

lm_trends <- agest_mortality %>% 
  group_by(sex, education, cause) %>% 
  do(model = cor(.$mortality_rate, .$year, method="kendall")) %>% 
  unnest(model) %>% 
  mutate(increasing = ifelse(model>0, 1,0)) %>% 
  group_by(cause) %>% 
  mutate(increasing_all = mean(increasing))

agest_mortality <- left_join(agest_mortality, lm_trends)
library(ggplot2)

ggplot(data=subset(agest_mortality, increasing_all==0), aes(x=year, y=mortality_rate, colour=education)) + 
  geom_line() + facet_grid(rows=vars(cause), cols=vars(sex), scales="free") + 
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank()) + 
  ylab("age st. mortality per 100,000") + ggtitle("all decreasing")
ggsave("SIMAH_workplace/microsim/2_output_data/mortality_calibration/target_data/targets_alldecreasing.png",
       dpi=300, width=33, height=19, units="cm")

ggplot(data=subset(agest_mortality, increasing_all>=0.8), aes(x=year, y=mortality_rate, colour=education)) + 
  geom_line() + facet_grid(rows=vars(cause), cols=vars(sex), scales="free") + 
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank()) + theme_bw() + 
  ylab("age st. mortality per 100,000") + ggtitle("all increasing")
ggsave("SIMAH_workplace/microsim/2_output_data/mortality_calibration/target_data/targets_allincreasing.png",
       dpi=300, width=33, height=19, units="cm")

ggplot(data=subset(agest_mortality, increasing_all<0.8 & increasing_all!=0), aes(x=year, y=mortality_rate, colour=education)) + 
  geom_line() + facet_grid(rows=vars(cause), cols=vars(sex), scales="free") +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank()) + theme_bw() + 
  ylab("age st. mortality per 100,000") + ggtitle("mixed trends")
ggsave("SIMAH_workplace/microsim/2_output_data/mortality_calibration/target_data/targets_mixed.png",
       dpi=300, width=33, height=19, units="cm")
