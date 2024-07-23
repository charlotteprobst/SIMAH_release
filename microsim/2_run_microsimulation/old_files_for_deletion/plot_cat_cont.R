summary <- basepop %>% 
  ungroup() %>% 
  dplyr::select(microsim.init.id, microsim.init.sex, AlcCAT, newALC) %>% 
  rename(original_cat = AlcCAT, new_cat = newALC) %>% 
  group_by(original_cat, new_cat) %>% 
  tally()

getwd()

test <- prepdata %>% filter(transition=="Low risk->Medium risk") %>% 
  ungroup() %>% 
  dplyr::select(microsim.init.id, microsim.init.sex, microsim.init.alc.gpd, newgpd) %>% 
  rename(gpd_2000=microsim.init.alc.gpd, gpd_2001=newgpd) %>% pivot_longer(gpd_2000:gpd_2001) %>%
  mutate(name=factor(name, levels=c("gpd_2000","gpd_2001")))

ids <- sample(test$microsim.init.id, 50)

test <- test %>% filter(microsim.init.id %in% ids)

ggplot(data=test, aes(x=name, y=value, colour=microsim.init.id, group=microsim.init.id)) +
  geom_line() + geom_point() + 
  facet_grid(cols=vars(microsim.init.sex)) + 
  theme_bw() + 
  theme(legend.position="none") + 
  ylab("grams per day")

ggsave("example_categorical_to_continuous.png", dpi=300, width=33, height=19, units="cm")

  