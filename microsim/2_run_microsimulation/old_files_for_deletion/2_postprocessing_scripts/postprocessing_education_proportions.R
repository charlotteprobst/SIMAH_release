# proportion of people aged 25 who have moved to further education (i.e. some college) or have graduated college
educationsummary <- list()
for(i in names(PopPerYear)){
educationsummary[[paste(i)]] <- PopPerYear[[paste(i)]] %>% filter(microsim.init.id<=1000 | microsim.init.id>=1500000) %>% group_by(microsim.init.id, microsim.init.age, microsim.init.race,
                                            microsim.init.sex, microsim.init.education) %>% tally() %>% 
  mutate(year=as.numeric(paste(i)))
}


educationsummary <- do.call(rbind, educationsummary)

educationsummary <- educationsummary %>% ungroup() %>% select(microsim.init.id, year, microsim.init.education) %>% pivot_wider(names_from=year, values_from=microsim.init.education)



summary <- list() 
for(i in names(PopPerYear)){
  summary[[paste(i)]] <- PopPerYear[[paste(i)]] %>% mutate(year=as.numeric(paste(i)))
}

do.call(rbind, summary)


summary <- educationsummary %>% mutate(yeargroup = ifelse(year<=2005, 1,
                                                          ifelse(year>2005 & year<=2011, 2,
                                                                 ifelse(year>2011,3,NA)))) %>% 
  group_by(microsim.init.race, microsim.init.sex, microsim.init.education, yeargroup) %>% 
  summarise(total = sum(n)) %>% filter(microsim.init.sex=="m" & microsim.init.race=="BLA") %>% ungroup() %>% 
  select(-c(microsim.init.sex, microsim.init.race))

summary %>% pivot_wider(names_from=microsim.init.education, values_from=yeargroup)


options(digits=2)
library(ggalluvial)
# sankey diagram

is_alluvia_form(summary, axes=1:3, silent=TRUE)

ggplot(data=summary, aes(y=total, axis1=microsim.init.race)) + geom_alluvium(aes(fill=microsim.init.education))
