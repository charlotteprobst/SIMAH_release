apply_death_rates <- function(basepop, deathrates, y){
  basepop <- basepop %>% mutate(agecat=cut(microsim.init.age,
                                           breaks=c(0,19,24,34,44,54,64,74,81),
                                           labels=c("15-19","20-24","25-34","35-44","45-54","55-64","65-74","75."))) %>% 
    mutate(cat=paste(agecat, microsim.init.sex, sep="_"))
  
  summary <- basepop %>% 
    group_by(cat) %>% 
    tally() 
  
  deathrates <- deathrates %>% filter(Year==y)
  summary <- left_join(summary, deathrates, by=c("cat"))
  summary$rate <- summary$Count/summary$n
  summary <- summary %>% dplyr::select(cat, rate)
  basepop <- left_join(basepop, summary, by=c("cat"))
  options(digits=22)
  basepop$prob <- runif(nrow(basepop))
  basepop$dies <- ifelse(basepop$prob<basepop$rate, 1,0)
  basepop <- basepop %>% filter(dies==0) %>% dplyr::select(-c(cat,prob, dies, rate))
  return(basepop)
}
