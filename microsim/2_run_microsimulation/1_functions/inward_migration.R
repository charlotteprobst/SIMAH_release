# function for adding new migrants in each year of the simulation from BRFSS pool 
inward_migration <- function(basepop, year_rates, y, brfss){
summary <- basepop %>%
  mutate(n=1,
         agecat = cut(microsim.init.age, breaks=c(0,18,24,29,34,39,44,49,54,59,
                                    64,69,74,79),
                      labels=c("18","19-24","25-29","30-34","35-39",
                               "40-44","45-49","50-54","55-59","60-64",
                               "65-69","70-74","75-79"))) %>% 
  complete(agecat, microsim.init.race, microsim.init.sex, fill=list(n=0)) %>%
  group_by(agecat, microsim.init.race, microsim.init.sex, .drop=FALSE) %>% 
  summarise(n=sum(n))

migin <- year_rates %>% filter(Year==y) %>% dplyr::select(agecat, microsim.init.sex,
                                                     microsim.init.race, MigrationInN)
summary <- left_join(summary, migin)
# convert from a rate to the N to remove 
summary <- summary %>% mutate(toadd = MigrationInN*proportion) %>% 
  dplyr::select(agecat, microsim.init.race, microsim.init.sex, toadd)

summary$cat <- paste(summary$microsim.init.sex, summary$agecat, summary$microsim.init.race, sep="_")
tojoin <- summary %>% ungroup() %>% dplyr::select(cat, toadd)

cats <- unique(tojoin$cat)
# region <- unique(brfss$region)
# y <- ifelse(y>=2019, 2019, 
#             ifelse(y==2011 & region=="division7",
#                    2010, y))
# if(region=="division7" & y>=2017){
#   pool <- brfss %>% filter(YEAR>=2017) %>% filter(microsim.init.age<=79) %>% mutate(agecat = cut(microsim.init.age, breaks=c(0,18,24,29,34,39,44,49,54,59,
#                                                                                                                           64,69,74,79),
#                                                                                               labels=c("18","19-24","25-29","30-34","35-39",
#                                                                                                        "40-44","45-49","50-54","55-59","60-64",
#                                                                                                        "65-69","70-74","75-79")),
#                                                                                  cat = paste(microsim.init.sex, agecat, microsim.init.race, sep="_"))
#   
# }else{

if(y>=2017){
  pool <- brfss %>% filter(YEAR>=2017) %>% filter(microsim.init.age<=79) %>% mutate(agecat = cut(microsim.init.age, breaks=c(0,18,24,29,34,39,44,49,54,59,
                                                                                                                          64,69,74,79),
                                                                                              labels=c("18","19-24","25-29","30-34","35-39",
                                                                                                       "40-44","45-49","50-54","55-59","60-64",
                                                                                                       "65-69","70-74","75-79")),
                                                                                 cat = paste(microsim.init.sex, agecat, microsim.init.race, sep="_"))
  
}else {pool <- brfss %>% filter(YEAR==y) %>% filter(microsim.init.age<=79) %>% mutate(agecat = cut(microsim.init.age, breaks=c(0,18,24,29,34,39,44,49,54,59,
                                                                                            64,69,74,79),
                                                                              labels=c("18","19-24","25-29","30-34","35-39",
                                                                                       "40-44","45-49","50-54","55-59","60-64",
                                                                                       "65-69","70-74","75-79")),
                                                                 cat = paste(microsim.init.sex, agecat, microsim.init.race, sep="_"))
}
brfsscats <- unique(pool$cat)
missing <- setdiff(cats, brfsscats)

toadd <- left_join(pool, tojoin) %>% filter(toadd!=0) %>% group_by(cat) %>% sample_n(toadd, replace=T) %>% 
  mutate(microsim.init.spawn.year=y) %>% ungroup() %>% 
  dplyr::select(microsim.init.age, microsim.init.race, microsim.init.sex, microsim.init.education, microsim.init.drinkingstatus,
                microsim.init.alc.gpd,
                microsim.init.income, microsim.init.spawn.year, agecat, microsimnewED, AlcCAT, formerdrinker)
microsim.init.id <- nrow(basepop)+1:nrow(toadd)+nrow(basepop)
toadd <- cbind(microsim.init.id, toadd)
basepop <- rbind(basepop, toadd)

return(basepop)
}

