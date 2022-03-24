# processing alcohol transitions target data 
# 
# df <- Output
# 
# # for(i in 2000:2018){
# #   df[[paste(i)]]$Year <- i
# # }
# 
# df <- do.call(rbind, df)

target <- brfss %>% filter(YEAR>=2000) %>% filter(State==SelectedState) %>%
  mutate(agecat = ifelse(microsim.init.age<=29, "18-29",
                ifelse(microsim.init.age>=30 & microsim.init.age<=49,"30-49",
                       "50+"))
         # AlcCAT = ifelse(formerdrinker==1, "Former drinker", AlcCAT)
         ) %>%
  group_by(YEAR, microsim.init.sex, microsim.init.education, AlcCAT) %>%
  tally() %>% ungroup() %>% ungroup() %>% 
  mutate(totalpop=sum(n)) %>% 
  group_by(YEAR, microsim.init.sex, microsim.init.education) %>%
  mutate(targetpercent=n/sum(n),
         sepercent = sqrt((targetpercent*(1-targetpercent))/sum(n)),
         lower_ci = targetpercent - (1.96*sepercent),
         upper_ci = targetpercent + (1.96*sepercent)) %>%
  rename(year=YEAR) %>% dplyr::select(-n)

# summary <- Output %>% 
#   # mutate(agecat = cut(microsim.init.age,
#   #                                     breaks=c(0,24,34,44,54,64,100),
#   #                                     labels=c("18-24","25-34","35-44",
#   #                                              "45-54","55-64","65+"))) %>%
#   group_by(year, microsim.init.sex, AlcCAT) %>% summarise(n=sum(n)) %>%
#   mutate(data="microsimulation") %>% ungroup() %>% group_by(year, microsim.init.sex) %>%
#   mutate(proportion=n/sum(n)) %>% rename(Year=year) %>% mutate(Year=as.numeric(as.character(Year)))
# 
# summarybrfss <- brfss %>% group_by(YEAR, microsim.init.sex,
#                                    AlcCAT)%>%
#   tally() %>% rename(Year=YEAR) %>%
#   mutate(data="brfss") %>% ungroup() %>%
#   group_by(Year, microsim.init.sex) %>%
#   mutate(proportion=n/sum(n))
# # 
# summary <- rbind(summary, summarybrfss) %>%
#   mutate(AlcCAT = factor(AlcCAT,
#                          levels=c(
#                            # "Lifetime abstainer","Former drinker",
#                            "Non-drinker",
#                                   "Low risk", "Medium risk", "High risk")),
#                          microsim.init.sex = recode(microsim.init.sex,
#                                                     "m"="Men","f"="Women"))
# 
# ggplot(data=summary, aes(x=Year, y=proportion, linetype=data)) +
#   geom_line() + facet_grid(cols=vars(microsim.init.sex), rows=vars(AlcCAT), scales="free") +
#   scale_y_continuous(labels=scales::percent, limits=c(0,NA)) +   theme_bw() +
#   theme(legend.title=element_blank(),
#         text = element_text(size=12),
#         legend.position="bottom") +
#   scale_colour_brewer(type="seq", palette="Dark2")
# 
# 
# ggsave("SIMAH_workplace/microsim/2_output_data/plots/alcohol_bysex_agecat_AlcUse5.png",
#        dpi=300, width=33, height=17, units="cm")
# #   
# 
# summary <- df %>% mutate(agecat = cut(microsim.init.age,
#                                       breaks=c(0,24,34,44,54,64,100),
#                                       labels=c("18-24","25-34","35-44",
#                                                "45-54","55-64","65+"))) %>% 
#   group_by(Year, microsim.init.sex, microsim.init.race, AlcCAT) %>% tally() %>% 
#   mutate(data="microsimulation") %>% ungroup() %>% group_by(Year, microsim.init.sex, microsim.init.race) %>% 
#   mutate(proportion=n/sum(n))
# 
# summarybrfss <- brfss %>% group_by(YEAR, microsim.init.sex,
#                                    microsim.init.race,
#                                    AlcCAT)%>% 
#   tally() %>% rename(Year=YEAR) %>% 
#   mutate(data="brfss") %>% ungroup() %>% 
#   group_by(Year, microsim.init.sex, microsim.init.race) %>% 
#   mutate(proportion=n/sum(n))
# 
# summary <- rbind(summary, summarybrfss) %>% 
#   mutate(AlcCAT = factor(AlcCAT,
#                          levels=c("Lifetime abstainer","Former drinker",
#                                   "Low risk", "Medium risk", "High risk")),
#          microsim.init.sex = recode(microsim.init.sex,
#                                     "m"="Men","f"="Women"),
#          microsim.init.race = recode(microsim.init.race,
#                                      "BLA"="Black","WHI"="White",
#                                      "SPA"="Hispanic","OTH"="Others"))
# 
# ggplot(data=summary, aes(x=Year, y=proportion, colour=AlcCAT, linetype=data)) + 
#   geom_line() + facet_grid(cols=vars(microsim.init.sex),rows=vars(microsim.init.race), scales="free") + 
#   scale_y_continuous(labels=scales::percent, limits=c(0,NA)) +   theme_bw() +
#   theme(legend.title=element_blank(),
#         legend.position="bottom",
#         text = element_text(size=12)) + 
#   scale_colour_brewer(type="seq", palette="Dark2")
# ggsave("SIMAH_workplace/microsim/2_output_data/plots/alcohol_byrace.png",
#        dpi=300, width=33, height=17, units="cm")
# 
# summary <- df %>% mutate(agecat = cut(microsim.init.age,
#                                       breaks=c(0,24,34,44,54,64,100),
#                                       labels=c("18-24","25-34","35-44",
#                                                "45-54","55-64","65+"))) %>% 
#   group_by(Year, microsim.init.sex, microsim.init.education, AlcCAT) %>% tally() %>% 
#   mutate(data="microsimulation") %>% ungroup() %>% group_by(Year, microsim.init.sex, microsim.init.education) %>% 
#   mutate(proportion=n/sum(n))
# 
# summarybrfss <- brfss %>% group_by(YEAR, microsim.init.sex,
#                                    microsim.init.education,
#                                    AlcCAT)%>% 
#   tally() %>% rename(Year=YEAR) %>% 
#   mutate(data="brfss") %>% ungroup() %>% 
#   group_by(Year, microsim.init.sex, microsim.init.education) %>% 
#   mutate(proportion=n/sum(n))
# 
# summary <- rbind(summary, summarybrfss) %>% 
#   mutate(AlcCAT = factor(AlcCAT,
#                          levels=c("Lifetime abstainer","Former drinker",
#                                   "Low risk", "Medium risk", "High risk")),
#          microsim.init.sex = recode(microsim.init.sex,
#                                     "m"="Men","f"="Women"),
#          microsim.init.education = recode(microsim.init.education,
#                                      "LEHS"="High school or less",
#                                      "SomeC"="Some College",
#                                      "College"="College+"))
# 
# ggplot(data=summary, aes(x=Year, y=proportion, colour=microsim.init.education, linetype=data)) + 
#   geom_line() + facet_grid(cols=vars(microsim.init.sex),rows=vars(AlcCAT), scales="free") + 
#   scale_y_continuous(labels=scales::percent, limits=c(0,NA)) +   theme_bw() +
#   theme(legend.title=element_blank(),
#         legend.position="bottom",
#         text = element_text(size=12)) + 
#   scale_colour_brewer(type="seq", palette="Dark2")
# ggsave("SIMAH_workplace/microsim/2_output_data/plots/alcohol_byeducation.png",
#        dpi=300, width=33, height=17, units="cm")
