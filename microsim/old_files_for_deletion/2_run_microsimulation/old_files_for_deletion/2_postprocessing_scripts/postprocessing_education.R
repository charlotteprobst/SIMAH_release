
for(i in 2000:2018){
  PopPerYear[[paste(i)]]$year <- i
}

PopPerYear <- do.call(rbind,PopPerYear)

PopSummary <- PopPerYear %>% 
    group_by(microsim.init.sex, microsim.init.race, microsim.init.education, year) %>% tally() %>%
    ungroup() %>% mutate(year=as.integer(year),
                                            datatype="Microsimulation",
                                            race = recode(microsim.init.race,
                                                                               "BLA"="Non-Hispanic Black",
                                                                               "WHI"="Non-Hispanic White",
                                                                               "OTH"="Non-Hispanic Other",
                                                                               "SPA"="Hispanic"),
                                            sex = recode(microsim.init.sex,
                                                         "f"="Women","m"="Men"),
                                            edclass = recode(microsim.init.education,
                                                             "LEHS"="High school degree or less",
                                                             "SomeC"="Some college",
                                                             "College"="College degree or more"),
                                            edclass = factor(edclass, levels=c("High school degree or less",
                                                                               "Some college", "College degree or more"))) %>% 
  select(year, sex, race, edclass, n) %>% data.frame(.)

write.csv(PopSummary, "3_output_data/microsim_education_summary.csv", row.names=FALSE)

summary <- do.call(rbind,summary)

source("postprocessing_scripts/process_education_compare.R")

PopSummary <- PopSummary %>% select(-Sample)

summary <- summary %>% group_by(microsim.init.race, year, microsim.init.education,
                                datatype) %>% summarise(n=sum(n)) %>% ungroup() %>% 
  group_by(microsim.init.race, year, datatype) %>% mutate(percent=n/sum(n))

summary <- rbind(summary, PopSummary)

names(summary) <- c("sex","year","edclass","datatype","n","percent")
summary <- data.frame(summary)
write.csv(summary, "output_data/microsim_education_summary_10000race.csv", row.names=FALSE)

summary <- summary %>% group_by(year, datatype, edclass, sex, race) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% group_by(year, datatype,sex, race) %>% 
  mutate(percentage = n/sum(n))

plot1 <- ggplot(data=summary, aes(x=year, y=percentage, colour=as.factor(edclass),
                                  shape=as.factor(datatype), linetype=as.factor(datatype))) + geom_line() + 
  geom_point(size=2) + facet_grid(~sex+race) + ylim(0,0.7) + theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        text= element_text(size=18)) + scale_linetype_manual(values=c("ACS"="dashed","dashed","solid","dashed"))
plot1



ggsave("compare_tunnel_states2.png", plot1, dpi=300, width=33, height=19, units="cm")


summary2 <- summary %>% filter(datatype=="microsim" | datatype=="Census")

plot1 <- ggplot(data=summary2, aes(x=year, y=percentage, colour=as.factor(microsim.init.education),
                                  shape=as.factor(datatype))) + geom_line() + 
  geom_point(size=3) + facet_grid(~microsim.init.sex+microsim.init.race) + ylim(0,1) + theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        text= element_text(size=18)) + scale_shape_manual(values=c("microsim"=16,
                                                                   "Census"=17))
plot1

ggsave("compare_tunnel_statesJUSTCENSUS.png", plot1, dpi=300, width=33, height=19, units="cm")

summary2 <- summary %>% filter(datatype=="microsim" | datatype=="PSID")

plot1 <- ggplot(data=summary2, aes(x=year, y=percentage, colour=as.factor(microsim.init.education),
                                   shape=as.factor(datatype))) + geom_line() + 
  geom_point(size=3) + facet_grid(~microsim.init.sex+microsim.init.race) + ylim(0,1) + theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        text= element_text(size=18)) + scale_shape_manual(values=c("microsim"=16,
                                                                "PSID"=17))
plot1

ggsave("compare_tunnel_statesJUSTPSID.png", plot1, dpi=300, width=33, height=19, units="cm")

summary2 <- summary %>% filter(datatype=="microsim" | datatype=="ACS")

plot1 <- ggplot(data=summary2, aes(x=year, y=percentage, colour=as.factor(microsim.init.education),
                                   shape=as.factor(datatype))) + geom_line() + 
  geom_point(size=3) + facet_grid(~microsim.init.sex+microsim.init.race) + ylim(0,0.7) + theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        text= element_text(size=18)) + scale_shape_manual(values=c("microsim"=16,
                                                                   "ACS"=17))
plot1

ggsave("compare_tunnel_statesJUSTACS.png", plot1, dpi=300, width=33, height=19, units="cm")






summary <- summary %>% filter(datatype!="microsim")

plot1 <- ggplot(data=summary, aes(x=year, y=percentage, colour=as.factor(microsim.init.education),
                                  shape=as.factor(datatype))) + geom_line() + 
  geom_point(size=3) + facet_grid(~microsim.init.sex) + ylim(0,0.7) + theme_bw() + 
  theme(legend.title=element_blank()) 
plot1

ggsave("MSMtime_changedAssociate_withoutmicrosim.png", plot1, dpi=300, width=33, height=19, units="cm")
