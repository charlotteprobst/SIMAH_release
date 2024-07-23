# postprocessing deaths data 
PopPerYear <- Output[[1]]
DeathSummary <- Output[[2]]
# save as 1_microsim_deaths_summary
rm(list=setdiff(ls(), c("PopPerYear", "DeathSummary", "baseorig", "proportion", "SelectedState")))
# total pop in each year
tpop <- list() 
for(i in names(PopPerYear)){
  tpop[[paste(i)]] <- PopPerYear[[paste(i)]] %>% mutate(agecat = cut(microsim.init.age,
                                                                     breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,100),
                                                                     labels=c("18-24","25-29","30-34","35-39","40-44","45-49",
                                                                              "50-54","55-59","60-64","65-69","70-74","75-79"))) %>% 
    mutate(microsim.init.sex=as.factor(microsim.init.sex),
           agecat=as.factor(agecat),
           microsim.init.education=as.factor(microsim.init.education),
           microsim.init.race = as.factor(microsim.init.race)) %>% 
    group_by(microsim.init.sex, agecat, microsim.init.education, microsim.init.race) %>% tally() %>% 
    mutate(year=paste(i),
           year=as.integer(year)) %>% rename(sex=microsim.init.sex,
                                     edclass=microsim.init.education,
                                     raceeth = microsim.init.race)
}
tpop <- do.call(rbind, tpop)
summary(tpop)
tpop$n <- tpop$n*(1/proportion)

Summary <- do.call(rbind,DeathSummary)

Summary <- Summary %>% mutate(year=as.factor(year),
                              microsim.init.sex = as.factor(microsim.init.sex),
                              microsim.init.race= as.factor(microsim.init.race),
                              microsim.init.education=as.factor(microsim.init.education),
                              agecat=as.factor(agecat),
                              cause=as.factor(cause)) %>% 
  group_by(year, microsim.init.sex, microsim.init.race, microsim.init.education, agecat, cause, .drop=FALSE) %>% summarise(totaldeaths=sum(dead),
                                                                                          totaldeaths = totaldeaths*(1/proportion)) %>% 
  mutate(datatype="microsim") %>% rename(sex=microsim.init.sex,
                                         edclass=microsim.init.education,
                                         raceeth=microsim.init.race)

# tpop$year <- as.factor(tpop$year)
# Summary <- left_join(Summary,tpop)

source("1_preprocessing_scripts/death_rates.R")

deathrates <- deathrates %>% separate(cat, sep=c(1,6,9), into=c("sex","agecat","raceeth","edclass")) %>% pivot_longer(cols=LVDCmort:RESTmort,
                                                                                                                      names_to="cause", values_to="totaldeaths") %>% 
  mutate(datatype="observed",
         cause = gsub("mort","",cause))

compare <- rbind(deathrates, Summary)

# remove age category 
compare <- compare %>% group_by(year, sex, cause, datatype) %>% summarise(totaldeaths = sum(totaldeaths)) %>% 
  mutate(cause = recode(cause, "AUD"="Alcohol use disorder", "DM"="Diabetes",
                        "HYPHD"="Hypertensive heart disease", "IHD"="Ischemic heart disease",
                        "IJ"="Suicide","ISTR"="Ischemic stroke","LVDC"="Liver disease and cirrhosis",
                        "MVACC"="Motor vehicle accidents", "REST"="All other causes", "UIJ"="Unintentional injuries"),
         cause = factor(cause, levels=c("Alcohol use disorder", "Diabetes","Hypertensive heart disease", 
                                        "Ischemic heart disease", "Ischemic stroke","Liver disease and cirrhosis",
                                        "Suicide","Motor vehicle accidents","Unintentional injuries","All other causes")),
         sex = recode(sex, "f"="Women","m"="Men"))


compare$year <- as.numeric(as.character(compare$year))
ggplot(data=compare, aes(x=year, y=totaldeaths, colour=sex, linetype=datatype)) + geom_line(alpha=0.9) + 
  facet_wrap(~cause, scales="free") + theme_bw() + ylim(0,NA) +
  scale_linetype_manual(values=c("dashed","solid")) +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        text = element_text(size=19)) + ggtitle(SelectedState) + ylab("Total deaths")

ggsave(paste("3_plots/", SelectedState, "deathscompare.png"), dpi=300, width=33, height=19, units="cm")


# now plot for different race groups

compare <- rbind(deathrates, Summary)

# remove age category 
compare <- compare %>% group_by(year, raceeth, cause, datatype) %>% summarise(totaldeaths = sum(totaldeaths)) %>% 
  mutate(cause = recode(cause, "AUD"="Alcohol use disorder", "DM"="Diabetes",
                        "HYPHD"="Hypertensive heart disease", "IHD"="Ischemic heart disease",
                        "IJ"="Suicide","ISTR"="Ischemic stroke","LVDC"="Liver disease and cirrhosis",
                        "MVACC"="Motor vehicle accidents", "REST"="All other causes", "UIJ"="Unintentional injuries"),
         cause = factor(cause, levels=c("Alcohol use disorder", "Diabetes","Hypertensive heart disease", 
                                        "Ischemic heart disease", "Ischemic stroke","Liver disease and cirrhosis",
                                        "Suicide","Motor vehicle accidents","Unintentional injuries","All other causes")),
         raceeth = recode(raceeth, "BLA"="Black","WHI"="White","SPA"="Hispanic","OTH"="Others"))

compare$year <- as.numeric(as.character(compare$year))
ggplot(data=compare, aes(x=year, y=totaldeaths, colour=raceeth, linetype=datatype)) + geom_line(alpha=0.9) + 
  facet_wrap(~cause, scales="free") + theme_bw() + ylim(0,NA) +
  scale_linetype_manual(values=c("dashed","solid")) +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        text = element_text(size=19)) + ggtitle(SelectedState) + ylab("Total deaths")
ggsave(paste("3_plots/", SelectedState, "deathscompareRACE.png"), dpi=300, width=33, height=19, units="cm")

compare <- rbind(deathrates, Summary)

# remove age category 
compare <- compare %>% group_by(year, edclass, cause, datatype) %>% summarise(totaldeaths = sum(totaldeaths)) %>% 
  mutate(cause = recode(cause, "AUD"="Alcohol use disorder", "DM"="Diabetes",
                        "HYPHD"="Hypertensive heart disease", "IHD"="Ischemic heart disease",
                        "IJ"="Suicide","ISTR"="Ischemic stroke","LVDC"="Liver disease and cirrhosis",
                        "MVACC"="Motor vehicle accidents", "REST"="All other causes", "UIJ"="Unintentional injuries"),
         cause = factor(cause, levels=c("Alcohol use disorder", "Diabetes","Hypertensive heart disease", 
                                        "Ischemic heart disease", "Ischemic stroke","Liver disease and cirrhosis",
                                        "Suicide","Motor vehicle accidents","Unintentional injuries","All other causes")),
         edclass = recode(edclass, "LEHS"="High school degree or less",
                          "SomeC"="Some college","College"="College degree or more"),
         edclass = factor(edclass, levels=c("High school degree or less","Some college","College degree or more")))

compare$year <- as.numeric(as.character(compare$year))
ggplot(data=compare, aes(x=year, y=totaldeaths, colour=edclass, linetype=datatype)) + geom_line(alpha=0.9) + 
  facet_wrap(~cause, scales="free") + theme_bw() + ylim(0,NA) +
  scale_linetype_manual(values=c("dashed","solid")) +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        text = element_text(size=19)) + ggtitle(SelectedState) + ylab("Total deaths")
ggsave(paste("3_plots/", SelectedState, "deathscompareEDUCATION.png"), dpi=300, width=33, height=19, units="cm")


comparewide <- compare %>% 
  pivot_wider(names_from=datatype, values_from=totaldeaths) %>% 
  mutate(percentdiff = (microsim-observed)/observed) %>% 
  
write.csv(Summary, "3_output_data/1_microsim_deaths_summary.csv", row.names=FALSE)

