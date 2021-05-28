# baseline alcohol consumption of population 
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)


k.wd <- c("C:/Users/Marie/Dropbox/NIH2020/Protocol_paper")
k.wd <- c("~/Google Drive/SIMAH Sheffield")
setwd(k.wd)

basepop <- read.csv("SIMAH_workplace/protocol/output_data/0_1millionbasepop.csv") %>% select(microsim.init.id, microsim.init.age, microsim.init.sex,
                                                      microsim.init.race, microsim.init.education, microsim.init.drinkingstatus,
                                                      microsim.init.alc.gpd)
# code each of the drinking categories
basepop <- basepop %>% mutate(drinkercat = ifelse(microsim.init.alc.gpd==0, "Abstainer",
                                                  ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>0 & microsim.init.alc.gpd<=40, "Low risk",
                                                         ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>40 & microsim.init.alc.gpd<=60, "Medium risk",
                                                                ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>60 & microsim.init.alc.gpd<=100, "High risk",
                                                                       ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>100, "Very high risk",
                                                                              ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>0 & microsim.init.alc.gpd<=20, "Low risk",
                                                                                     ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>20 & microsim.init.alc.gpd<=40, "Medium risk",
                                                                                            ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>40 & microsim.init.alc.gpd<=60, "High risk",
                                                                                                   ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>60, "Very high risk", NA))))))
                                                                ))),
                              microsim.init.sex = ifelse(microsim.init.sex=="m","Men","Women"),
                              microsim.init.race = recode(microsim.init.race, "BLA"="non-Hispanic Black",
                                                          "WHI"="non-Hispanic White",
                                                          "SPA"="Hispanic",
                                                          "OTH"="Other"),
                              microsim.init.education = recode(microsim.init.education,
                                                               "LEHS"="High school degree or less",
                                                               "SomeC"="Some college",
                                                               "College"="College degree or more"),
                              microsim.init.education = factor(microsim.init.education,
                                                               levels=c("High school degree or less",
                                                                        "Some college",
                                                                        "College degree or more")),
                              drinkercat = factor(drinkercat, levels=c("Abstainer","Low risk","Medium risk","High risk","Very high risk")),
                              microsim.init.race = factor(microsim.init.race, levels=c("non-Hispanic White","non-Hispanic Black",
                                                                                       "Hispanic", "Other")))
# in percentages
summary <- basepop %>% group_by(microsim.init.sex, microsim.init.education, drinkercat) %>% tally() %>% 
  ungroup() %>% group_by(microsim.init.sex, microsim.init.education) %>% 
  mutate(percent=n/sum(n)*100) 
summary$drinkercat <- fct_rev(summary$drinkercat)

col.vec <- c('#cccccc', '#93aebf','#447a9e', '#132268','#d72c40')
col.vec <- c('#d72c40', '#132268', '#447a9e','#93aebf', '#cccccc')
summary <- summary[summary$drinkercat != "Abstainer",]

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

ggplot(data=summary, aes(x=microsim.init.education, y=percent, fill=drinkercat)) + 
  geom_col(position=position_stack(reverse=T), width = 0.7 ) + 
  facet_grid(cols=vars(microsim.init.sex)) +
  theme_light() + 
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'black'), 
        text = element_text(size = 12),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12), #angle = 47, hjust=1),
        legend.position="bottom", 
        legend.title = element_blank()) +
  ylab("Percentage (%)")+ xlab("") + 
  scale_fill_manual(values=col.vec) + 
  scale_y_continuous(breaks = seq(0, 70, 10), expand=c(0,0.05), limits=c(0,70)) +
  scale_x_discrete(breaks=unique(summary$microsim.init.education), 
                   labels=addline_format(c("High school degree or less", 
                                           "Some college", "College degree or more")))

ggsave("SIMAH_workplace/protocol/output_data/0_microsim_alcohol_graph.jpeg", dpi = 600, width = 17, height = 14, units = "cm")
write.csv(summary, "output_data/alcohol use by SES and sex.csv", row.names=F)
# per 100,000 population (calculated on 1 million so divide by 10 for per 100,000)
summary <- basepop %>% group_by(microsim.init.sex, microsim.init.education, drinkercat) %>% tally() %>% 
  ungroup() %>% group_by(microsim.init.sex, microsim.init.education) %>% mutate(n=n*(1/percentpop),
                                                                                n=n/100000)
col.vec <- c('#808080', '#132268','#447a9e','#93aebf','#d72c40')

ggplot(data=summary, aes(x=microsim.init.education, y=n, fill=drinkercat)) + geom_col(position=position_stack(reverse=T)) + 
  facet_grid(cols=vars(microsim.init.sex)) +
  theme_light() + 
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'black'), 
        text = element_text(size = 14),
        axis.text = element_text(size = 12), legend.position="bottom", 
        legend.title = element_blank()) + ylab("100,000 population")+ xlab("") + 
  scale_fill_manual(values=col.vec) + scale_y_continuous(expand=c(0,0), limits=c(0,500))
ggsave("graphs/0_microsim_alcohol_graph.jpeg", dpi = 600, width = 27, height = 20, units = "cm")


