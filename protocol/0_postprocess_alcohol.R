# SIMAH - protocol paper. June 2021
# This code reads in the baseline population and generates a Figure for the 
# protocol paper showing drinking patterns in the baseline population

# baseline alcohol consumption of population 
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(readr)
library(plotrix)


k.wd <- c("C:/Users/Marie/Dropbox/NIH2020/")
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

# read in upshifted BRFSS data - commented out because file so big
# read in summary BRFSS file below instead
# brfss <- read_csv("SIMAH_workplace/protocol/output_data/BRFSS_upshift_BMIUSA.csv") %>% 
#   filter(YEAR==2000 & AGE<=79) %>% select(SEX, EDUCATION, alcgpd_new, DRINKINGSTATUS_NEW) %>% 
#   mutate(drinkercat = ifelse(alcgpd_new==0, "Abstainer",
#                              ifelse(SEX=="Male" & alcgpd_new>0 & alcgpd_new<=40, "Low risk",
#                                     ifelse(SEX=="Male" & alcgpd_new>40 & alcgpd_new<=60, "Medium risk",
#                                            ifelse(SEX=="Male" & alcgpd_new>60 & alcgpd_new<=100, "High risk",
#                                                   ifelse(SEX=="Male" & alcgpd_new>100, "Very high risk",
#                                                          ifelse(SEX=="Female" & alcgpd_new>0 & alcgpd_new<=20, "Low risk",
#                                                                 ifelse(SEX=="Female" & alcgpd_new>20 & alcgpd_new<=40, "Medium risk",
#                                                                        ifelse(SEX=="Female" & alcgpd_new>40 & alcgpd_new<=60, "High risk",
#                                                                               ifelse(SEX=="Female" & alcgpd_new>60, "Very high risk", NA))))))
#                                     ))),
#          microsim.init.sex = ifelse(SEX=="Female","Women","Men"),
#          microsim.init.education = ifelse(EDUCATION==1, "High school degree or less",
#                                           ifelse(EDUCATION==2, "Some college",
#                                                  ifelse(EDUCATION==3, "College degree or more", NA))),
#          microsim.init.education = factor(microsim.init.education,
#                                           levels=c("High school degree or less",
#                                                    "Some college",
#                                                    "College degree or more")),
#          drinkercat = factor(drinkercat, levels=c("Abstainer","Low risk","Medium risk","High risk","Very high risk")))

# in percentages
summary <- basepop %>% group_by(microsim.init.sex, microsim.init.education, drinkercat) %>% tally() %>% 
  ungroup() %>% group_by(microsim.init.sex, microsim.init.education) %>% 
  mutate(percent=n/sum(n)*100) 
summary$drinkercat <- fct_rev(summary$drinkercat)

# summarise brfss data - mean prevalence by education and sex
# summarybrfss <- brfss %>% group_by(microsim.init.sex, microsim.init.education) %>% 
#   mutate(DRINKINGSTATUS_NEW = ifelse(alcgpd_new==0, 0, DRINKINGSTATUS_NEW),
#          prevalence=mean(DRINKINGSTATUS_NEW)*100,
#          se = std.error(DRINKINGSTATUS_NEW)*100) %>% 
#   select(drinkercat, microsim.init.sex, microsim.init.education, prevalence, se) %>% 
#   distinct() %>% filter(drinkercat!="Abstainer")
# write.csv(summarybrfss, "SIMAH_workplace/protocol/output_data/summarybrfss.csv", row.names=FALSE)

# read in the processed brfss data - to save time associated with reading full BRFSS
summarybrfss <- read.csv("SIMAH_workplace/protocol/output_data/summarybrfss.csv")

summary <- left_join(summary, summarybrfss)
summary$microsim.init.education <- factor(summary$microsim.init.education, 
                                          levels=c("High school degree or less",
                                                   "Some college",
                                                   "College degree or more"))

col.vec <- c('#cccccc', '#93aebf','#447a9e', '#132268','#d72c40')
col.vec <- c('#d72c40', '#132268', '#447a9e','#93aebf', '#cccccc')
summary <- summary[summary$drinkercat != "Abstainer",]

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

# plot graph
ggplot(data=summary, aes(x=microsim.init.education, y=percent, fill=drinkercat)) + 
  geom_col(position=position_stack(reverse=T), width = 0.7 ) + 
  geom_point(aes(x=microsim.init.education, y=prevalence, colour="black"), fill=NA) + 
  geom_errorbar(aes(x=microsim.init.education, ymin=prevalence-se*1.96, ymax=prevalence+se*1.96), width=0.5) +
  facet_grid(cols=vars(microsim.init.sex)) +
  theme_light() + 
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'black'), 
        text = element_text(size = 12),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12), #angle = 47, hjust=1),
        legend.position="bottom", 
        legend.title = element_blank()) +
  ylab("Prevalence (%)")+ xlab("") + 
  scale_fill_manual(values=col.vec) + 
  scale_y_continuous(breaks = seq(0, 70, 10), expand=c(0,0.05), limits=c(0,85)) +
  scale_x_discrete(breaks=unique(summary$microsim.init.education), 
                   labels=addline_format(c("High school degree or less", 
                                           "Some college", "College degree or more"))) + 
  scale_colour_manual(values="black", labels="BRFSS")

ggsave("SIMAH_workplace/protocol/graphs/0_microsim_alcohol_graph.jpeg", dpi = 600, width = 17, height = 14, units = "cm")
write.csv(summary, "SIMAH_workplace/protocol/output_data/0_alcohol_use_by_SES_and_sex.csv", row.names=F)

