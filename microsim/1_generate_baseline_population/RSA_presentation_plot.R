# generating plot for RSA presentation

# baseline alcohol consumption of population 
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(readr)
library(plotrix) 

k.wd <- c("~/Google Drive/SIMAH Sheffield")
setwd(k.wd)

basepop <- read.csv("SIMAH_workplace/microsim/1_input_data/agent_files/USAbasepop1000000.csv") %>% dplyr::select(microsim.init.age, microsim.init.sex,
                                                                                                                 microsim.init.race, microsim.init.education, microsim.init.drinkingstatus,
                                                                                                                 microsim.init.alc.gpd) %>% 
  mutate(microsim.init.education = recode(microsim.init.education, "LEHS"="High school degree or less",
                                          "SomeC"="Some college","College"="College degree or more"),
         microsim.init.sex = recode(microsim.init.sex, "m"="Men","f"="Women"))

brfss <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_states_upshifted.RDS") %>% 
  filter(age_var<=79) %>% filter(YEAR==1999 | YEAR==2000 | YEAR==2001) %>% 
  filter(State=="USA") %>% 
  mutate(microsim.init.sex = recode(sex_recode,"Men"="m","Women"="f"),
         microsim.init.education = education_summary,
         microsim.init.alc.gpd = gramsperday,
         drinkercat = ifelse(microsim.init.alc.gpd==0, "Abstainer",
                             ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>0 & microsim.init.alc.gpd<=40, "Category I",
                                    ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>40 & microsim.init.alc.gpd<=60, "Category II",
                                           ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>60 & microsim.init.alc.gpd<=100, "Category III",
                                                  ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>100, "Category IV",
                                                         ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>0 & microsim.init.alc.gpd<=20, "Category I",
                                                                ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>20 & microsim.init.alc.gpd<=40, "Category II",
                                                                       ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>40 & microsim.init.alc.gpd<=60, "Category III",
                                                                              ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>60, "Category IV", NA))))))))),
         microsim.init.sex = recode(microsim.init.sex, "m"="Men","f"="Women"),
         microsim.init.education = recode(microsim.init.education, "LEHS"="High school degree or less",
                                          "SomeC"="Some college",
                                          "College" = "College degree or more"))


summarybasepop <- basepop %>% group_by(microsim.init.education,microsim.init.sex) %>% 
  tally() %>% ungroup() %>% group_by(microsim.init.sex) %>% mutate(percent=n/sum(n)) %>% mutate(type="Synthetic population")


summarybrfss <- brfss %>% drop_na(microsim.init.education) %>% group_by(microsim.init.education, microsim.init.sex) %>% 
  tally() %>% ungroup() %>% group_by(microsim.init.sex) %>% mutate(percent=n/sum(n)) %>% mutate(type="BRFSS data")

compare <- rbind(summarybasepop, summarybrfss) %>% 
  mutate(microsim.init.education = factor(microsim.init.education, levels=c("High school degree or less","Some college",
                                                                            "College degree or more")),
         microsim.init.sex = ifelse(microsim.init.sex=="Female","Women",
                                    ifelse(microsim.init.sex=="Male","Men",microsim.init.sex))) %>% filter(microsim.init.education=="High school degree or less")
col.vec <- c('#cccccc', '#93aebf','#447a9e', '#132268','#d72c40')

ggplot(data=compare, aes(x=microsim.init.sex, y=percent, fill=type)) + geom_bar(stat="identity", position="dodge", colour="black") + 
  theme_bw() + xlab("") + ylab("Proportion") +
  scale_fill_manual(values=c("#93aebf","#132268")) +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        text = element_text(size=18),
        strip.background = element_rect(fill="white")) + scale_y_continuous(labels=scales::percent, limits=c(0,0.6))
ggsave("SIMAH_workplace/RSA/baseline_plot.png", dpi=300, width=33, height=14, units="cm")


summarybasepop <- basepop %>% mutate(over60 = ifelse(microsim.init.alc.gpd>=60, 1,0)) %>% 
  group_by(microsim.init.sex, over60) %>% 
  tally() %>% ungroup() %>% 
  group_by(microsim.init.sex) %>% mutate(percent = n/sum(n)) %>% mutate(type="Synthetic population")

summarybrfss <- brfss %>% drop_na(microsim.init.education) %>% mutate(over60 = ifelse(microsim.init.alc.gpd>=60, 1,0)) %>% 
  group_by(microsim.init.sex, over60) %>% 
  tally() %>% ungroup() %>% 
  group_by(microsim.init.sex) %>% mutate(percent = n/sum(n)) %>% mutate(type="BRFSS data")

compare <- rbind(summarybasepop, summarybrfss) %>% 
  mutate(microsim.init.sex = ifelse(microsim.init.sex=="Female","Women",
                                    ifelse(microsim.init.sex=="Male","Men",microsim.init.sex)))


                                                           