# Generating Figure 1 for education transitions paper
setwd("~/Google Drive/SIMAH Sheffield")
gc()
library(ggplot2)
library(dplyr)
library(tidyr)

prob <- read.csv("SIMAH_workplace/education_transitions/TP_2019_final.csv") %>% 
  mutate(Transition1 = recode(Transition1, "LEHS"="High school diploma or less",
                              "SomeC1"="One year of college",
                              "SomeC2"="Two years of college",
                              "SomeC3"="Three years of college",
                              "College"="College degree or more"),
         Transition2 = recode(Transition2, "LEHS"="High school diploma or less",
                              "SomeC1"="One year of college",
                              "SomeC2"="Two years of college",
                              "SomeC3"="Three years of college",
                              "College"="College degree or more"),
         transition = paste(Transition1, Transition2, sep="->"),
         sex = recode(sex, "male"="Men","female"="Women"),
         race = recode(racefinal, "white"="White","black"="Black","hispanic"="Hispanic",
                       "other"="Other","Asian"="Asian/PI")) %>% 
  rename(probability=prob,
         period=Time) %>% 
  select(period, transition,age,sex,race,probability)
write.csv(prob, "SIMAH_workplace/education_transitions/SuppTable2.csv", row.names=F)

prob <- read.csv("SIMAH_workplace/education_transitions/TP_2019_final.csv") %>% select(StateFrom, StateTo,
                                                                 prob, age, sex, racefinal, Time) %>% 
  mutate(racefinal=ifelse(racefinal=="Asian/PI","Asian",racefinal)) %>% 
  group_by(Time, age, sex, racefinal, StateFrom) %>% 
  mutate(Transition = paste(StateFrom, "->", StateTo, sep="")) %>% 
  filter(Transition=="State 1->State 2" | Transition=="State 4->State 5") %>% 
  mutate(racefinal = recode(racefinal, "white"="White",
                            "black"="Black",
                            "Asian"="Asian/PI",
                            "hispanic"="Hispanic",
                            "other"="Others"),
         racefinal = factor(racefinal, levels=c("White","Asian/PI","Hispanic","Black","Others")),
         sex = ifelse(sex=="female","Women","Men")) %>% 
  filter(Time=="2011-2019") %>% 
  mutate(prob = ifelse(Transition=="State 4->State 5" & age<21, NA, prob),
         Transition= ifelse(Transition=="State 1->State 2",
                                   "Stage 1 to Stage 2",
                                   "Stage 4 to Stage 5"))

col.vec <- c("#A6D854", "#E78AC3", "#8DA0CB", "#FC8D62", "#66C2A5")

ggplot(data=prob, aes(x=age, y=prob, colour=racefinal, order=racefinal, linetype=racefinal)) + 
  facet_grid(cols=vars(sex), rows=vars(Transition), scales="free") +
  geom_line(size=1.5, alpha=0.8) + xlab("Age") + 
  ylab("Transition probability") + theme_bw() +
  scale_x_continuous(breaks=c(18,20,22,24,26,28,30,32,34)) + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(colour="black",fill="white"),
        text=element_text(size=20),
        strip.text=element_text(size=18)) +
  scale_colour_manual(values=col.vec) +
  scale_linetype_manual(values=c("dashed","dashed","dotdash","dashed","dashed"))


ggsave("SIMAH_workplace/education_transitions/Figure1_main.pdf", dpi = 300, width = 33, height = 32, units = "cm")

