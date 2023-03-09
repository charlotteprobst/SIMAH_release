# plot transition probabilities for parents education

setwd("~/Google Drive/SIMAH Sheffield")
gc()
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(ggplot2)


probs <- read.csv("SIMAH_workplace/education_transitions/final_models/TP_parental_interaction.csv") %>% 
  filter(Transition=="LEHS->SomeC1" | Transition=="SomeC1->SomeC2" | Transition=="SomeC2->SomeC3" | Transition=="SomeC3->College") %>% 
  filter(model=="Interaction") %>% 
  mutate(oneCollegeplus = ifelse(oneCollegeplus==0, "no parent college +", "one parent college +"),
         sex = ifelse(sex=="male","Men","Women"),
         racefinal = ifelse(racefinal=="white","Non-Hispanic White",
                            ifelse(racefinal=="black","Non-Hispanic Black",
                                   ifelse(racefinal=="hispanic","Hispanic","Non-Hispanic others"))),
         racefinal = factor(racefinal, levels=c("Non-Hispanic White","Non-Hispanic Black","Hispanic","Non-Hispanic others")))

t1 <- ggplot(data=subset(probs,Transition=="LEHS->SomeC1" & age==18), aes(x=racefinal, y=meanprob, fill=as.factor(oneCollegeplus))) + geom_bar(stat="identity", position="dodge", colour="black") + 
  facet_grid(cols=vars(Time), rows=vars(sex)) + geom_errorbar(aes(ymin=min, ymax=max),position="dodge") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white")) +
  scale_fill_brewer(palette="Set1") +
  scale_colour_brewer(palette="Set1") + xlab("") + ylab("Transition probability (one year)") + 
  ggtitle("LEHS -> SomeC1 (18 years old)")
t1

t2 <- ggplot(data=subset(probs,Transition=="SomeC3->College" & age==21), aes(x=racefinal, y=meanprob, fill=as.factor(oneCollegeplus))) + geom_bar(stat="identity", position="dodge", colour="black") + 
  facet_grid(cols=vars(Time), rows=vars(sex)) + geom_errorbar(aes(ymin=min, ymax=max),position="dodge") + 
  theme_bw() + theme(legend.title=element_blank(),
                     legend.position="bottom",
                     strip.background = element_rect(fill="white")) +
  scale_fill_brewer(palette="Set1") +
  scale_colour_brewer(palette="Set1") + xlab("") + ylab("Transition probability (one year)") + 
  ggtitle("SomeC3 -> College (21 years old)")
t2

# line graphs 
gap <- probs %>% dplyr::select(-c(min,max)) %>% pivot_wider(names_from=oneCollegeplus, values_from=meanprob) %>% 
  mutate(gap = `one parent college +` - `no parent college +`)

l1 <- ggplot(data=subset(gap, Transition=="LEHS->SomeC1"), aes(x=age, y=gap, colour=racefinal)) + 
  geom_line() + facet_grid(cols=vars(Time), rows=vars(sex), scales="free")
l1

