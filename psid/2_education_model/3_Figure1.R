# Generating Figure 1 for education transitions paper, with new TPs (including parental)
setwd("C:/Users/cmp21seb/Documents/SIMAH/")
setwd("~/Google Drive/SIMAH Sheffield")

gc()
library(ggplot2)
library(dplyr)
library(tidyr)

prob <- read.csv("SIMAH_workplace/education_transitions/final_models/income_model_TP_6cat_16_new_noint.csv")

plots <- prob %>% 
  filter(Transition=="LHS->HS" | Transition=="HS->SomeC1" | Transition=="SomeC3->College") %>%
  filter(time=="2009 - 2019") %>% 
  filter(incomequintile==1|incomequintile==5) %>%
  mutate(prob = ifelse(Transition=="LHS->HS" & age<18, NA, prob),
         prob = ifelse(Transition=="HS->SomeC1" & age<18, NA, prob),
         prob = ifelse(Transition=="SomeC3->College" & age<21, NA, prob),
         Transition = recode(Transition, "LHS->HS" = "Graduating high school",
                                          "HS->SomeC1" = "Starting college",
                                          "SomeC3->College" = "Graduating college"),
         incomequintile = recode(incomequintile, "1" = "Lowest parental income",
                                                 "5" = "Highest parental income"),
         incomequintile = factor(incomequintile, levels=c("Lowest parental income", "Highest parental income")),
         Transition = factor(Transition, levels=c("Graduating high school", "Starting college", "Graduating college")),
         racefinal = recode(racefinal, "white"="White","black"="Black","hispanic"="Hispanic",
                            "other"="Others"))

col.vec <- c("#A6D854", "#E78AC3", "#8DA0CB", "#FC8D62")

# Full sample
ggplot(data=plots, aes(x=age, y=prob, colour=racefinal, order=racefinal, linetype=racefinal)) + 
  facet_grid(cols=vars(incomequintile), rows=vars(Transition), scales="free") +
  # geom_line(size=1.5, alpha=0.8) + xlab("Age") +
  geom_smooth(se=FALSE) +
  ylab("Transition probability") + theme_bw() +
  scale_x_continuous(limits = c(18,34), breaks=c(18,20,22,24,26,28,30,32,34)) + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(colour="black",fill="white"),
        text=element_text(size=20),
        strip.text.y=element_text(size=14)) +
  scale_colour_manual(values=col.vec) +
  scale_linetype_manual(values=c("dashed","dashed","dashed","dashed"))

# Men only
male_plot <- plots %>%
  filter(sex=="male") %>%
ggplot(aes(x=age, y=prob, colour=racefinal, order=racefinal, linetype=racefinal)) + 
  facet_grid(cols=vars(incomequintile), rows=vars(Transition), scales="free") +
  geom_line(size=1.5, alpha=0.8) + xlab("Age") +
  # geom_smooth(se=FALSE) +
  ylab("Transition probability") + theme_bw() +
  ggtitle("Men") +
  scale_x_continuous(limits = c(16,34), breaks=c(16,18,20,22,24,26,28,30,32,34)) + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(colour="black",fill="white"),
        text=element_text(size=20),
        strip.text.y=element_text(size=11)) +
  scale_colour_manual(values=col.vec) +
  # scale_linetype_manual(values=c("dashed","dashed","dashed","dashed"))
  scale_linetype_manual(values=c("solid","solid","solid","solid"))
print(male_plot)

ggsave("SIMAH_workplace/education_transitions/Figure1_men.png", dpi = 300, width = 33, height = 32, units = "cm")

# Women only
female_plot <- plots %>%
  filter(sex=="female") %>%
  ggplot(aes(x=age, y=prob, colour=racefinal, order=racefinal, linetype=racefinal)) + 
  facet_grid(cols=vars(incomequintile), rows=vars(Transition), scales="free") +
  geom_line(size=1.5, alpha=0.8) + xlab("Age") +
  # geom_smooth(se=FALSE) +
  ylab("Transition probability") + theme_bw() +
  ggtitle("Women") +
  scale_x_continuous(limits = c(18,34), breaks=c(18,20,22,24,26,28,30,32,34)) + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(colour="black",fill="white"),
        text=element_text(size=20),
        strip.text.y=element_text(size=11)) +
  scale_colour_manual(values=col.vec) +
  # scale_linetype_manual(values=c("dashed","dashed","dashed","dashed"))
  scale_linetype_manual(values=c("solid","solid","solid","solid"))

print(female_plot)

ggsave("SIMAH_workplace/education_transitions/Figure1_women.png", dpi = 300, width = 33, height = 32, units = "cm")

library(gridExtra)
library(ggpubr)

Figure1 <- ggarrange(male_plot, female_plot, ncol=2, common.legend = TRUE, legend="bottom")
Figure1
ggsave("SIMAH_workplace/education_transitions/Figure1_main_nointeraction.png", Figure1, dpi = 300, width = 45, height = 30, units = "cm")

test <- prob %>% filter(age==21) %>% filter(Transition=="SomeC3->College") %>% 
  # filter(time=="2009 - 2019") %>% 
  filter(incomequintile==1 | incomequintile==5) %>% 
  pivot_wider(names_from=racefinal, values_from=prob)


