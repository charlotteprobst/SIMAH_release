# SIMAH October 2021 - code to generate plots for publication
library(foreign)
library(SASxport)
library(readr)
library(dplyr)
library(tidyr)
library(labelled)
library(sjlabelled)
library(tidyverse)

# CB laptop directory
wd <- "~/Google Drive/SIMAH Sheffield/"
# CB desktop directory
# wd <- "G:/My Drive/SIMAH Sheffield/"
setwd(wd)

####read in the joined up data files 
data <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_1984_2020_paper.RDS")

# summary <- data %>% 
#   mutate(agecat = cut(age_var,
#                       breaks=c(0,24,34,44,54,64,74,100),
#                       labels=c("18-24","25-34","35-44","45-54","55-64",
#                                "65-74","75+"))) %>% 
#   filter(drinkingstatus_updated==1) %>% filter(State=="USA") %>% 
#   group_by(YEAR, sex_recode, agecat) %>% 
#   summarise(meanGPD = mean(gramsperday_upshifted_crquotient),
#             se = std.error(gramsperday_upshifted_crquotient),
#             upper = meanGPD + (1.96*se),
#             lower = meanGPD - (1.96*se))
# 
# 
# ggplot(data=summary, aes(x=YEAR, y=meanGPD, colour=agecat)) + geom_line() +
#   facet_grid(cols=vars(sex_recode))

# levels of drinkingpercentage in the different categories
drinkingcats_baseline <- data %>% 
  mutate(alcCAT = ifelse(gramsperday==0, "Abstainer",
                         ifelse(sex_recode=="Male" & gramsperday>0 &
                                  gramsperday<=40, "Category I",
                                ifelse(sex_recode=="Female" & gramsperday>0 &
                                         gramsperday<=20, "Category I",
                                       ifelse(sex_recode=="Male" & gramsperday>40 &
                                                gramsperday<=60, "Category II",
                                              ifelse(sex_recode=="Female" & gramsperday>20 &
                                                       gramsperday<=40, "Category II",
                                                     ifelse(sex_recode=="Male" & gramsperday>60 & gramsperday<=100,
                                                            "Category III", 
                                                            ifelse(sex_recode=="Female" & gramsperday>40 & gramsperday<=60, "Category III", 
                                                                   ifelse(sex_recode=="Male" & gramsperday>100, "Category IV",
                                                                          ifelse(sex_recode=="Female" & gramsperday>60, "Category IV", NA)))))))))) %>% 
  group_by(YEAR, State, sex_recode, alcCAT) %>% tally() %>% mutate(data="baseline")

drinkingcats_adjusted <- data %>% 
  mutate(alcCAT = ifelse(gramsperday_upshifted_crquotient==0, "Abstainer",
                         ifelse(sex_recode=="Male" & gramsperday_upshifted_crquotient>0 &
                                  gramsperday_upshifted_crquotient<=40, "Category I",
                                ifelse(sex_recode=="Female" & gramsperday_upshifted_crquotient>0 &
                                         gramsperday_upshifted_crquotient<=20, "Category I",
                                       ifelse(sex_recode=="Male" & gramsperday_upshifted_crquotient>40 &
                                                gramsperday_upshifted_crquotient<=60, "Category II",
                                              ifelse(sex_recode=="Female" & gramsperday_upshifted_crquotient>20 &
                                                       gramsperday_upshifted_crquotient<=40, "Category II",
                                                     ifelse(sex_recode=="Male" & gramsperday_upshifted_crquotient>60 & gramsperday_upshifted_crquotient<=100,
                                                            "Category III", 
                                                            ifelse(sex_recode=="Female" & gramsperday_upshifted_crquotient>40 & gramsperday_upshifted_crquotient<=60, "Category III", 
                                                                   ifelse(sex_recode=="Male" & gramsperday_upshifted_crquotient>100, "Category IV",
                                                                          ifelse(sex_recode=="Female" & gramsperday_upshifted_crquotient>60, "Category IV", NA)))))))))) %>% 
  group_by(YEAR, State, sex_recode, alcCAT) %>% tally() %>% mutate(data="adjusted")

drinkingcats <- rbind(drinkingcats_baseline, drinkingcats_adjusted) %>% 
  group_by(YEAR, State, sex_recode, data) %>% mutate(percent=n/sum(n)) %>% 
  filter(State=="USA") %>% filter(alcCAT!="Abstainer") %>% 
  mutate(data = ifelse(data=="baseline","Initial","Adjusted"),
         data = factor(data, levels=c("Initial","Adjusted")),
         sex_recode=ifelse(sex_recode=="Male","Men","Women")) %>% 
  dplyr::select(YEAR, State, sex_recode, alcCAT, data, percent) %>% 
  pivot_wider(names_from=data, values_from=percent) %>% 
  mutate(paired=1:n()) %>% 
  pivot_longer(cols=Initial:Adjusted) %>% mutate(name=factor(name, levels=c("Initial","Adjusted")))

Initial <- drinkingcats %>% filter(name=="Initial")
Adjusted <- drinkingcats %>% filter(name=="Adjusted")

p <- ggplot(drinkingcats) + 
  geom_segment(data=Initial, 
               aes(x=value, y=YEAR, 
                   yend = Adjusted$YEAR, xend = Adjusted$value),
               color = "grey60", size=4.5, alpha=.5) + 
  geom_point(aes(x=value, y=YEAR, color=name), size=4, show.legend=TRUE) +
  scale_colour_manual(values=c("grey50","black")) + 
  facet_grid(cols=vars(alcCAT), rows=vars(sex_recode), scales="free") +
  scale_x_continuous(labels=scales::percent_format(accuracy=1L)) + theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(fill="white"),
        text= element_text(size=18)) + xlab("Proportion") +
  ylab("Year") + scale_y_reverse()

p
ggsave("SIMAH_workplace/brfss/paper/Figure4_Categories.png", dpi=500, width=33, height=25, units="cm")

# ggplot(data=drinkingcats, aes(x=value, y=YEAR)) + geom_point(aes(colour=name), size=4) + 
#   geom_line(aes(group=paired)) +
#   facet_grid(cols=vars(alcCAT), rows=vars(sex_recode)) + theme_bw() + 
#   scale_fill_grey() + 
#   theme(legend.title=element_blank(),
#         legend.position="bottom",
#         strip.background = element_rect(fill="white")) + xlab("proportion")
# 
# ggsave("SIMAH_workplace/brfss/paper/Figure4_Categories.png", dpi=500, width=33, height=19, units="cm")

meanovertime <- rbind(drinkingcats_baseline, drinkingcats_adjusted) %>% 
  group_by(YEAR, State, sex_recode, data) %>% mutate(percent=n/sum(n)) %>% filter(State=="USA") %>% 
  ungroup() %>% group_by(sex_recode, alcCAT, data) %>% summarise(mean = round(mean(percent),3), sd=round(sd(percent),3)) %>% 
  pivot_wider(names_from=data, values_from=c(mean,sd)) %>% 
  mutate(ratio = round(mean_adjusted/mean_baseline,1)) %>% 
  dplyr::select(mean_baseline, sd_baseline, mean_adjusted, sd_adjusted, ratio)
write.csv(meanovertime, "SIMAH_workplace/brfss/paper/Table1.csv", row.names=F)

frequencycatsbaseline <- data %>% filter(State=="USA") %>% 
  mutate(freqcat = cut(alc_frequency, 
                       breaks=c(-1,0,5,10,15,20,25,31),
                       labels=c("0","1-5","6-10","11-15","16-20","21-25","26-30"))) %>% 
  group_by(YEAR,sex_recode,freqcat) %>% tally() %>% mutate(data="baseline")

frequencycatsadjusted <- data %>% filter(State=="USA") %>% 
  mutate(freqcat = cut(frequency_upshifted, 
                       breaks=c(-1,0,5,10,15,20,25,31),
                       labels=c("0","1-5","6-10","11-15","16-20","21-25","26-30"))) %>% 
  group_by(YEAR,sex_recode,freqcat) %>% tally() %>% mutate(data="adjusted")

meanovertime <- rbind(frequencycatsbaseline, frequencycatsadjusted) %>% 
  group_by(YEAR, sex_recode, data) %>% mutate(percent=n/sum(n)) %>% 
  ungroup() %>% group_by(YEAR, sex_recode, freqcat, data) %>% summarise(mean = round(mean(percent),3), sd=round(sd(percent),3)) %>% 
  pivot_wider(names_from=data, values_from=c(mean,sd)) %>% 
  mutate(ratio = round(mean_adjusted/mean_baseline,1)) %>% 
  dplyr::select(mean_baseline, sd_baseline, mean_adjusted, sd_adjusted, ratio)


write.csv(meanovertime, "SIMAH_workplace/brfss/paper/Table2.csv", row.names=F)




# calculate proportions 

malepop <- 126707757
femalepop <- 130828335

#original male 
abs(malepop*0.6/100-malepop*1.8/100)

abs(femalepop*0.2/100-femalepop*0.8/100)

