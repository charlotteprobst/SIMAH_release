# Script to view transtions over time
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(readxl)
library(doParallel)
library(foreach)
library(parallel)
library(ggplot2)
library(RColorBrewer)
library(ggalluvial)
library(patchwork)

setwd("C:/Users/cmp21seb/Documents/SIMAH/")

source("SIMAH_code/education_transitions/2021/0_transition_population_2021.R")

TPs <- read.csv("SIMAH_workplace/education_transitions/2021/annual_education_TPs_all_years_a_detail.csv") 

# Generate a column of transition
TPs <- TPs %>% mutate(
  StateFrom = case_when(
  StateFrom == "State 1" ~ "LEHS",
  StateFrom == "State 2" ~ "SomeC1",
  StateFrom == "State 3" ~ "SomeC2",
  StateFrom == "State 4" ~ "SomeC3",
  StateFrom == "State 5" ~ "College"),
  StateTo = case_when(
  StateTo == "State 1" ~ "LEHS",
  StateTo == "State 2" ~ "SomeC1",
  StateTo == "State 3" ~ "SomeC2",
  StateTo == "State 4" ~ "SomeC3",
  StateTo == "State 5" ~ "College")
) %>%
  mutate(Transition = paste(StateFrom, StateTo, sep = "->"))

# Generate a table showing only TPs that are feasible i.e. excluding categories such as College -> HS (for supplementary info)
TPs_allowed <- TPs %>% filter(Transition=="LEHS->LEHS"|Transition=="LEHS->HS"|
                              Transition=="HS->HS"|Transition=="HS->SomeC1"|
                              Transition=="SomeC1->SomeC1"|Transition=="SomeC1->SomeC2"|
                              Transition=="SomeC2->SomeC2"|Transition=="SomeC2->SomeC3"|
                              Transition=="SomeC3->SomeC3"|Transition=="SomeC3->College"|
                              Transition=="College->College")

#write.csv(TPs_allowed, "SIMAH_workplace/education_transitions/TPs_allowed_2021_model5.csv")

population <- generate_population(TPs, 1000000) # population starts all aged 18.

# Simulate the population forward based on the model with a time covariate covering all years 
simulatedpop2005 <- simulate_population(population, TPs, "1999-2005") 
simulatedpop2011 <- simulate_population(population, TPs, "2006-2011") 
simulatedpop2018 <- simulate_population(population, TPs, "2012-2018")  
simulatedpop2021 <- simulate_population(population, TPs, "2019-2021") 

simulatedpop_all_years <- rbind(simulatedpop2005, simulatedpop2011) %>%
  rbind(., simulatedpop2018) %>% 
  rbind(., simulatedpop2021)
output <- simulatedpop_all_years

## Continue from here.  

# by age 26 where have people ended up
# 6 categories instead of 5 
# Including income as a breakdown
totals <- output %>% filter(agecat=="26+")  %>%
  group_by(sex, race, education, period) %>% 
  tally(name="Npergroup") %>% ungroup() %>% group_by(sex, period, .drop=FALSE) %>% 
  mutate(TotalN = sum(Npergroup)) %>% ungroup() %>% group_by(sex, race, period, .drop=FALSE) %>% # add agecat
  pivot_wider(names_from=education, values_from=Npergroup) %>% group_by(sex, period, race) %>%
  dplyr::select(TotalN, sex, period, race, LEHS, SomeC1, SomeC2, SomeC3,College) %>% 
  mutate(sumLEHS = sum(c_across(`LEHS`:`College`), na.rm=T), 
         sum1YR = sum(c_across(`SomeC1`:`College`), na.rm=T),
         sum2YR = sum(c_across(`SomeC2`:`College`), na.rm=T),
         sum3YR = sum(c_across(`SomeC3`:`College`), na.rm=T),
         sumCollege = `College`,
         percentLEHS = sumLEHS/sumLEHS,
         percent1yr = sum1YR/sumLHS,
         percent2yr = sum2YR/sumLHS,
         percent3yr = sum3YR/sumLHS,
         percentcollege = sumCollege/sumLHS)

# extract info 
overall <- totals %>% dplyr::select(period, sex, race, sumLEHS, sum1YR, sum2YR,
                                    sum3YR, sumCollege) %>% 
  group_by(period, race,sex) %>% 
  # filter(racefinal=="black") %>% filter(incomecat=="Lowest") %>% 
  summarise(percentstartcollege = sum(sum1YR)/sum(sumLEHS),
    percentcollegegrad = sum(sumCollege)/sum(sumLEHS))%>% 
  filter(racefinal=="Black")

overall

# extract info for table 3
HS <- totals %>% select(sex, period, racefinal, incomecat, `sumHS`, `sumLHS`) %>% 
  mutate(percent=`sumHS`/`sumLHS`) %>% select(-c(`sumHS`, `sumLHS`)) %>% 
  pivot_wider(names_from=racefinal, values_from=percent) %>% mutate(Transition="LHS->HS")

one <- totals %>% select(sex, period, racefinal, incomecat, `sum1YR`, `sumLHS`) %>% 
  mutate(percent=`sum1YR`/`sumLHS`) %>% select(-c(`sum1YR`, `sumLHS`)) %>% 
  pivot_wider(names_from=racefinal, values_from=percent) %>% mutate(Transition="HS->C1")

two <- totals %>% select(sex, period, racefinal, incomecat, `sum2YR`, `sumLHS`) %>% 
  mutate(percent=`sum2YR`/`sumLHS`) %>% select(-c(`sum2YR`, `sumLHS`)) %>% 
  pivot_wider(names_from=racefinal, values_from=percent) %>% mutate(Transition="C1->C2")

three <- totals %>% select(sex, period, racefinal, incomecat, `sum3YR`, `sumLHS`) %>% 
  mutate(percent=`sum3YR`/`sumLHS`) %>% select(-c(`sum3YR`, `sumLHS`)) %>% 
  pivot_wider(names_from=racefinal, values_from=percent) %>% mutate(Transition="C2->C3")

college <- totals %>% select(sex, period, racefinal, incomecat, `sumCollege`, `sumLHS`) %>% 
  mutate(percent=`sumCollege`/`sumLHS`) %>% select(-c(`sumCollege`, `sumLHS`)) %>% 
  pivot_wider(names_from=racefinal, values_from=percent) %>% mutate(Transition="C3->College")


summary <- rbind(HS,one,two,three,college) %>%
  mutate(period=ifelse(period=="1999-2009",1,2)) %>% 
  pivot_wider(names_from=period,
              values_from=c("White","Black","Hispanic","Others")) %>% 
  mutate(WhiteChange = White_2-White_1,
         BlackChange = Black_2-Black_1,
         HispanicChange = Hispanic_2-Hispanic_1,
         OtherChange = Others_2-Others_1) %>% select(sex, Transition, WhiteChange, BlackChange,HispanicChange,
                                                     OtherChange) %>% 
  pivot_longer(cols=WhiteChange:OtherChange) %>% 
  pivot_wider(names_from=Transition, values_from=value) %>% 
  mutate(name=gsub("Change","",name))

write.csv(summary, "SIMAH_workplace/education_transitions/Table3_percentage_summary_change_noint.csv", row.names=F)

totals_n <- totals %>% select(sex, racefinal, period, incomecat, sumLHS, sumHS, sum1YR, sum2YR, sum3YR, sumCollege) %>% 
  pivot_longer(cols=c(sumLHS:sumCollege), names_to="education") %>% ungroup() %>% 
  mutate(education = ifelse(education=="sumLHS", "Less than High school diploma",
                        ifelse(education=="sumHS","High school diploma",
                            ifelse(education=="sum1YR", "One year of college",
                                   ifelse(education=="sum2YR", "Two years of college",
                                          ifelse(education=="sum3YR", "Three years of college",
                                                 ifelse(education=="sumCollege", "College degree or more", NA)))))),
         education= factor(education, levels=c("Less than High school diploma",
                                               "High school diploma",
                                               "One year of college",
                                               "Two years of college",
                                               "Three years of college",
                                               "College degree or more")),
         period = recode(period, "1"="Period 1 (1999-2009)", "2"="Period 2 (2011-2019)")) 
summary(as.factor(totals_n$racefinal))
totals_n$racefinal <- factor(totals_n$racefinal, levels=c("Native American","Others","Black","Hispanic","Asian/PI","White"))
# totals_n$race <- factor(totals_n$race, levels=c("Hispanic","others", "Black","White"))
totals_n$incomecat <- factor(totals_n$incomecat, levels=c("Lowest", "Highest"), labels=(c("Lowest income cat.", "Highest income cat.")))

# Figure 2: Total number of simulated individuals at each level of education by age 26.

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

col.vec <-  c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")
display.brewer.pal(5, "Set2")
scale_fill_brewer(type="qual", palette="Set2")
totals_n$period <- as.factor(totals_n$period)
totals_n$sex <- as.factor(totals_n$sex)

# check alluvia form - correct 
is_alluvia_form(as.data.frame(totals_n), axes = 1:6, silent = TRUE)

# Low income
plot_lowest_income <- ggplot(subset(totals_n,incomecat=="Lowest income cat."), 
                         aes(x=education, stratum=racefinal, alluvium=racefinal, y=value,
                             fill=racefinal)) + 
  scale_fill_manual(values=col.vec) + 
  geom_flow(stat="alluvium", lode.guidance="frontback", colour="darkgray") + theme_bw() +
  geom_stratum() + theme(legend.position="none",
                         strip.background = element_rect(colour="black", fill="white"),
                         strip.text.y = element_blank(),
                         text = element_text(size=10),
                         axis.text.x = element_text(angle=90)) +
                         facet_grid(cols=vars(period), rows=vars(sex)) + ylab("Population count") + 
  xlab("Educational attainment") +
  scale_x_discrete(breaks=unique(totals_n$education),
                   labels=(c("Less than \nHS diploma",
                             "HS diploma",
                             "One year \nof college",
                             "Two years \nof college",
                             "Three years \nof college",
                             "College grad. \nor more"))) +
  scale_y_continuous(breaks = seq(0, 260000, by=50000), limits=c(0,260000),
                     labels= scales::comma
                     ) +
  ggtitle("Lowest income") 
plot_lowest_income

# Highest income
plot_highest_income <- ggplot(subset(totals_n,incomecat=="Highest income cat."), 
                         aes(x=education, stratum=racefinal, alluvium=racefinal, y=value,
                             fill=racefinal)) + 
  labs(fill='Race and ethnicity') +
  scale_fill_manual(values=col.vec) + 
  geom_flow(stat="alluvium", lode.guidance="frontback", colour="darkgray") + theme_bw() +
  geom_stratum() + theme(legend.position="right",
                         strip.background = element_rect(colour="black", fill="white"),
                         text = element_text(size=10),
                         axis.text.x = element_text(angle=90),
                         axis.text.y = element_blank(),
                         axis.title.y=element_blank(),
                         axis.ticks.y = element_blank()) + 
  facet_grid(cols=vars(period), rows=vars(sex)) +
  xlab("Educational attainment") +
  scale_x_discrete(breaks=unique(totals_n$education),
                   labels=(c("Less than \nHS diploma",
                             "HS diploma",
                             "One year \nof college",
                             "Two years \nof college",
                             "Three years \nof college",
                             "College grad. \nor more"))) +
  scale_y_continuous(breaks = seq(0, 260000, by=50000), limits=c(0,260000),
                     labels=scales::comma) + 
          ggtitle("Highest income") 
plot_highest_income

Figure_2_counts <- plot_lowest_income + plot_highest_income + plot_annotation(
  title = 'Educational attainment at age 26',
  subtitle = 'Split by time parental income, time period, sex, and race and ethnicity',
  caption = 'Note: Based on 1,000,000 simulated individuals (starting population counts by group vary slightly)')

plot(Figure_2_counts)
ggsave("SIMAH_workplace/education_transitions/Figure2_counts_nointeraction.png", dpi = 300, width = 45, height = 30, units = "cm")


# As percentage of population ??

# plot_men+percent <- ggplot(subset(totals,sex=="Men"), aes(x=education, stratum=racefinal, alluvium=racefinal, y=value,
#                                                   fill=racefinal, label=racefinal)) + 
#   scale_fill_manual(values=col.vec) + 
#   geom_flow(stat="alluvium", lode.guidance="frontback", colour="darkgray") + theme_bw() +
#   geom_stratum() + theme(legend.position="bottom",
#                          legend.title=element_blank(),
#                          strip.background = element_rect(colour="black", fill="white"),
#                          text = element_text(size=10)) + 
#   facet_grid(cols=vars(incomecat), rows=vars(period), scales="free") + ylab("Total population") + 
#   xlab("Educational Attainment by age 26") +
#   scale_x_discrete(breaks=unique(totals$education),
#                    labels=(c("Less than \nHS diploma",
#                              "HS \ndiploma",
#                              "One year \nof college",
#                              "Two years \nof college",
#                              "Three years \nof college",
#                              "College grad. \nor more"))) +
#   scale_y_continuous(label=scales::comma)
# plot_men_percent