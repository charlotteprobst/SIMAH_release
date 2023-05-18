# script to extract summary tables for education for education transitions paper
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

# setwd("/home/cbuckley")
setwd("~/Google Drive/SIMAH Sheffield/")
 setwd("C:/Users/cmp21seb/Documents/SIMAH/")

# data <- read_csv("SIMAH_workplace/education_transitions/new_PSID_weighted_IDs.csv") # this file doesn't seem to be used anywhere?

source("SIMAH_code/psid/2_education_model/0_generate_population.R")

TPs <- read.csv("SIMAH_workplace/education_transitions/final_models/income_model_TP_6cat_16_new_noint.csv") %>% 
  mutate(racefinal=ifelse(racefinal=="white","White",
                          ifelse(racefinal=="black","Black",
                                 ifelse(racefinal=="hispanic","Hispanic",
                                        ifelse(racefinal=="other","Others",
                                               ifelse(racefinal=="Asian/PI","Others",
                                                      racefinal))))),
         racefinal = factor(racefinal,
                            levels=c("Black","Hispanic","White","AsianPI","Others")),
         sex = ifelse(sex=="male","Men","Women")) %>% filter(incomequintile==1 | incomequintile==5) %>% 
  mutate(incomecat = ifelse(incomequintile==1, "Lowest","Highest"))

# Generate a table showing only TPs that are feasible i.e. excluding categories such as College -> HS (for supplementary info)
TPs_allowed <- TPs %>% filter(Transition=="LHS->LHS"|Transition=="LHS->HS"|
                              Transition=="HS->HS"|Transition=="HS->SomeC1"|
                              Transition=="SomeC1->SomeC1"|Transition=="SomeC1->SomeC2"|
                              Transition=="SomeC2->SomeC2"|Transition=="SomeC2->SomeC3"|
                              Transition=="SomeC3->SomeC3"|Transition=="SomeC3->College"|
                              Transition=="College->College")

write.csv(TPs_allowed, "SIMAH_workplace/education_transitions/TPs_allowed.csv")
  
population <- generate_population(TPs, 1000000) # population starts all aged 16.

# need to add incomecat to simulate_population function
simulatedpop1999 <- simulate_population(population, TPs, "1999-2009") # only up to age 26 as only simulating 20 years
simulatedpop2009<- simulate_population(population, TPs, "2009-2019") # only up to age 26 as only simulating 20 years

output <- rbind(simulatedpop1999, simulatedpop2009)

# by age 26 where have people ended up
# 6 categories instead of 5 
# Including income as a breakdown
totals <- output %>% filter(age==18 | age==21 | age==24 | age==26) %>% 
  filter(age==26)  %>%
  group_by(sex, racefinal, education, period, incomecat) %>% 
  tally(name="Npergroup") %>% ungroup() %>% group_by(sex, period, incomecat, .drop=FALSE) %>% 
  mutate(TotalN = sum(Npergroup)) %>% ungroup() %>% group_by(sex, racefinal, period, incomecat, .drop=FALSE) %>% 
  pivot_wider(names_from=education, values_from=Npergroup) %>% group_by(sex, period, racefinal, incomecat) %>%
  dplyr::select(TotalN, sex, period, racefinal, incomecat, LHS, HS, SomeC1, SomeC2, SomeC3,College) %>% 
  mutate(sumLHS = sum(c_across(`LHS`:`College`), na.rm=T), # added additional LHS category
         sumHS = sum(c_across(`HS`:`College`), na.rm=T),
         sum1YR = sum(c_across(`SomeC1`:`College`), na.rm=T),
         sum2YR = sum(c_across(`SomeC2`:`College`), na.rm=T),
         sum3YR = sum(c_across(`SomeC3`:`College`), na.rm=T),
         sumCollege = `College`,
         percentLHS = sumLHS/sumLHS,
         percentHS = sumHS/sumLHS,
         percent1yr = sum1YR/sumLHS,
         percent2yr = sum2YR/sumLHS,
         percent3yr = sum3YR/sumLHS,
         percentcollege = sumCollege/sumLHS)

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