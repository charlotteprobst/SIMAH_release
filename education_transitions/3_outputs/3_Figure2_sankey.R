# generating sankey plot for the education transitions paper 
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(ggalluvial)
library(gridExtra)

setwd("~/Google Drive/SIMAH Sheffield")
source('SIMAH_code/education_transitions/3_outputs/transition_ed.R')

# for first time period 
prob <- read.csv("SIMAH_workplace/education_transitions/TP_2019_final.csv") %>% select(StateFrom, StateTo,
                                               prob, age, sex, racefinal, Time) %>% 
  mutate(racefinal=ifelse(racefinal=="Asian/PI","Asian",racefinal)) %>% 
  group_by(Time, age, sex, racefinal, StateFrom) %>% 
  mutate(cumsum = cumsum(prob)) %>% 
  mutate(state=parse_number(StateFrom),
         cat = paste(age, sex, racefinal, "STATEFROM", state, sep="_")) %>% ungroup() %>% 
  select(cat, cumsum, Time)

data <- read_csv("SIMAH_workplace/education_transitions/PSID_reweighted_2019.csv")

summary(as.factor(data$racefinal))
# data$racefinal <- ifelse(data$racefinal=="Native","other",
#                          ifelse(data$racefinal=="Asian/PI","other",data$racefinal))
# data$racefinal <- ifelse(data$racefinal=="Native","other",data$racefinal)
# get age,sex,race
data$racefinal <- ifelse(data$racefinal=="Asian/PI","Asian",
                         ifelse(data$racefinal=="Native","other",data$racefinal))
distributions <- data %>% filter(age==18) %>% filter(year==1999 | year==2001) %>% mutate(cat=paste(sex, racefinal, sep="_")) %>% 
  group_by(cat) %>% tally() %>% ungroup() %>% mutate(percent=n/sum(n))

# sample 10,000 people from this distribution - 1999=2008
samplecats <- distributions$cat
sampleprobs <- distributions$percent
sampleprobs <- rep(1/10, times=length(samplecats))
population1 <- data.frame(sample(samplecats, 1000000, replace=T, prob=sampleprobs))
names(population1)[1] <- "cats"
population1 <- population1 %>% separate(cats, into=c("sex","race")) %>% mutate(id = 1:nrow(population1)) %>% 
  select(id, sex, race) %>% 
  mutate(age=18,
         state=1,
         education="LEHS",
         cat = paste(age,sex,race, "STATEFROM", state, sep="_"))

distributions <- data %>% filter(age==18) %>% filter(year==2011 | year==2013) %>% mutate(cat=paste(sex, racefinal, sep="_")) %>% 
  group_by(cat) %>% tally() %>% ungroup() %>% mutate(percent=n/sum(n))

# sample 10,000 people from this distribution - 1999=2008
samplecats <- distributions$cat
sampleprobs <- distributions$percent
sampleprobs <- rep(1/10, times=length(samplecats))
population2 <- data.frame(sample(samplecats, 1000000, replace=T, prob=sampleprobs))
names(population2)[1] <- "cats"
population2 <- population2 %>% separate(cats, into=c("sex","race")) %>% mutate(id = 1:nrow(population2)) %>% 
  select(id, sex, race) %>% 
  mutate(age=18,
         state=1,
         education="LEHS",
         cat = paste(age,sex,race, "STATEFROM", state, sep="_"))

# run for 16 years to simulate from age 18 to age 34
prob1 <- prob %>% filter(Time=="1999-2009") %>% select(-Time)
output1 <- list()
for(i in 1999:2009){
output1[[paste(i)]] <- population1 %>% select(id, sex, race, age, education) %>% 
  mutate(year=as.numeric(i),
         period=1)
population1$prob <- runif(nrow(population1))
population1 <- population1 %>% group_by(cat) %>% do(transition_ed(., prob1)) %>% 
  mutate(education = newED,
         state = ifelse(education=="LEHS",1,
                        ifelse(education=="SomeC1",2,
                               ifelse(education=="SomeC2",3,
                                      ifelse(education=="SomeC3",4,
                                             ifelse(education=="College",5,NA))))),
         age = age+1,
         cat = paste(age,sex,race, "STATEFROM", state, sep="_")) %>% data.frame(.)
}
prob2 <- prob %>% filter(Time=="2011-2019") %>% select(-Time)
output2 <- list()
for(i in 2011:2021){
  output2[[paste(i)]] <- population2 %>% select(id, sex, race, age, education) %>% 
    mutate(year=as.numeric(i),
           period=2)
  population2$prob <- runif(nrow(population2))
  population2 <- population2 %>% group_by(cat) %>% do(transition_ed(., prob2)) %>% 
    mutate(education = newED,
           state = ifelse(education=="LEHS",1,
                          ifelse(education=="SomeC1",2,
                                 ifelse(education=="SomeC2",3,
                                        ifelse(education=="SomeC3",4,
                                               ifelse(education=="College",5,NA))))),
           age = age+1,
           cat = paste(age,sex,race, "STATEFROM", state, sep="_")) %>% data.frame(.)
}

output1 <- do.call(rbind, output1)
output2 <- do.call(rbind, output2)
output <- rbind(output1, output2)
rm(output1, output2)
# output <- output  %>% mutate(education = ifelse(education=="SomeC1","Some college",
#                                                 ifelse(education=="SomeC2","Some college",
#                                                        ifelse(education=="SomeC3","Some college",
#                                                               ifelse(education=="LEHS","High school diploma or less",
#                                                                      ifelse(education=="College","College degree or more",
#                                                                             NA))))),
#                              education = factor(education, 
#                                                 levels=c("High school diploma or less",
#                                                          "Some college",
#                                                          "College degree or more")))

output <- output %>% mutate(education=ifelse(education=="LEHS","High school diploma or less",
                                             ifelse(education=="SomeC1", "1 year college",
                                                    ifelse(education=="SomeC2","2 year college",
                                                           ifelse(education=="SomeC3","3 year college",
                                                                  ifelse(education=="College", "College degree or more", NA))))),
                            education = factor(education, levels=c("High school diploma or less",
                                                                   "1 year college",
                                                                   "2 year college",
                                                                   "3 year college",
                                                                   "College degree or more")))

# write.csv(output, "output_equal.csv", row.names=F)
# 
# 
# output <- read_csv("output_equal.csv")
totals <- output %>% filter(age==18 | age==21 | age==24 | age==27) %>% 
  filter(age==27) %>% 
  mutate(sex = ifelse(sex=="female","Women","Men"),
         race = ifelse(race=="black","Black",
                       ifelse(race=="white","White",
                              ifelse(race=="other","Others",
                                     ifelse(race=="hispanic","Hispanic",
                                            ifelse(race=="Asian","Asian/PI",
                                            ifelse(race=="Native","Native American",NA)))))),
         race = factor(race, levels=c("White","Black","Hispanic","Native American","Asian/PI","Others"))) %>% 
  group_by(sex, race, education, period) %>% 
  tally(name="Npergroup") %>% ungroup() %>% group_by(sex, period, .drop=FALSE) %>% 
  mutate(TotalN = sum(Npergroup)) %>% ungroup() %>% group_by(sex, race, period, .drop=FALSE) %>% pivot_wider(names_from=education, values_from=Npergroup) %>% group_by(sex, period, race) %>% 
  mutate(sumHS = sum(c_across(`High school diploma or less`:`College degree or more`), na.rm=T),
         sum1YR = sum(c_across(`1 year college`:`College degree or more`), na.rm=T),
         sum2YR = sum(c_across(`2 year college`:`College degree or more`), na.rm=T),
         sum3YR = sum(c_across(`3 year college`:`College degree or more`), na.rm=T),
         sumCollege = `College degree or more`,
         percentHS = sumHS/TotalN,
         percent1yr = sum1YR/TotalN,
         percent2yr = sum2YR/TotalN,
         percent3yr = sum3YR/TotalN,
         percentcollege = sumCollege/TotalN)

HS <- totals %>% select(sex, race, period, `sum1YR`) %>% 
  mutate(percent=`sum1YR`/100000) %>% select(-c(`sum1YR`)) %>% 
  pivot_wider(names_from=race, values_from=percent) %>% mutate(Transition="HS->C1")

two <- totals %>% select(sex, race, period, `sum2YR`) %>% 
  mutate(percent=`sum2YR`/100000) %>% select(-c(`sum2YR`)) %>% 
  pivot_wider(names_from=race, values_from=percent) %>% mutate(Transition="C1->C2")

three <- totals %>% select(sex, race, period, `sum3YR`) %>% 
  mutate(percent=`sum3YR`/100000) %>% select(-c(`sum3YR`)) %>% 
  pivot_wider(names_from=race, values_from=percent) %>% mutate(Transition="C2->C3")

College <- totals %>% select(sex, race, period, `sumCollege`) %>% 
  mutate(percent=`sumCollege`/100000)%>%  select(-c(`sumCollege`)) %>% 
  pivot_wider(names_from=race, values_from=percent)%>% mutate(Transition="C3->College")

summary <- rbind(HS,two,three,College) %>% rename(Asian='Asian/PI') %>%
  pivot_wider(names_from=period,
              values_from=c("White","Black","Hispanic","Asian","Others")) %>% 
  mutate(WhiteChange = White_2-White_1,
         BlackChange = Black_2-Black_1,
         HispanicChange = Hispanic_2-Hispanic_1,
         AsianChange = Asian_2-Asian_1,
         OtherChange = Others_2-Others_1) %>% select(sex, Transition, WhiteChange, BlackChange,HispanicChange,
                                                     AsianChange, OtherChange) %>% 
  pivot_longer(cols=WhiteChange:OtherChange) %>% 
  pivot_wider(names_from=Transition, values_from=value) %>% 
  mutate(name=gsub("Change","",name))

summary <- rbind(HS,two,three,College) %>% rename(Asian='Asian/PI') %>%
  pivot_wider(names_from=period,
              values_from=c("White","Black","Hispanic","Asian","Others")) %>% 
  mutate(WhiteChange = White_2-White_1,
         BlackChange = Black_2-Black_1,
         HispanicChange = Hispanic_2-Hispanic_1,
         AsianChange = Asian_2-Asian_1,
         OtherChange = Others_2-Others_1) %>% select(sex, Transition, WhiteChange, BlackChange,HispanicChange,
                                                     AsianChange, OtherChange) %>% 
  pivot_longer(cols=WhiteChange:OtherChange) %>% 
  pivot_wider(names_from=Transition, values_from=value) %>% 
  mutate(name=gsub("Change","",name))
  
write.csv(summary, "SIMAH_workplace/education_transitions/percentage_summary_change.csv", row.names=F)

# write.csv(totals, "totals_equaldistr.csv", row.names=FALSE)

totals <- totals %>% select(sex, race, period, sumHS, sum1YR, sum2YR, sum3YR, sumCollege) %>% 
  pivot_longer(cols=c(sumHS:sumCollege), names_to="education") %>% ungroup() %>% 
  mutate(education = ifelse(education=="sumHS","High school diploma or less",
                            ifelse(education=="sum1YR", "One year of college",
                                   ifelse(education=="sum2YR", "Two years of college",
                                          ifelse(education=="sum3YR", "Three years of college",
                                                 ifelse(education=="sumCollege", "College degree or more", NA))))),
         education= factor(education, levels=c("High school diploma or less",
                                                     "One year of college",
                                                     "Two years of college",
                                                     "Three years of college",
                                                     "College degree or more")),
         period = recode(period, "1"="Period 1 (1999-2009)", "2"="Period 2 (2011-2019)"))
summary(totals$race)
totals$race <- factor(totals$race, levels=c("Native American","Others","Black","Hispanic","Asian/PI","White"))
# totals$race <- factor(totals$race, levels=c("Hispanic","others", "Black","White"))

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

col.vec <-  c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")
display.brewer.pal(5, "Set2")
scale_fill_brewer(type="qual", palette="Set2")

plot1 <- ggplot(totals, aes(x=education, stratum=race, alluvium=race, y=value,
                   fill=race, label=race)) + 
  scale_fill_manual(values=col.vec) + 
  geom_flow(stat="alluvium", lode.guidance="frontback", colour="darkgray") + theme_bw() +
  geom_stratum() + theme(legend.position="bottom",
                         legend.title=element_blank(),
                         strip.background = element_rect(colour="black", fill="white"),
                         text = element_text(size=22)) + 
  facet_grid(cols=vars(sex),rows=vars(period), scales="free") + ylab("Total population") + 
  xlab("Educational Attainment by age 27") +
  scale_x_discrete(breaks=unique(totals$education),
                   labels=(c("High school \ndiploma or less",
                                           "One year \nof college",
                                           "Two years \nof college",
                                           "Three years \nof college",
                                           "College degree \nor more"))) +
  scale_y_continuous(label=scales::comma)
  

plot1

ggsave("SIMAH_workplace/education_transitions/Figure2_main.pdf", dpi = 300, width = 45, height = 30, units = "cm")

plot2 <- ggplot(subset(sub, period==2), aes(x=age, stratum=education, alluvium=education, y=percent*100,
                         fill=education, label=education)) + 
  scale_fill_brewer(type="qual", palette="Set2") + 
  geom_flow(stat="alluvium", lode.guidance="frontback", colour="darkgray") + theme_bw() +
  geom_stratum() + theme(legend.position="bottom",
                         legend.title=element_blank(),
                         strip.background = element_rect(colour="black", fill="white"),
                         text = element_text(size=14)) + 
  facet_grid(cols=vars(sex), rows=vars(race), scales="free") + ylab("Percentage (%)") + xlab("Age") + ggtitle("Cohort 2: 2008-2017")

plot2

library(ggpubr)
ggarrange(plot1, plot2, ncol=2, common.legend=TRUE, legend="bottom")

# ggsave("SIMAH_workplace/education_transitions/sankey_v1.png", dpi=300, width=33, height=17, units="cm")

