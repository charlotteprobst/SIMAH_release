# extracting transition probabilities from the parental model versions 
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(readxl)

setwd("~/Google Drive/SIMAH Sheffield")
modelst1_interaction <- read_rds("SIMAH_workplace/education_transitions/final_models/t1_parent.RDS")
modelst2_interaction <- read_rds("SIMAH_workplace/education_transitions/final_models/t2_parent.RDS")

source("SIMAH_code/education_transitions/2_analysis/extractTP_function.R")

data$agescaled <- scale(data$age, center=T)
data$agesqscaled <- scale((data$age^2), center=T)

mapping <- data %>% ungroup() %>% select(age, agescaled, agesqscaled) %>% 
  distinct()

combo <- expand.grid(age = unique(data$age), sex = c(0,1), racefinal2=unique(data$racefinal2),
                     oneCollegeplus=unique(data$oneCollegeplus))

TP_t1 <- lapply(modelst1_interaction, extractTP, combo=combo, mapping=mapping)
T1 <- TP_t1 %>% bind_rows() %>% 
  mutate(time="1999-2009",
         age = as.numeric(as.character(age))) %>% 
  group_by(time,age, sex, racefinal, oneCollegeplus, Transition, Transition1, Transition2) %>% 
  summarise(mean = mean(prob),
            min=min(prob),
            max=max(prob))
TP_t2 <- lapply(modelst2_interaction, extractTP, combo=combo, mapping=mapping)
T2<- TP_t2 %>% bind_rows() %>% 
  mutate(time="2011-2019",
         age = as.numeric(as.character(age))) %>% 
  group_by(time,age, sex, racefinal, oneCollegeplus, Transition, Transition1, Transition2) %>% 
  summarise(mean = mean(prob),
            min=min(prob),
            max=max(prob))

TP <- rbind(T1, T2) %>% 
  filter(Transition=="LEHS->SomeC1") %>% filter(racefinal!="Native")

ggplot(data=subset(TP,sex=="female"), aes(x=age, y=mean, colour=as.factor(oneCollegeplus))) + geom_line() + 
  facet_grid(rows=vars(racefinal), cols=vars(time)) +
  geom_ribbon(aes(ymin=min, ymax=max, fill=as.factor(oneCollegeplus)),colour=NA, alpha=0.5)

ggplot(data=subset(TP,sex=="female"), aes(x=age, y=mean, colour=time)) + geom_line() + 
  facet_grid(cols=vars(oneCollegeplus), rows=vars(racefinal)) +
  geom_ribbon(aes(ymin=min, ymax=max, fill=time, alpha=0.5),colour=NA)

for(i in 1:length(TP_t1)){
  TP_t1[[paste(i)]]$imp <- i
  TP_t2[[paste(i)]]$imp <- i
}

T1 <- TP_t1 %>% bind_rows() %>% 
  mutate(time="1999-2009",
         age = as.numeric(as.character(age))) %>% 
  dplyr::select(imp,time,age,sex,racefinal,oneCollegeplus, Transition, prob) %>% ungroup() %>% 
  pivot_wider(names_from=oneCollegeplus, values_from=prob) %>% 
  mutate(fct_increase = `1`/`0`) %>% ungroup() %>% 
  group_by(time, age, sex, racefinal, Transition) %>% 
  summarise(mean = mean(fct_increase),
            min = min(fct_increase),
            max = max(fct_increase))

T2 <- TP_t2 %>% bind_rows() %>% 
  mutate(time="2011-2019",
         age = as.numeric(as.character(age))) %>% 
  dplyr::select(imp,time,age,sex,racefinal,oneCollegeplus, Transition, prob) %>% ungroup() %>% 
  pivot_wider(names_from=oneCollegeplus, values_from=prob) %>% 
  mutate(fct_increase = `1`/`0`) %>% ungroup() %>% 
  group_by(time, age, sex, racefinal, Transition) %>% 
  summarise(mean = mean(fct_increase),
            min = min(fct_increase),
            max = max(fct_increase))

reporting <- rbind(T1, T2) %>% 
  filter(Transition=="LEHS->SomeC1" | Transition=="SomeC1->SomeC2" | Transition=="SomeC2->SomeC3" | Transition=="SomeC3->College") %>% 
  mutate(select = ifelse(Transition=="LEHS->SomeC1" & age==18, 1,
                         ifelse(Transition=="SomeC1->SomeC2" & age==19, 1,
                                ifelse(Transition=="SomeC2->SomeC3" & age==20, 1,
                                       ifelse(Transition=="SomeC3->College" & age==21, 1, 0))))) %>% 
  filter(racefinal=="white" | racefinal=="black" | racefinal=="hispanic") %>% 
  filter(select==1) %>% 
  filter(racefinal!="Native") %>% filter(racefinal!="other") %>% 
  ungroup() 

test <- reporting %>% filter(Transition=="SomeC3->College") %>% filter(racefinal=="white")

ggplot(data=reporting, aes(x=mean, y=Transition, colour=time)) + geom_point(position=position_dodge(width=-0.5), size=1) + 
  geom_errorbar(aes(xmin=min, xmax=max), position=position_dodge(width=-0.5))+
  facet_grid(cols=vars(sex), rows=vars(racefinal)) + 
  scale_y_discrete(limits=rev) +
  scale_colour_manual(values=c("grey70","black")) + xlab("Ratio of parents with college degree / no college degree") +
  theme_bw() +
  theme(legend.title=element_blank(),
        strip.background=element_rect(fill="white"),
        text = element_text(size=18))
# proportion of people that have at least 1 college educated parent over time, by sex and race and ethnicity 

descriptives <- data %>% 
  mutate(period = ifelse(year<=2009, "1999-2009","2011-2019")) %>% 
  group_by(period, racefinal2, oneCollegeplus) %>% 
  tally() %>% ungroup() %>% 
  group_by(period, racefinal2) %>%
  mutate(percent=n/sum(n)) %>% 
  ungroup() %>% 
  filter(oneCollegeplus==1) %>% 
  dplyr::select(period, racefinal2,percent) %>% 
  pivot_wider(names_from=period, values_from=percent)
