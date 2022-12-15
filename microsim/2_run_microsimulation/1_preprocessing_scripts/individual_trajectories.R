# correlations of year on year drinking 

Pops <- do.call(rbind, PopPerYear) %>% 
  dplyr::select(microsim.init.id, year, microsim.init.education, microsim.init.sex, microsim.init.alc.gpd)

YearonYear <- Pops %>% pivot_wider(names_from=year, values_from=microsim.init.alc.gpd)

cor.test(YearonYear$`2001`, YearonYear$`2002`)

cor.test(YearonYear$`2002`, YearonYear$`2003`)

cor.test(YearonYear$`2003`, YearonYear$`2004`)

cor.test(YearonYear$`2004`, YearonYear$`2005`)


sampled <- Pops %>% group_by(microsim.init.sex,
                             microsim.init.education) %>% filter(year==2000) %>% 
  sample_n(5)

selected <- Pops %>% filter(microsim.init.id %in% sampled$microsim.init.id) %>% 
  mutate(microsim.init.education=factor(microsim.init.education,
                                        levels=c("LEHS","SomeC","College")),
         microsim.init.sex = ifelse(microsim.init.sex=="m","Men","Women"))

ggplot(data=selected, aes(x=year, y=microsim.init.alc.gpd, colour=as.factor(microsim.init.id))) + geom_line(size=1) +
  facet_grid(cols=vars(microsim.init.education),
             rows=vars(microsim.init.sex)) + theme_bw() + 
  theme(legend.position="none",
        strip.background = element_rect(fill="white"),
        text = element_text(size=18)) + 
  ggtitle("Individual drinking trajectories") + ylab("grams per day (annual)")

ggsave("SIMAH_workplace/microsim/2_output_data/individual_trajectories_gpd.png", dpi=300,
       width=33, height=19, units="cm")


library(tidyverse)
library(MASS)
library(magrittr)
library(caTools)
library(party)
library(fitdistrplus)

dat <- readRDS("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_1984_2020_final.RDS") %>% filter(YEAR>=2000 & YEAR<=2019) %>% 
  filter(drinkingstatus==1) %>% filter(State=="USA") %>% ungroup() %>% 
  dplyr::select(YEAR, sex_recode, race_eth, education_summary, gramsperday) %>% 
  mutate(sex_recode=ifelse(sex_recode=="Female","Women","Men"),
         data="brfss")

distributions <- do.call(rbind, PopPerYear) %>% 
  mutate(sex_recode = ifelse(microsim.init.sex=="f","Women","Men"),
         race_eth = ifelse(microsim.init.race=="WHI","White",
                           ifelse(microsim.init.race=="BLA","Black",
                                  ifelse(microsim.init.race=="SPA","Hispanic","Other")))) %>% 
  filter(microsim.init.alc.gpd!=0) %>% 
  ungroup() %>% 
  dplyr::select(year, sex_recode, microsim.init.education, race_eth,
                microsim.init.alc.gpd) %>% 
  rename(YEAR=year, education_summary = microsim.init.education,
         gramsperday = microsim.init.alc.gpd) %>% 
  mutate(data="microsim")

assign_alc_cat <- function(data){
  data <- data %>% mutate(AlcCAT = ifelse(sex_recode=="Men" & gramsperday>0 &
                                            gramsperday<=40, "Low risk",
                                          ifelse(sex_recode=="Women" & gramsperday>0 &
                                                   gramsperday<=20, "Low risk",
                                                 ifelse(sex_recode=="Men" & gramsperday>40 &
                                                          gramsperday<=60, "Medium risk",
                                                        ifelse(sex_recode=="Women" & gramsperday>20 &
                                                                 gramsperday<=40, "Medium risk",
                                                               ifelse(sex_recode=="Men" & gramsperday>60,
                                                                      "High risk",
                                                                      ifelse(sex_recode=="Women" & gramsperday>40,
                                                                             "High risk", NA)))))))
  return(data)
}

dat <- rbind(dat, distributions) %>% 
  mutate(education_summary = factor(education_summary, 
                                    levels=c("LEHS","SomeC","College")),
         race_eth = factor(race_eth,
                           levels=c("White","Black","Hispanic","Other"))) %>% 
  assign_alc_cat() %>% 
  mutate(AlcCAT = factor(AlcCAT, levels=c("Low risk","Medium risk","High risk")))

scaleFUN <- function(x) sprintf("%.2f", x)

ggplot(data=subset(dat, YEAR==2018 & AlcCAT=="High risk" & sex_recode=="Men"), aes(x=gramsperday,
                                                             colour=data, fill=data)) + 
  geom_density(aes(color=as.factor(data), fill=as.factor(data)), alpha=0.6) +
  # geom_vline(aes(xintercept=median, color=as.factor(yearMOD)), linetype="dashed") + 
  # scale_x_continuous(limits = c(0, 200)) +  
  facet_grid(as.factor(education_summary) ~ as.factor(race_eth), scales="free_x") +
  theme(legend.position="bottom") + 
  scale_y_continuous(labels=scaleFUN) + 
  ggtitle("Men, year = 2018") + 
  theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position = "bottom",
        panel.background =element_rect(fill="white"),
        strip.background =element_rect(fill="white"),
        text = element_text(size=24))

ggsave("SIMAH_workplace/microsim/2_output_data/distributions_alc_race_ed_men.png", dpi=300,
       width=33, height=19, units="cm")

overallmeandrinking <- dat %>% 
  group_by(YEAR,sex_recode, education_summary, data) %>% 
  summarise(gramsperday = mean(gramsperday))

ggplot(data=subset(overallmeandrinking), aes(x=YEAR,y=gramsperday, colour=data, fill=data)) + 
  geom_line(size=2) + 
  # geom_vline(aes(xintercept=median, color=as.factor(yearMOD)), linetype="dashed") + 
  # scale_x_continuous(limits = c(0, 200)) +  
  facet_grid(as.factor(education_summary) ~ as.factor(sex_recode), scales="free_x") +
  theme(legend.position="bottom") + 
  scale_y_continuous(labels=scaleFUN) + 
  ggtitle("Mean drinking over time") + 
  theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position = "bottom",
        panel.background =element_rect(fill="white"),
        strip.background =element_rect(fill="white"),
        text = element_text(size=24)) + ylim(0,NA)

ggsave("SIMAH_workplace/microsim/2_output_data/meandrinkingovertime.png", dpi=300,
       width=33, height=19, units="cm")

