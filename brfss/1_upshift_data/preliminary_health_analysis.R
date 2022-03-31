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
data <- readRDS("SIMAH_workplace/brfss/processed_data/BRFSS_reweighted_upshifted_1984_2020.RDS")

USA <- data %>% filter(State=="USA")

summarystats <- data %>% 
  # mutate(gpd_cat = cut(gramsperday_upshifted_crquotient,
  #                                            breaks=c(-1,0,20,40,60,200),
  #                                            labels=c("non drinker","1-20grams","21-40grams","41-60grams","61+grams"))) %>% 
group_by(YEAR, State, sex_recode, education_summary) %>% 
  summarise(meanmentalhealthdays = mean(mentalhealth,na.rm=T),
            meanphysicalhealthdays = mean(physicalhealth, na.rm=T),
            meanHED = mean(hed)) %>% drop_na() %>% 
  mutate(sex_recode=ifelse(sex_recode=="Male","Men","Women"),
         education_summary = ifelse(education_summary=="LEHS","High school or less",
                                    ifelse(education_summary=="SomeC","Some college", "College plus")),
         education_summary=factor(education_summary, levels=c("High school or less","Some college","College plus")))
  # mutate(drinkingstatus_updated = recode(drinkingstatus_updated, "0"="non-drinker","1"="drinker"))

ggplot(data=subset(summarystats,State=="USA"), aes(x=YEAR, y=meanmentalhealthdays, colour=as.factor(education_summary))) + 
  geom_line() + 
  facet_grid(cols=vars(sex_recode)) +
  ylim(0,NA) + ylab("Mean poor mental health days") +
  theme_bw() +
  theme(legend.title=element_blank(),
        strip.background=element_rect(fill="white"),
        legend.position="bottom",
        text = element_text(size=18))

ggplot(data=subset(summarystats,State=="USA"), aes(x=YEAR, y=meanphysicalhealthdays, colour=as.factor(education_summary))) + 
  geom_line() + 
  facet_grid(cols=vars(sex_recode)) +
  ylim(0,NA) + ylab("Mean poor physical health days") +
  theme_bw() +
  theme(legend.title=element_blank(),
        strip.background=element_rect(fill="white"),
        legend.position="bottom",
        text = element_text(size=18))

USA$race_eth <- factor(USA$race_eth, levels=c("White","Black","Hispanic","Other"))
USA$period <- USA$YEAR-1993
USA$drinkingcat <- ifelse(USA$gramsperday_upshifted_crquotient==0, "non-drinker",
                          ifelse(USA$gramsperday_upshifted_crquotient>0 & USA$gramsperday_upshifted_crquotient<=20 & data$sex_recode=="Female","Category I",
                                 ifelse(USA$gramsperday_upshifted_crquotient>0 & USA$gramsperday_upshifted_crquotient<=40 & data$sex_recode=="Male","Category I",
                                        ifelse(USA$gramsperday_upshifted_crquotient>20 & USA$gramsperday_upshifted_crquotient<=40 & data$sex_recode=="Female","Category II",
                                               ifelse(USA$gramsperday_upshifted_crquotient>40 & USA$gramsperday_upshifted_crquotient<=60 & data$sex_recode=="Male","Category II",
                                                      ifelse(USA$gramsperday_upshifted_crquotient>40 & data$sex_recode=="Female","Category III",
                                                             ifelse(USA$gramsperday_upshifted_crquotient>60 & data$sex_recode=="Male","Category III", NA)))))))
  
summary(as.factor(USA$drinkingcat))
USA$drinkingcat <- factor(USA$drinkingcat, levels=c("non-drinker","Category I","Category II","Category III"))
USA$education_summary <- factor(USA$education_summary, levels=c("LEHS","SomeC","College"))
model <- lm(mentalhealth ~ period + education_summary + sex_recode + race_eth + age_var + drinkingcat +
              period*education_summary + drinkingcat*education_summary, data=USA)
options(scipen=999)
summary(model)

modelsummary <- as.data.frame(model)
library(broom)
modelsummary <- tidy(model)
modelsummary$p.value <- ifelse(modelsummary$p.value<0.001, "p<.001",modelsummary$p.value)
ci <- data.frame(confint(model))
modelsummary$lower <- ci$X2.5..
modelsummary$upper <- ci$X97.5..

modelsummary <- modelsummary %>% dplyr::select(term, estimate, lower, upper, p.value)

write.csv(modelsummary, "SIMAH_workplace/brfss/despair_analysis/mentalhealth_regression.csv", row.names=F)

library(emmeans)
emm <- emmeans(model, pairwise ~ period*education_summary, level = 0.95)
summary(emm)
means <- as.data.frame(emm$emmeans)
contrasts <- as.data.frame(emm$contrasts)
eff_size(emm, sigma=sigma(model), edf=model$df.residual)

write.csv(means, "SIMAH_workplace/brfss/despair_analysis/mentalhealth_marginalmeans.csv", row.names=F)


emmip(model, drinkingcat~education_summary,
      cov.keep=3, at=list(),
      CIs = TRUE, level=0.95, position="jitter")



USA$ID <- 1:nrow(USA)




summarystats <- USA %>% 
  # mutate(gpd_cat = cut(gramsperday_upshifted_crquotient,
  #                                            breaks=c(-1,0,20,40,60,200),
  #                                            labels=c("non drinker","1-20grams","21-40grams","41-60grams","61+grams"))) %>% 
  group_by(YEAR, sex_recode, education_summary, drinkingcat) %>% 
  summarise(meanmentalhealthdays = mean(mentalhealth,na.rm=T),
            meanphysicalhealthdays = mean(physicalhealth, na.rm=T),
            meanHED = mean(hed)) %>% drop_na() %>% 
  mutate(sex_recode=ifelse(sex_recode=="Male","Men","Women"),
         education_summary = ifelse(education_summary=="LEHS","High school or less",
                                    ifelse(education_summary=="SomeC","Some college", "College plus")),
         education_summary=factor(education_summary, levels=c("High school or less","Some college","College plus")))
# mutate(drinkingstatus_updated = recode(drinkingstatus_updated, "0"="non-drinker","1"="drinker"))


ggplot(data=summarystats, aes(x=YEAR, y=meanmentalhealthdays, colour=as.factor(education_summary))) + 
  geom_line() + 
  facet_grid(rows=vars(sex_recode), cols=vars(drinkingcat)) +
  ylim(0,NA) + ylab("Mean poor mental health days") +
  theme_bw() +
  theme(legend.title=element_blank(),
        strip.background=element_rect(fill="white"),
        legend.position="bottom",
        text = element_text(size=18)) + xlim(2001,2020)

ggsave("SIMAH_workplace/brfss/despair_analysis/mentalhealthdays.png", dpi=300, width=33, height=19, units="cm")

ggplot(data=summarystats, aes(x=YEAR, y=meanphysicalhealthdays, colour=as.factor(education_summary))) + 
  geom_line() + 
  facet_grid(rows=vars(sex_recode), cols=vars(drinkingcat)) +
  ylim(0,NA) + ylab("Mean poor mental health days") +
  theme_bw() +
  theme(legend.title=element_blank(),
        strip.background=element_rect(fill="white"),
        legend.position="bottom",
        text = element_text(size=18)) + xlim(2001,2020)

ggsave("SIMAH_workplace/brfss/despair_analysis/physicalhealthdays.png", dpi=300, width=33, height=19, units="cm")

summarystats <- USA %>% 
  mutate(FMD = ifelse(mentalhealth>=14, "FMD","noFMD")) %>% 
  # mutate(gpd_cat = cut(gramsperday_upshifted_crquotient,
  #                                            breaks=c(-1,0,20,40,60,200),
  #                                            labels=c("non drinker","1-20grams","21-40grams","41-60grams","61+grams"))) %>% 
  group_by(YEAR, sex_recode, education_summary, drinkingcat, FMD) %>% 
  tally() %>% 
  ungroup() %>% group_by(YEAR, sex_recode, education_summary, drinkingcat) %>% 
  mutate(percent=n/sum(n)) %>% 
  drop_na() %>% 
  mutate(sex_recode=ifelse(sex_recode=="Male","Men","Women"),
         education_summary = ifelse(education_summary=="LEHS","High school or less",
                                    ifelse(education_summary=="SomeC","Some college", "College plus")),
         education_summary=factor(education_summary, levels=c("High school or less","Some college","College plus"))) %>% 
  filter(FMD=="FMD")
# mutate(drinkingstatus_updated = recode(drinkingstatus_updated, "0"="non-drinker","1"="drinker"))


ggplot(data=summarystats, aes(x=YEAR, y=percent, colour=as.factor(education_summary))) + 
  geom_line() + 
  facet_grid(rows=vars(sex_recode), cols=vars(drinkingcat)) +
  ylim(0,NA) + ylab("% experiencing 14+ poor mental health days per month") +
  theme_bw() +
  theme(legend.title=element_blank(),
        strip.background=element_rect(fill="white"),
        legend.position="bottom",
        text = element_text(size=18)) + xlim(2002,2020)

USA$FMD <- ifelse(USA$mentalhealth>=14, 1,0)
USA$alcoholSES <- paste(USA$education_summary, USA$drinkingcat, sep="_")
summary(as.factor(USA$alcoholSES))

USA$alcoholSES <- as.factor(USA$alcoholSES)
USA$alcoholSES <- relevel(USA$alcoholSES, ref="College_non-drinker")
USA$education_summary <- factor(USA$education_summary,
                                levels=c("LEHS","SomeC","College"))
  
USA <- USA %>% drop_na(mentalhealth)

min(USA$YEAR)
USA$periodCAT <- ifelse(USA$YEAR<=2000, '1993-2000',
                        ifelse(USA$YEAR>2000 & USA$YEAR<=2010, "2001-2010",
                               ifelse(USA$YEAR>=2011, "2011-2020", NA)))

model <- lm(FMD ~ period + education_summary + drinkingcat + 
              race_eth + age_var + drinkingcat*education_summary, data=USA)
summary(model)
exp(coef(model))

library(emmeans)
emm <- emmeans(model, pairwise ~ drinkingcat*education_summary, level = 0.95)
summary(emm)
means <- as.data.frame(emm$emmeans)
contrasts <- as.data.frame(emm$contrasts)
eff_size(emm, sigma=sigma(model), edf=model$df.residual)

emmip(model, drinkingcat~education_summary|periodCAT,
      cov.keep=3, at=list(),
      CIs = TRUE, level=0.95, position="jitter")

# reading in the PSID data 
PSIDdistress <- read.csv("SIMAH_workplace/brfss/processed_data/distress2.csv") %>% 
  mutate(sex = ifelse(sex=="male","Men","Women"),
         educationCAT = ifelse(educationCAT=="LEHS","High school or less",
                               ifelse(educationCAT=="SomeC", "Some college", "College plus")),
         educationCAT = factor(educationCAT, levels=c("High school or less","Some college","College plus")),
         racefinal = ifelse(racefinal=="white","NH-White",
                            ifelse(racefinal=="black","NH-Black",
                                   ifelse(racefinal=="hispanic","Hispanic",
                                          ifelse(racefinal=="Native","Other",
                                                 ifelse(racefinal=="other","Other",
                                                        "Other"))))))
PSIDdistress <- expandRows(PSIDdistress, "weight")

PSIDsummary <- PSIDdistress %>% group_by(year, sex, educationCAT) %>% 
  summarise(meanKessler=mean(kessler_score))

ggplot(data=PSIDsummary, aes(x=year, y=meanKessler, colour=educationCAT)) + geom_line() + 
  ylim(0,NA) + ylab("Mean Kessler score") + facet_grid(cols=vars(sex)) + 
  theme_bw() +
  theme(legend.title=element_blank(),
        strip.background=element_rect(fill="white"),
        legend.position="bottom",
        text = element_text(size=18)) + xlim(2001,2020)
ggsave("SIMAH_workplace/brfss/despair_analysis/PSIDmeanKesslerSES.png", dpi=300, width=33, height=19, units="cm")

PSIDsummary <- PSIDdistress %>% group_by(year, sex, educationCAT, distress_class) %>% 
  tally() %>% 
  ungroup() %>% group_by(year, sex, educationCAT) %>% 
  mutate(percent=n/sum(n)) %>% filter(distress_class!="Low or none")

ggplot(data=PSIDsummary, aes(x=year, y=percent, colour=educationCAT)) + geom_line() + 
  ylim(0,NA) + ylab("Percentage") + facet_grid(rows=vars(distress_class), cols=vars(sex), scales="free") + 
  theme_bw() +
  theme(legend.title=element_blank(),
        strip.background=element_rect(fill="white"),
        legend.position="bottom",
        text = element_text(size=18)) + xlim(2001,2020)
ggsave("SIMAH_workplace/brfss/despair_analysis/PSIDKesslerScoreSES.png", dpi=300, width=33, height=19, units="cm")

PSIDsummary <- PSIDdistress %>% group_by(year, racefinal, educationCAT, distress_class) %>% 
  tally() %>% 
  ungroup() %>% group_by(year, racefinal, educationCAT) %>% 
  mutate(percent=n/sum(n)) %>% filter(distress_class!="Low or none")

ggplot(data=PSIDsummary, aes(x=year, y=percent, colour=educationCAT)) + geom_line() + 
  ylim(0,NA) + ylab("Percentage") + facet_grid(rows=vars(distress_class), cols=vars(racefinal), scales="free") + 
  theme_bw() +
  theme(legend.title=element_blank(),
        strip.background=element_rect(fill="white"),
        legend.position="bottom",
        text = element_text(size=18)) + xlim(2001,2020)
ggsave("SIMAH_workplace/brfss/despair_analysis/PSIDKesslerScoreSES.png", dpi=300, width=33, height=19, units="cm")

PSIDsummary <- PSIDdistress %>% group_by(sex, educationCAT, distress_class) %>% 
  tally() %>% 
  ungroup() %>% group_by(sex, educationCAT) %>% 
  mutate(percent=n/sum(n)) %>% filter(distress_class!="Low or none")

PSIDsummary <- PSIDdistress %>% group_by(year, sex, educationCAT) %>% 
  summarise(meanKessler=mean(kessler_score))

ggplot(data=PSIDsummary, aes(x=year, y=meanKessler, colour=educationCAT)) + geom_line() + 
  ylim(0,NA) + ylab("Mean poor mental health days") + facet_grid(cols=vars(sex)) + 
  theme_bw() +
  theme(legend.title=element_blank(),
        strip.background=element_rect(fill="white"),
        legend.position="bottom",
        text = element_text(size=18)) + xlim(2001,2020)


library(lme4)

model <- lmer(mentalhealth ~ period  + education_summary + sex_recode + race_eth + age_var + gramsperday_upshifted_crquotient +
              period*education_summary + (1|region), data=data)
summary(model)

USAdropmissing <- USA %>% drop_na(mentalhealth)
USAdropmissing$period <- USAdropmissing$YEAR-min(USAdropmissing$YEAR)
USAdropmissing$anyhed <- ifelse(USAdropmissing$hed==0, 0, 1)

ggplot(data=USAdropmissing, aes(x=gramsperday_upshifted_crquotient, 
                                y=mentalhealth, colour=as.factor(education_summary))) + 
  geom_point() + 
  facet_grid(rows=vars(sex_recode)) +
  theme(legend.title=element_blank())

correlations <- USAdropmissing %>% group_by(YEAR, sex_recode, education_summary) %>% 
  summarise(correlation = cor(mentalhealth, hed))

ggplot(data=correlations, aes(x=YEAR, y=correlation, colour=education_summary)) + geom_line() + 
  facet_grid(cols=vars(sex_recode))

USAdropmissing <- USAdropmissing %>% filter(drinkingstatus_updated==1)

modelmen <- lm(gramsperday_upshifted_crquotient ~period + education_summary + 
                 education_summary*mentalhealth*period, 
               data=subset(USAdropmissing,sex_recode=="Male"))
summary(modelmen)
plot_model(modelmen, type=c("pred"), terms=c("mentalhealth","education_summary","period"))


modelmen <- lm(hed ~period + education_summary + 
                 education_summary*mentalhealth*period, 
               data=subset(USAdropmissing,sex_recode=="Male"))
summary(modelmen)
plot_model(modelmen, type=c("pred"), terms=c("mentalhealth","education_summary","period"))

modelmen <- glm(anyhed ~period + education_summary + 
                 education_summary*mentalhealth*period, 
               data=subset(USAdropmissing,sex_recode=="Male"))
summary(modelmen)
plot_model(modelmen, type=c("pred"), terms=c("mentalhealth","education_summary","period"))

modelwomen <- lm(gramsperday_upshifted_crquotient~period + education_summary + mentalhealth + education_summary*mentalhealth*period, 
               data=subset(USAdropmissing,sex_recode=="Female"))
summary(modelwomen)

modelwomen <- lm(hed~period + education_summary + mentalhealth + education_summary*mentalhealth*period, 
                 data=subset(USAdropmissing,sex_recode=="Female"))
summary(modelwomen)

plot_model(modelwomen, type=c("pred"), terms=c("mentalhealth","education_summary","period"))
