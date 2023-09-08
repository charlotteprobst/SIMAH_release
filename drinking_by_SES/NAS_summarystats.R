
# Read in libraries
library(haven)
library(dplyr)
library(readr)
library(labelled)
library(survey)
library(tidyverse)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"

setwd(WorkingDirectory)

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

NAS <- read.csv("SIMAH_workplace/NAS/NAS.csv")

NAS <- NAS %>% 
  mutate(education = ifelse(educ1=="less than hs", "LEHS",
                            ifelse(educ1=="high school grad", "LEHS",
                                   ifelse(educ1=="some college", "SomeC",
                                          ifelse(educ1=="college grad/more","College", NA)))),
         # sex = ifelse(sex=="women", 1,0),
         alc_cat = ifelse(drkstats=="lifetime abstainers","Lifetime abstainer",
                          ifelse(drkstats=="ex-drinkers", "Former drinker",
                                 ifelse(sex=="women" & GPD <=20, "Category I",
                                        ifelse(sex=="men" & GPD<=40, "Category I",
                                               ifelse(sex=="women" & GPD>20 & GPD<=40, "Category II",
                                                      ifelse(sex=="men" & GPD>40 & GPD<=60, "Category II",
                                                             ifelse(sex=="women" & GPD>40, "Category III",
                                                                    ifelse(sex=="men" & GPD>60, "Category III",
                                                                           NA))))))))) %>% 
  dplyr::select(id_str, year, dataset, sex, age, education,
                curdrnkr, GPD, alc_cat, drkstats,
                wt_comb, psuid, stratnew, wt_all_tm)

summary(as.factor(NAS$alc_cat))


SVYobj <-  svydesign(id = ~psuid , strata = ~stratnew , weights = ~wt_all_tm, data = NAS, nest = TRUE )

summarystats <- svyby(~alc_cat, ~year+education+sex, SVYobj, svymean, na.rm=T)

alccat_summary <- summarystats %>% 
  pivot_longer(`alc_catCategory I`:`se.alc_catLifetime abstainer`) %>% 
  mutate(measure = ifelse(grepl("se", name), "SE","Mean"),
         alc_cat = gsub("se.alc_cat", "", name),
         alc_cat = gsub("alc_cat","",name),
         education = factor(education, levels=c("LEHS","SomeC","College"))) %>% 
  filter(measure=="Mean") %>% 
  dplyr::select(year, alc_cat, sex, education, value) %>% 
  pivot_wider(names_from=c(sex, education), values_from=value) %>% 
  dplyr::select(year, alc_cat, men_LEHS, men_SomeC,men_College,women_LEHS,women_SomeC,women_College)
write.csv(alccat_summary, "SIMAH_workplace/drinking_by_SES/NAS_mean_alc_cats.csv")

# now make a survey object for only drinkers and calculate grams per day 
SVYobj <-  svydesign(id = ~psuid , strata = ~stratnew , weights = ~wt_all_tm, data = subset(NAS,GPD>0), nest = TRUE )

summarystats <- svyby(~GPD, ~year+education+sex, SVYobj, svymean, na.rm=T)

gpd_summary <- summarystats %>% 
  dplyr::select(-se) %>% 
  pivot_wider(names_from=c(sex,education), values_from =GPD) %>% 
  dplyr::select(year, men_LEHS, men_SomeC,men_College,women_LEHS,women_SomeC,women_College)

write.csv(gpd_summary, "SIMAH_workplace/drinking_by_SES/NAS_mean_GPD.csv")

