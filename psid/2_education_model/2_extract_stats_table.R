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

# setwd("/home/cbuckley")
setwd("~/Google Drive/SIMAH Sheffield")

source("SIMAH_code/psid/2_education_model/1_setup_markov_model.R")

data <- read_csv("SIMAH_workplace/education_transitions/new_PSID_weighted_IDs.csv")

# do the first analysis on the split time periods 

# # setup the datasets for both time periods
# 
datat1 <- setup_markov_model(data, y=2009)
datat2 <- setup_markov_model(data, y=2011)

# modelt1_baseline <- readRDS("SIMAH_workplace/education_transitions/final_models/modelt1_baseline_6cat.RDS")
# modelt1_income <- readRDS("SIMAH_workplace/education_transitions/final_models/modelt1_income_6cat.RDS")
modelt1_income_int <- readRDS("SIMAH_workplace/education_transitions/final_models/modelt1_income_int_6cat_16.RDS")

# modelt2_baseline <- readRDS("SIMAH_workplace/education_transitions/final_models/modelt2_baseline_6cat.RDS")
# modelt2_income <- readRDS("SIMAH_workplace/education_transitions/final_models/modelt2_income_6cat.RDS")
modelt2_income_int <- readRDS("SIMAH_workplace/education_transitions/final_models/modelt2_income_int_6cat_16.RDS")

# datat1baseline <- extract_coefficients_6cat(modelt1_baseline, "baseline", "1999-2009", datat1)
# datat1income <- extract_coefficients_6cat(modelt1_income, "income", "1999-2009", datat1)
datat1incomeint <- extract_coefficients_6cat(modelt1_income_int, "incomeint", "1999-2009", datat1)

# datat2baseline <- extract_coefficients_6cat(modelt2_baseline, "baseline", "2009-2019", datat2)
# datat2income <- extract_coefficients_6cat(modelt2_income, "income", "2009-2019", datat2)
datat2incomeint <- extract_coefficients_6cat(modelt2_income_int, "incomeint", "2009-2019", datat2)

coefs <- rbind(
  # datat1baseline, datat1income,
               datat1incomeint,
               # datat2baseline, datat2income,
               datat2incomeint) %>% 
  dplyr::select(Variable, Transition, model, time, Estimate, newLower, newUpper) %>% 
  rename(Upper=newUpper, Lower=newLower) %>% 
  mutate(Variable = recode(Variable, "sex1"="Women","racefinal2black"="Black",
                           "racefinal2Asian/PI"="Asian/PI","racefinal2hispanic"="Hispanic",
                           "racefinal2Native"="Native American","racefinal2other"="Others",
                           "incomescaled"="Household income",
                           "racefinal2black:incomescaled"="Black*Household income",
                           "racefinal2hispanic:incomescaled"="Hispanic*Household income",
                           "racefinal2Asian/PI:incomescaled"="Asian/PI*Household income",
                           "racefinal2other:incomescaled"="Others*Household income"),
         Variable = factor(Variable,
                           levels=c("Women","Black","Hispanic","Asian/PI","Others","Household income",
                                    "Black*Household income", "Hispanic*Household income", "Asian/PI*Household income",
                                    "Others*Household income")),
         time = factor(time, levels=c("1999-2009","2009-2019")),
         model = recode(model, "baseline"="Baseline","income" = "Income", "incomeint" = "Interaction"))


# ggplot(data=coefs, aes(x=Estimate, y=Variable, colour=time)) + geom_point(position=position_dodge(width=-0.5), size=1) + 
#   geom_errorbar(aes(xmin=Lower, xmax=Upper), position=position_dodge(width=-0.5)) + 
#   facet_grid(cols=vars(Transition), rows=vars(model)) + geom_vline(xintercept=1, colour="black",linetype="dashed") +
#   scale_y_discrete(limits=rev) + theme_bw() + 
#   scale_colour_manual(values=c("grey70","black")) + theme(legend.title=element_blank(),
#                                                           strip.background = element_rect(fill="white"),
#                                                           text = element_text(size=18)) + 
#   xlab("Hazard Ratio (+/- 95% CI") + ylab("") + xlim(0,5)

# create a table 
table <- coefs %>% 
  mutate(Estimate = round(Estimate, digits=2),
    finalest = paste0(Estimate, " (", Lower, ",", Upper, ")")) %>% 
  dplyr::select(model, time, Variable, Transition, finalest) %>% 
  pivot_wider(names_from=Transition, values_from=finalest)
write.csv(table, "SIMAH_workplace/education_transitions/final_models/income_model_table_16.csv", row.names=F)



