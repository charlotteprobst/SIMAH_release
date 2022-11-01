library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(readxl)

setwd("~/Google Drive/SIMAH Sheffield")

data <- read_rds("SIMAH_workplace/reweighted_PSID_imp_list.RDS")
data <- data[[1]]

source("SIMAH_code/education_transitions/2_analysis/1_setup_markov_model.R")

modelst1_baseline <- read_rds("SIMAH_workplace/education_transitions/final_models/t1_baseline_parent.RDS")
modelst2_baseline <- read_rds("SIMAH_workplace/education_transitions/final_models/t2_baseline_parent.RDS")
modelst1_parent <- read_rds("SIMAH_workplace/education_transitions/final_models/t1_interaction.RDS")
modelst2_parent <- read_rds("SIMAH_workplace/education_transitions/final_models/t2_interaction.RDS")

data1 <- data %>% filter(year<=2009)
data2 <- data %>% filter(year>=2011)
coefst1baseline <- lapply(modelst1_baseline, extract_coefficients, type="baseline", timeperiod="1999-2009",
                          data=data1)
coefst1baseline <- bind_imputations(coefst1baseline)

coefst2baseline <- lapply(modelst2_baseline, extract_coefficients, type="baseline", timeperiod="2011-2019",
                          data=data1)
coefst2baseline <- bind_imputations(coefst2baseline)

coefst1parent <- lapply(modelst1_parent, extract_coefficients, type="parent", timeperiod="1999-2009",
                          data=data1)
coefst1parent <- bind_imputations(coefst1parent)

coefst2parent <- lapply(modelst2_parent, extract_coefficients, type="parent", timeperiod="2011-2019",
                          data=data1)
coefst2parent <- bind_imputations(coefst2parent)

coefs <- rbind(coefst1baseline, coefst2baseline,
               coefst1parent, coefst2parent) %>% 
  dplyr::select(Variable, Transition, model, time, Estimate, NewLower, NewUpper) %>% 
  rename(Upper=NewUpper, Lower=NewLower) %>% 
  mutate(Variable = recode(Variable, "sex1"="Women","racefinal2black"="Black",
                           "racefinal2Asian/PI"="Asian/PI","racefinal2hispanic"="Hispanic",
                           "racefinal2Native"="Native American","racefinal2other"="Others",
                           "oneCollegeplus"="One parent college +",
                           "racefinal2black:oneCollegeplus"="Black*One parent college +",
                           "racefinal2hispanic:oneCollegeplus"="Hispanic*One parent college +",
                           "racefinal2other:oneCollegeplus"="Others*One parent college +"),
         Variable = factor(Variable,
                           levels=c("Women","Black","Hispanic","Asian/PI","Native American","Others","One parent college +",
                                    "Black*One parent college +", "Hispanic*One parent college +",
                                    "Others*One parent college +")),
         time = factor(time, levels=c("1999-2009","2011-2019")),
         model = recode(model, "baseline"="Baseline","parent" = "Interaction"))
# %>% 
#   mutate(Stat = paste0(Estimate, " (", Lower, "-", Upper, ")")) %>% 
#   dplyr::select(time, model, Variable, Transition, Stat) %>% 
#   pivot_wider(names_from=Transition, values_from=Stat) %>% 
#   dplyr::select(-`NA`)

coefs <- coefs %>% drop_na()

ggplot(data=coefs, aes(x=Estimate, y=Variable, colour=model)) + geom_point(position=position_dodge(width=-0.5), size=1) + 
  geom_errorbar(aes(xmin=Lower, xmax=Upper), position=position_dodge(width=-0.5)) + 
  facet_grid(cols=vars(Transition), rows=vars(time)) + geom_vline(xintercept=1, colour="black",linetype="dashed") +
  scale_y_discrete(limits=rev) + theme_bw() + 
  scale_colour_manual(values=c("grey70","black")) + theme(legend.title=element_blank(),
                                                          strip.background = element_rect(fill="white"),
                                                          text = element_text(size=18)) + 
  xlab("Hazard Ratio (+/- 95% CI") + ylab("")

ggsave("SIMAH_workplace/education_transitions/final_models/Figure1.png", dpi=300, width=33, height=19, units="cm")

