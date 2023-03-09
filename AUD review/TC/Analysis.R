
# Alcohol Use Disorder dose response meta-analysis

# Load libraries 
library(tidyverse)  # data management
library(readxl)     # import excel data
library(janitor)    # Edit data formatting
library(skimr)      # descriptive statistics
library(tableone)   # create table one


library(dosresmeta)
library(mvtnorm)
library(ellipse)
library(mvmeta)
library(rms)
library(meta)
library(metafor)
library(rmeta)


# Specify the data and output file locations
data    <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/AUD review/Data/"   # Location of data
output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/AUD review/"        # Location of figures/tables


data1 <- read_xlsx (paste0(data, "Data_extraction_AUD_mortality.xlsx"))

data2 <- data1 %>%
  clean_names() %>% remove_empty(which = c("rows", "cols")) %>%
  fill(id_study, .direction="down") %>%
  group_by(id_study) %>% 
    fill(c(first_author, year_published, study_size_n, exposure, outcome, outcome_categories), .direction="down") %>%
  ungroup %>% 
  filter(!str_detect(adjusted_or_or_rr_hr, "rate")) %>%
  mutate(author_year = paste0(first_author, " (", year_published, ")"),
         alc_daily_g = alcohol_mean_gr_day_calculated,
         group = outcome_categories,
         total_n = n_per_alcohol_category,
         outcome_n = n_per_outcome_category,
         RR = ifelse(!is.na(adjusted_or_or_rr_hr), adjusted_or_or_rr_hr, or_or_rr_hr),
         RR = as.numeric(ifelse(RR=="Ref", 1, RR)), 
         lowerRR = as.numeric(ifelse(ci_95_percent_lower=="Ref", 1, ci_95_percent_lower)),
         upperRR = as.numeric(ifelse(ci_95_percent_upper=="Ref", 1, ci_95_percent_upper)),
         logRR = log(RR),
         loglowerRR = log(lowerRR),
         logupperRR = log(upperRR),
         se = ifelse(upperRR!=1 & lowerRR!=1, (logupperRR - loglowerRR)/3.92, NA),
         inverse_se = 1/se) %>%
  select(id_study, id_line, author_year, study_size_n, exposure, outcome, 
         group, alc_daily_g, total_n, outcome_n, 
         RR, lowerRR, upperRR, logRR, logupperRR, loglowerRR, se, inverse_se)
  


ggplot(data2, aes(alc_daily_g, logRR, size=inverse_se)) + 
  geom_point (shape=1, colour="black") + scale_size_area(max_size=20)


lin_mod <- dosresmeta(formula=logRR ~ alc_daily_g, id=id_study, type= type, se=se, cases=cases, n=n, data=data_bin)
summary(lin_bin)












