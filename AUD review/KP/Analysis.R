
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

# Load and edit data
data1 <- read_xlsx (paste0(data, "Analysis_data_AUD_mortality.xlsx")) %>%
  mutate(author_year = paste0(first_author, " (", year_published, ")"),
         group=factor(group),
         log_RR = log(RR),
         log_lowerRR = log(lowerRR),
         log_upperRR = log(upperRR),
         se = ifelse(RR==1 & lowerRR ==1 & upperRR==1, NA, (log_upperRR - log_lowerRR)/3.92),
         inverse_se = 1/se)

data_all <- data %>%
  filter(group == "All participants")

data_men <- data %>%
  filter(group == "Males")

data_women <- data %>%
  filter(group == "Females")


# Analyses *********************************************************************************************
ggplot(data_all, aes(alc_daily_g, log_RR, size=inverse_se, color=group, fill=group)) + 
  geom_point (shape=1) + scale_size_area(max_size=20) + theme_bw()



lin_mod <- dosresmeta(formula=log_RR ~ alc_daily_g, id=id_study, type="cc", se=se, cases=outcome_n, n=total_n, data=data1)
summary(lin_bin)


meta <- metagen(TE = log_RR, lower = log_lowerRR, upper = log_upperRR,
                level.ci = 0.95, studlab = id_study,  data = data1,
                sm = "RR", fixed = FALSE, random = TRUE, method.tau = "REML")
summary(meta)

forest.meta(meta)







