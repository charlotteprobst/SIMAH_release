
# Alcohol Use Disorder dose response meta-analysis

# Load libraries 
library(tidyverse)  # data management
library(readxl)     # import excel data
library(janitor)    # Edit data formatting
library(skimr)      # descriptive statistics
library(tableone)   # create table one
# devtools::install_github("alecri/dosresmeta")
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
         se = (log_upperRR - log_lowerRR)/3.92)

data_all <- data1 %>%
  filter(group == "All participants")



# Analyses *********************************************************************************************

# Graph of logRRs by alcohol use
ggplot(data_all, aes(alc_daily_g, log_RR, size=1/se)) + 
  geom_point (shape=1) + scale_size_area(max_size=20) + theme_bw()

# One stage dose-response meta-analysis 
lin_mod <- dosresmeta(formula=log_RR ~ alc_daily_g, proc="1stage", 
                        id=id_study, type="cc", se=se, cases=outcome_n, n=total_n, 
                        data=data_all)
    # Should address/further look into the warning regarding optimization
    summary(lin_mod)
    predict(lin_mod, delta=10, exp=TRUE) # delta specifies dose increase of interest; e.g., delta=1 will give the RR associated with a 1-unit increase

# Plot
predict(lin_mod, data.frame(alc_daily_g=seq(0, 200, 1)), order=TRUE, exp=TRUE) %>% 
  ggplot(aes(x=alc_daily_g, y=pred)) + geom_line() + 
    geom_ribbon(aes(ymin= ci.lb, ymax=ci.ub), alpha=0.1) + 
    coord_cartesian(ylim=c(-5, 50))+
    theme_bw()




