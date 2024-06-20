# Race x Lifestyle Differential Vulnerability & Exposure Project
# Descriptive Statistics

# LOAD DATA AND SET FILE LOCATIONS

# load libraries
library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(gmodels)    # CrossTable command
library(tableone)   # create table one
library(survival)   # surivval analyses
library(survminer)  # surivval analyses
library(timereg)    # additive survival models
library(survey)     # for survey weighted cox model
library(biostat3)   # survRate command
library(broom)      # data/model management
library(fmsb)       # Radar plots
library(srvyr)


# Specify file locations
data    <- "~/Dropbox/Mac/Desktop/Public Health Institute/Alcohol Research Group/SIMAH/SIMAH_workplace/nhis/Restricted access data/Data/"    # Location of data
output  <- "~/Dropbox/Mac/Desktop/Public Health Institute/Alcohol Research Group/SIMAH/SIMAH_workplace/nhis/Restricted access data/Causal mediation/Race x Lifestyle/CausMed/18-85 years old/Figures/"           # Location of figures/tables

# Load data (participants aged 25-85 years)
nhis_all    <- readRDS (paste0(data, "nhis_all18_85.rds"))
nhis18_85   <- readRDS (paste0(data, "nhis18_85.rds"))
nhis18_male   <- filter(nhis18_85, female==0)
nhis18_female <- filter(nhis18_85, female==1)



# Function to resolve a conflict between {survey} and {tableone} -------------------------------------------------------
# print() would not work with continuous variables in objects created by svyCreateTableOne()
svyQuant_alt <- function (vars, design, q = 0.5) {
  res <- vector()
  for (i in 1:length(vars)) {
    var <- vars[i]
    res[i] <- oldsvyquantile(design$variables[var], design = design,
                             quantiles = q[1], na.rm = TRUE)
  }
  out <- as.vector(res)
  names(out) <- vars
  out
}

environment(svyQuant_alt) <- asNamespace('tableone')
assignInNamespace("svyQuant", svyQuant_alt, ns = "tableone")


# DESCRIPTIVE STATISTICS 

# Table 1: Participant characteristics - STRATIFIED BY SEX

# by race and sex
  ## unweighted
tab1 <- CreateTableOne(vars = c("age", "yrs_followup","allcause_death.factor", "alcohol5v2.factor","smk.factor",  
                               "bmi.factor", "phy.factor", "edu.factor",  "married.factor", "us_born"), 
                      factorVars = c("allcause_death.factor",  "alcohol5v2.factor", "smoking4.factor", "bmi.factor", 
                                      "phy.factor", "edu.factor",  "married.factor", "us_born"), 
                      strata = c("ethnicity.factor", "female.factor"), addOverall = TRUE, data = nhis18_85)
  table1_v1 <- print(tab1, noSpaces = TRUE, contDigits = 1, printToggle = FALSE, test=FALSE)  # Shows sample size and %
  table1_v2 <- print(tab1, noSpaces = TRUE, contDigits = 1, printToggle = FALSE, test=FALSE, format="p") # shows % only
  write.csv(table1_v1, file = file.path(output, "Table1 Demographics_V1.csv"))
  write.csv(table1_v2, file = file.path(output, "Table1 Demographics_V2.csv"))
  kableone(table1_v1)

  ## weighted
  # Survey adjusted Data
  # Using {srvyr} finalize data set adjusting for survey weights
  nhis18_85_svy <- nhis18_85 %>%
    as_survey_design(id=new_psu, strata=new_stratum, weights=new_weight, nest = TRUE)
  
  svyCreateTableOne(vars = c("age", "yrs_followup","allcause_death.factor", "alcohol5v2.factor","smk.factor",  
                             "bmi.factor", "phy.factor", "edu.factor",  "married.factor", "us_born"), 
                    strata = c("ethnicity.factor", "female.factor"), addOverall = TRUE, data = nhis18_85_svy) %>% 
    print(noSpaces = TRUE, contDigits = 1, printToggle = FALSE, test=FALSE) %>%
    write.csv(paste0(output, "Table1_svy Demographics_V1.csv")) # shows sample size and %
  
  svyCreateTableOne(vars = c("age", "yrs_followup","allcause_death.factor", "alcohol5v2.factor","smk.factor",  
                             "bmi.factor", "phy.factor", "edu.factor",  "married.factor", "us_born"), 
                    strata = c("ethnicity.factor", "female.factor"), addOverall = TRUE, data = nhis18_85_svy) %>% 
    print(noSpaces = TRUE, contDigits = 1, printToggle = FALSE, test=FALSE, format="p") %>%
    write.csv(paste0(output, "Table1_svy Demographics_V2.csv")) # shows % only
  
  
  
# by sex only
  ## unweighted
tab2 <- CreateTableOne(vars = c("age", "yrs_followup","allcause_death.factor", "alcohol5v2.factor","smk.factor",  
                                  "bmi.factor", "phy.factor", "edu.factor",  "married.factor", "us_born"), 
                         factorVars = c("allcause_death.factor",  "alcohol5v2.factor", "smoking4.factor", "bmi.factor", 
                                        "phy.factor", "edu.factor",  "married.factor", "us_born"), 
                         strata = c("female.factor"), addOverall = TRUE, data = nhis18_85)
  table2_v1 <- print(tab2, noSpaces = TRUE, contDigits = 1, printToggle = FALSE, test=FALSE)  # Shows sample size and %
  table2_v2 <- print(tab2, noSpaces = TRUE, contDigits = 1, printToggle = FALSE, test=FALSE, format="p") # shows % only
  write.csv(table2_v1, file = file.path(output, "Table2 Demographics_V1.csv"))
  write.csv(table2_v2, file = file.path(output, "Table2 Demographics_V2.csv"))
  kableone(table2_v1)
  
  
  svyCreateTableOne(vars = c("age", "yrs_followup","allcause_death.factor", "alcohol5v2.factor","smk.factor",  
                             "bmi.factor", "phy.factor", "edu.factor",  "married.factor", "us_born"), 
                    strata = c("female.factor"), addOverall = TRUE, data = nhis18_85_svy) %>% 
    print(noSpaces = TRUE, contDigits = 1, printToggle = FALSE, test=FALSE) %>%
    write.csv(paste0(output, "Table2_svy Demographics_V1.csv")) # shows sample size and %
  
  svyCreateTableOne(vars = c("age", "yrs_followup","allcause_death.factor", "alcohol5v2.factor","smk.factor",  
                             "bmi.factor", "phy.factor", "edu.factor",  "married.factor", "us_born"), 
                    strata = c("female.factor"), addOverall = TRUE, data = nhis18_85_svy) %>% 
    print(noSpaces = TRUE, contDigits = 1, printToggle = FALSE, test=FALSE, format="p") %>%
    write.csv(paste0(output, "Table2_svy Demographics_V2.csv")) # shows % only
  
  
  
  
  
# Person years and death rate: tstop = person years; event = # events; rate = events per person year 
survRate(Surv(yrs_followup, allcause_death) ~ female.factor + ethnicity.factor, data = nhis18_85) %>% 
  mutate(group = paste(female.factor, ethnicity.factor, sep=", "),
         rate_10000py = rate * 10000) %>% remove_rownames() %>%
  dplyr::select (group, tstop, rate_10000py) %>% t()%>%
  write.csv(file = file.path(output, "Table1 Demographics_V3.csv"))
  
survRate(Surv(yrs_followup, allcause_death) ~ ethnicity.factor, data = nhis18_85) %>% 
  mutate(rate_10000py = round(rate * 10000, 0)) %>% 
  dplyr::select (ethnicity.factor, rate_10000py)



# Race/ethnicity proportions
nhis18_85 %>% group_by(ethnicity.factor) %>%
  summarise(n = n()) %>%
  mutate(perc = round(n / sum(n) *100 ,0))

nhis18_85 %>% filter(ethnicity.factor == "Other") %>% 
  group_by (ethnicity_detail) %>%
  summarise (n = n()) %>%
  mutate (perc = round(n / sum(n) *100 ,0))



# eTable 1: Attrition - Participant characteristics - STRATIFIED BY SEX
nhis_all_male <- filter(nhis_all, female==0)
nhis_all_female <- filter(nhis_all, female==1)

all_vars <- c("age", "alcohol5v2.factor","smk.factor", "bmi.factor", "phy.factor", "edu.factor",  "married.factor")
factor_vars <- c("alcohol5v2.factor", "smk.factor", "bmi.factor", "phy.factor", "edu.factor",  "married.factor")

tab_e1 <- CreateTableOne(all_vars, factor_vars, strata = "lost", data = nhis_all_male)
table_e1 <- print(tab_e1, noSpaces = TRUE, contDigits = 1, printToggle = FALSE, test=FALSE, smd=TRUE, format="p") # shows % only
write.csv(table_e1, file = file.path(output, "Table e1 (men) Attrition.csv"))
kableone(table_e1)

tab_e1 <- CreateTableOne(all_vars, factor_vars, strata="lost", data = nhis_all_female)
table_e1 <- print(tab_e1, noSpaces = TRUE, contDigits = 1, printToggle = FALSE, test=FALSE, smd=TRUE, format="p") # shows % only
write.csv(table_e1, file = file.path(output, "Table e1 (women) Attrition.csv"))
kableone(table_e1)



fit <- survfit(Surv(bl_age, end_age, allcause_death) ~ ethnicity.factor, data = nhis18_85)

tiff(file.path(output, "Figure 3.tiff"), width=3000, height = 1500, res=300)

# Figure 3: Survival plot 
ggsurvplot_facet(fit = fit,
  data = nhis18_85, 
  facet.by="female.factor", censor = FALSE, xlim = c(18, 100), 
  conf.int = TRUE, 
  #surv.median.line = "hv",
  risk.table = TRUE, cumevents = FALSE,
  short.panel.labs = TRUE, 
  legend.title = "Race/ethnicity",
  # legend.labs = c("Black", "Hispanic", "Other", "White"),
  xlab = "Age (years)")

dev.off()

      # Age Medium Survival:
      survfit(Surv(bl_age, end_age, allcause_death) ~ ethnicity.factor, data = nhis18_85)
      survfit(Surv(bl_age, end_age, allcause_death) ~ ethnicity.factor, data = nhis18_female)
      survfit(Surv(bl_age, end_age, allcause_death) ~ ethnicity.factor, data = nhis18_male)
      
    
# Radar plots
radar <- nhis18_85 %>%
  mutate(high_risk_drinker = ifelse(alcohol5v2.factor %in% c("Category II", "Category III"), 1, 0),
         everyday_smoker = ifelse(smk.factor %in% c("Current everyday smoker"), 1, 0),
         sedentary = ifelse(phy.factor %in% c("Sedentary"), 1, 0),
         obese = ifelse(bmi.factor %in% c("Obese"), 1, 0)) %>%
  group_by(ethnicity.factor, female.factor) %>%
  summarise(across(c(high_risk_drinker, everyday_smoker, sedentary, obese), mean)) %>%
  mutate (across(c(high_risk_drinker, everyday_smoker, sedentary, obese), ~ round(.x*100, 0)))  %>% 
  ungroup()

radar_svy <- read_csv(paste0(output, "Table1_svy Demographics_V2.csv")) %>%
  rename(Variable = ...1) %>% 
  select(-Overall) %>%
  filter(Variable %in% c("Category II", "Category III", "Current everyday smoker", "Obese", "Sedentary")) %>%
  t() %>%
  row_to_names(row_number = 1) %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  separate(rowname, into = c("ethnicity.factor", "female.factor"), sep = ":", remove = F) %>%
  mutate_at(c("Category II", "Category III", "Current everyday smoker", "Obese", "Sedentary"), as.numeric) %>%
  mutate(`Category II/III` = `Category II` + `Category III`) %>%
  rename(high_risk_drinker = `Category II/III`, 
         everyday_smoker = `Current everyday smoker`, 
         sedentary = Sedentary, 
         obese = Obese) %>%
  select(ethnicity.factor, female.factor, high_risk_drinker, everyday_smoker, sedentary, obese)



# Stratified by ethnicity
my_radarchart <- function(data, ethnicity, title = NULL, legend = FALSE){
      
      min <- 0
      max <- 60
      
      data <- data %>%
        filter(ethnicity.factor == ethnicity) %>% 
        dplyr::select(-ethnicity.factor) %>%
        mutate(female.factor = recode(female.factor, "Male" = "Men", "Female" = "Women")) %>%
        column_to_rownames(var = "female.factor") %>%
        add_row(high_risk_drinker=min, everyday_smoker=min, sedentary=min, obese=min, .before=1) %>%
        add_row(high_risk_drinker=max, everyday_smoker=max, sedentary=max, obese=max, .before=1) 
      
      
      radarchart(data, axistype = 1,
                # Customize the polygon
                pcol = c("#00AFBB", "#FC4E07"), pfcol = scales::alpha(c("#00AFBB", "#FC4E07"), 0.15), 
                plwd = 2, plty = 1,
                # Customize the grid
                cglcol = "grey", cglty = 1, cglwd = 0.8,
                # Customize the axis
                axislabcol = "grey", 
                # Variable labels
                vlcex = 1, vlabels = c("Category II/III \n drinker", "Current \n everyday \n smoker", "Sedentary", "Obese"),
                caxislabels = seq(0,60,15), title = title)
      
      if(legend==TRUE){
        legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col= c("#00AFBB", "#FC4E07") , text.col = "black", cex=1.2, pt.cex=3)
      }
    }
    
    tiff(file.path(output, "Figure 2 - Prevalence of risk factors.tiff"), width=2200, height = 2000, res=300)
    op <- par(mar = c(1, 1, 1, 1))
    par(mfrow=c(2,2))
    my_radarchart(radar, "Non-Hispanic White", title="Non-Hispanic White")
    my_radarchart(radar, "Non-Hispanic Black", title="Non-Hispanic Black", legend=TRUE)
    my_radarchart(radar, "Hispanic", title="Hispanic/Latinx")
    my_radarchart(radar, "Other", title="Non-Hispanic Other")
    par(mfrow=c(1,1))
    dev.off()
    
    
    
# Stratified by sex     
my_radarchart2 <- function(data, sex, title = NULL, legend=FALSE){
  
  min <- 0
  max <- 60
  
  data <- data %>%
    filter(female.factor==sex) %>% 
    filter(ethnicity.factor!="Other") %>% 
    dplyr::select(-female.factor) %>%
    #mutate(female.factor = recode(female.factor, "Male" = "Men", "Female" = "Women")) %>%
    column_to_rownames(var = "ethnicity.factor") %>%
    add_row(high_risk_drinker=min, everyday_smoker=min, sedentary=min, obese=min, .before=1) %>%
    add_row(high_risk_drinker=max, everyday_smoker=max, sedentary=max, obese=max, .before=1) 
  
  radarchart(data, axistype = 1,
    # Customize the polygon
    pcol = c("#00AFBB", "#FC4E07", "#E7B800", "#99999980"), 
            pfcol = scales::alpha(c("#00AFBB", "#FC4E07", "#E7B800"), 0.15), 
    plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = 1, vlabels = c("Category II/III drinker", "Current \n everyday \n smoker", "Sedentary", "Obese"),
    caxislabels = seq(0,60,15), title = title)
  
  if(legend==TRUE){
    legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , 
          col= c("#00AFBB", "#FC4E07", "#E7B800") , text.col = "black", cex=1.2, pt.cex=3)
  }
}


    tiff(file.path(output, "Figure 2_v2_unweighted - Prevalence of risk factors.tiff"), width=2000, height = 2500, res=300)
    op <- par(mar = c(1, 1, 1, 1))
    par(mfrow=c(2,1))
    my_radarchart2(radar, "Men", title="Men", legend=TRUE)
    my_radarchart2(radar, "Women", title="Women")
    par(mfrow=c(1,1))
    dev.off()
    

    # weighted
    tiff(file.path(output, "Figure 2_v2_weighted - Prevalence of risk factors.tiff"), width=2000, height = 2500, res=300)
    op <- par(mar = c(1, 1, 1, 1))
    par(mfrow=c(2,1))
    my_radarchart2(radar_svy, "Male", title="Men", legend=TRUE)
    my_radarchart2(radar_svy, "Female", title="Women")
    par(mfrow=c(1,1))
    dev.off()

    
# Race by education counts
library(plotly)   
library(htmlwidgets)
race_edu <- nhis18_85 %>%
  dplyr::select(female.factor, ethnicity.factor, edu.factor) %>% 
  count(female.factor, ethnicity.factor, edu.factor) %>%
  rename(sex = female.factor, ethnicity = ethnicity.factor, edu = edu.factor) %>% 
  mutate(edu = factor(edu, levels=c("Highschool", "Some college", "Bachelors"))) %>% 
  ggplot(aes(ethnicity, edu, fill= n)) + geom_tile() + facet_wrap(~sex)

ggplotly(race_edu)  
ggplotly(race_edu) %>% saveWidget(file=paste0(output, "Race by education counts.html"))
    

  
  
  