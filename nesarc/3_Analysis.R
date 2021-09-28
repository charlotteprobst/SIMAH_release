
# SIMAH - NESARC Alcohol Transitions
# Data Analysis

library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(msm)        # model transition probabilities
library(tableone)   # create descriptives table

## Set the working directory
setwd("C:/Users/klajd/OneDrive/SIMAH")
data    <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Data/"
output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Output/"

# Load data / functions
nesarc <- readRDS(paste0(data, "nesarc_clean.rds")) 
nesarc_expanded <- readRDS(paste0(data, "nesarc_clean_expanded.rds")) 

# Descriptives ------------------------------------------------------------------------------------------

# At baseline
nesarc_wave1 <- filter(nesarc, wave==1)

tab1 <-CreateTableOne(vars= c("female", "age", "race.factor", "married.factor", "edu3.factor", "alc5.factor", "hed.factor"), 
                      factorVars = c("female", "race.factor", "married.factor", "edu3.factor", "alc5.factor", "hed.factor"), 
                      data=nesarc_wave1)
summary(tab1)
table1 <- print(tab1, noSpaces = TRUE, catDigits = 0, contDigits = 1, pDigits = 2, printToggle = FALSE)   
write.csv(table1, file=paste0(output,"Table1-Descriptives.csv"))  # export to excel, to copy/paste into manuscript
kableone(table1)                             # view in R; R Markdown friendly version


# years follow-up
nesarc %>%
  filter(wave==2) %>%
  select (years) %>% 
  skim()

# Run MSM Model ------------------------------------------------------------------------------------------------
# Count of transitions 
statetable.msm(alc5, idnum, data=nesarc)


# Specify transition intensity matrix (Q) - i.e., what (instanteneous) transitions are allowed (specified by the non-zero entries)
# Will only allow transitions to an adjacent state , except for transitions back to lifetime abstainers, and abstainer->former drinker

Q <- rbind ( c(0,     0,    0.25,  0,    0),
             c(0,     0,    0.25,  0,    0),
             c(0,     0.25, 0,     0.25, 0),
             c(0,     0,    0.25,  0,    0.25),
             c(0,     0,    0,     0.25, 0))

# Specify initial values 
Q <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q)

# Run MSM model
alc5.msm <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                 qmatrix = Q, center=FALSE,
                 control = list(trace=1, maxit=500, fnscale = 3000000),
                 covariates = ~ female_wave1.factor + age_scaled + edu3.factor + race_wave1.factor)

    # Save Results
    saveRDS(alc5.msm, paste0(output, "alc5.msm.RDS"))


# Run MSM model with interaction
alc5.msm_int <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
  qmatrix = Q, center=FALSE,
  control = list(trace=1, maxit=1500, fnscale = 3000000),
  covariates = ~ female_wave1 + age_scaled + edu3.factor*race_wave1.factor)


    # Save Results
    saveRDS(alc5.msm_int, paste0(output, "alc5.msm_int.RDS"))
    
    alc5.msm_int <- readRDS(paste0(output, "alc5.msm_int.RDS"))
    alc5.msm_int
# Compare the two models
AIC(alc5.msm, alc5.msm_int)
    
    
# Load Model and View Results -----------------------------------------------------------------------------------------------
alc5.msm <- readRDS(paste0(output, "alc5.msm.RDS"))

      # Transition probabilities at year = t; covariates set to their mean value
      pmatrix.msm(alc5.msm, t=1, ci="norm") # CI based on drawing random samples, default size=1000
      
      # Hazard ratios for transition
      hazard.msm(alc5.msm)


# Table 2 - Extract Annual Transition Probabilities (aTP) and correct CI to original sample size 
aTP.results <- data.frame(print(pmatrix.msm(alc5.msm, t=1, ci="norm"))) %>%
  mutate(From = row.names(.)) %>%
  pivot_longer(cols = -From, names_to = "To") %>%
  separate(value, into=c("Estimate","Lower","Upper", NA), sep="\\(|\\,|\\)", convert=TRUE) %>%  # separated based on "(" "," and  ")"  convert=TRUE names variables numeric
  mutate(
    SE = (Upper - Lower) / 3.92,
    SD = SE * sqrt(4061624),  # sample size of expanded data
    newSE = SD / sqrt(68168), # sample size of original data
    newLower = round((Estimate - (newSE * 1.96))*100, digits=1),
    newUpper = round((Estimate + (newSE * 1.96))*100, digits=1),
    Estimate = round(Estimate*100, digits=1), 
    EstimateCI = paste0(Estimate, " (", newLower, ", ", newUpper, ")")) %>%
  select (From, To, EstimateCI) %>%
  pivot_wider(names_from = "To", values_from = "EstimateCI")

  # save results for paper
  write_csv(aTP.results, paste0(output, "Table2-Annual TP.csv"))


# Table 3 - Extract HR results, rearrange, and correct CI to original sample size 
HR.results <- data.frame(hazard.msm(alc5.msm))%>%
  mutate(transition = row.names(.)) %>%
  pivot_longer(cols=-transition) %>%
  extract(name, into=c("Variable","Type"), regex="(.*)\\.(.*)") %>%   # Separate the string (name) into the variable and type of estimate (HR, Upper, Lower), separate at last occuring period .
  pivot_wider(names_from="Type", values_from = "value") %>%
  mutate(
    SE=(log(U) - log(L))/3.92, 
    SD = SE * sqrt(4061624),  # sample size of expanded data
    newSE = SD / sqrt(68168), # sample size of original data
    newLower = round(exp((log(HR)) - (newSE * 1.96)), digits=2), 
    newUpper = round(exp((log(HR)) + (newSE * 1.96)), digits=2), 
    HR = round(HR, digits=2),
    EstimateCI = paste0(HR, " (", newLower, ", ", newUpper, ")")) %>%
  select(transition, Variable, EstimateCI) %>%
  pivot_wider(names_from = "transition", values_from = EstimateCI) %>%
  rename("Abstainer->LowRisk"   = "State 1 - State 3",
         "Former->LowRisk"      = "State 2 - State 3",
         "LowRisk->Former"      = "State 3 - State 2",
         "LowRisk->MediumRisk"  = "State 3 - State 4",
         "MediumRisk->LowRisk"  = "State 4 - State 3",
         "MediumRisk->HighRisk" = "State 4 - State 5",
         "HighRisk->MediumRisk" = "State 5 - State 4") %>%
  select(Variable, "Abstainer->LowRisk", "Former->LowRisk", "LowRisk->MediumRisk", "MediumRisk->HighRisk", "HighRisk->MediumRisk", "MediumRisk->LowRisk", "LowRisk->Former")

  # save results for paper
  write_csv(HR.results, paste0(output, "Table3-HR.csv"))

  
  
  
# Extract and Plot TP at each level of each covariate --------------------------------------------------------------------------------------------------

# Function to extract TP 
extract_TP <- function(model, age_z, sex, race, edu) {
  probs <- list()
  for (i in age_z){
    for (j in sex){
      for (l in race){
        for (k in edu){
          # extract the probabilities
          probs[[paste(i,j,l,k)]] <- data.frame(print(pmatrix.msm(model, t=1, 
            covariates = list(age_scaled = i,   female_wave1.factor = j, 
                              race_wave1.factor = l, edu3.factor = k)))) %>%
            
            # modify the output presentation
            mutate(From = row.names(.)) %>%
            pivot_longer(cols = -From, names_to = "To") %>%
            rename(Probability = value) %>%
            mutate(
              # Recode values
              From = recode(From, "State 1" = "Abstainer", 
                "State 2" = "Former",
                "State 3" = "Category I",
                "State 4" = "Category II",
                "State 5" = "Category III"), 
              To = recode(To, "State.1" = "Abstainer", 
                "State.2" = "Former",
                "State.3" = "Category I",
                "State.4" = "Category II",
                "State.5" = "Category III"),
              Transition = paste(From, To, sep = "->"),
              Transition = fct_relevel(Transition, "Abstainer->Category I",     # re-arrange order of transition variable
                "Former->Category I",	"Category I->Category II",	"Category II->Category III",
                "Category III->Category II",	"Category II->Category I",	"Category I->Former"),
              age_z = i,
              age = round((age_z * 16.9995) + 46.28592, digits = 0),   # SD of original age variable: 16.9995; mean: 46.28592 
              sex = j, 
              race = l,
              edu = k) 
        }
      }
    }
  }
  
  probs <- do.call(rbind,probs)
  row.names(probs) <- NULL  # remove row names
  return(probs)
  
}
extract_TP_atmean_race <- function(model, age_z, sex, edu) {
    probs <- list()
    for (i in age_z){
      for (j in sex){
        for (k in edu){
            # extract the probabilities
            probs[[paste(i,j,k)]] <- data.frame(print(pmatrix.msm(model, t=1, 
              covariates = list(age_scaled = i,   female_wave1.factor = j, edu3.factor = k)))) %>%
              
              # modify the output presentation
              mutate(From = row.names(.)) %>%
              pivot_longer(cols = -From, names_to = "To") %>%
              rename(Probability = value) %>%
              mutate(
                # Recode values
                From = recode(From, "State 1" = "Abstainer", 
                  "State 2" = "Former",
                  "State 3" = "Category I",
                  "State 4" = "Category II",
                  "State 5" = "Category III"), 
                To = recode(To, "State.1" = "Abstainer", 
                  "State.2" = "Former",
                  "State.3" = "Category I",
                  "State.4" = "Category II",
                  "State.5" = "Category III"),
                Transition = paste(From, To, sep = "->"),
                Transition = fct_relevel(Transition, "Abstainer->Category I",     # re-arrange order of transition variable
                  "Former->Category I",	"Category I->Category II",	"Category II->Category III",
                  "Category III->Category II",	"Category II->Category I",	"Category I->Former"),
                age_z = i,
                age = round((age_z * 16.9995) + 46.28592, digits = 0),   # SD of original age variable: 16.9995; mean: 46.28592 
                sex = j, 
                edu = k) 
        }
      }
    }
    
    probs <- do.call(rbind,probs)
    row.names(probs) <- NULL  # remove row names
    return(probs)
    
}
extract_TP_atmean_edu <- function(model, age_z, sex, race) {
  probs <- list()
  for (i in age_z){
    for (j in sex){
      for (l in race){
          # extract the probabilities
          probs[[paste(i,j,l)]] <- data.frame(print(pmatrix.msm(model, t=1, 
            covariates = list(age_scaled = i,   female_wave1.factor = j, race_wave1.factor = l)))) %>%
            
            # modify the output presentation
            mutate(From = row.names(.)) %>%
            pivot_longer(cols = -From, names_to = "To") %>%
            rename(Probability = value) %>%
            mutate(
              # Recode values
              From = recode(From, "State 1" = "Abstainer", 
                "State 2" = "Former",
                "State 3" = "Category I",
                "State 4" = "Category II",
                "State 5" = "Category III"), 
              To = recode(To, "State.1" = "Abstainer", 
                "State.2" = "Former",
                "State.3" = "Category I",
                "State.4" = "Category II",
                "State.5" = "Category III"),
              Transition = paste(From, To, sep = "->"),
              Transition = fct_relevel(Transition, "Abstainer->Category I",     # re-arrange order of transition variable
                "Former->Category I",	"Category I->Category II",	"Category II->Category III",
                "Category III->Category II",	"Category II->Category I",	"Category I->Former"),
              age_z = i,
              age = round((age_z * 16.9995) + 46.28592, digits = 0),   # SD of original age variable: 16.9995; mean: 46.28592 
              sex = j, 
              race = l) 
      }
    }
  }
  
  probs <- do.call(rbind,probs)
  row.names(probs) <- NULL  # remove row names
  return(probs)
  
}

  

# First, specify the covariate values
age_z <- sort(unique(nesarc_expanded$age_scaled))
sex <- unique(nesarc_expanded$female_wave1.factor)
race <- unique(nesarc_expanded$race_wave1.factor)
edu <- unique(nesarc_expanded$edu3.factor)

# Run function to extract TP
aTP <- extract_TP(alc5.msm, age_z, sex, race, edu)
aTP_atmean_race <- extract_TP_atmean_race(alc5.msm, age_z, sex, edu) # TP at mean value of race (for plots)
aTP_atmean_edu <- extract_TP_atmean_edu(alc5.msm, age_z, sex, race) # TP at mean value of edu (for plots)


# Save TP
write_csv(aTP, paste0(output, "Transition Probabilities.csv"))



### Plot Transition probabilities --------------------------------------------------------------------------
# 'Poor' Transitions, among non-Hispanic Whites 
aTP %>%
  filter(Transition %in% c("Abstainer->Category I", "Former->Category I",  "Category I->Category II", "Category II->Category III")) %>%
  filter(race=="White, non-Hispanic") %>%
  ggplot(aes(x=age, y=Probability, color=edu, linetype=edu)) + geom_line(size=1)+
  facet_grid(cols=vars(sex), rows=vars(Transition), scale="free") +
  scale_x_continuous(limits=c(18, 90), breaks=seq(20, 90, by= 5)) + 
  labs(title= "'Poor' Transitions", subtitle="White, non-Hispanic", 
        color="Education:", linetype="Education:",
        x = "Age (years)", y="Transition probability") + 
  theme_bw() + theme(legend.position=c(0.8, 1.1), legend.direction="horizontal")
ggsave(paste0(output, "Figure 1a.tiff"), dpi=600, width=7.5)



# 'Better' Transitions, among non-Hispanic Whites 
aTP %>%
  filter(Transition %in% c("Category III->Category II", "Category II->Category I", "Category I->Former")) %>%
  filter(race=="White, non-Hispanic") %>%
  ggplot(aes(x=age, y=Probability, color=edu, linetype=edu)) + geom_line(size=1)+
  facet_grid(cols=vars(sex), rows=vars(Transition), scale="free") +
  scale_x_continuous(limits=c(18, 90), breaks=seq(20, 90, by= 5)) + 
  labs(title= "'Poor' Transitions", subtitle="White, non-Hispanic", 
    color="Education:", linetype="Education:",
    x = "Age (years)", y="Transition probability") + 
  theme_bw() + theme(legend.position=c(0.8, 1.1), legend.direction="horizontal")
ggsave(paste0(output, "Figure 1b.tiff"), dpi=600, width=7.5)


### Plot Transition probabilities at mean race --------------------------------------------------------------------------

# 'Poor' Transitions
aTP_atmean_race %>%
  filter(Transition %in% c("Abstainer->Category I", "Former->Category I",  "Category I->Category II", "Category II->Category III")) %>%
  ggplot(aes(x=age, y=Probability, color=edu, linetype=edu)) + geom_line(size=1)+
  facet_grid(cols=vars(sex), rows=vars(Transition), scale="free") +
  scale_x_continuous(limits=c(18, 90), breaks=seq(20, 90, by= 5)) + 
  labs(title= "'Poor' Transitions", 
    color="Education:", linetype="Education:",
    x = "Age (years)", y="Transition probability") + 
  theme_bw() + theme(legend.position=c(0.8, 1.075), legend.direction="horizontal")
ggsave(paste0(output, "TP by edu 1.tiff"), dpi=600, width=7.5)


# 'Better' Transitions 
aTP_atmean_race %>%
  filter(Transition %in% c("Category III->Category II", "Category II->Category I", "Category I->Former")) %>%
  ggplot(aes(x=age, y=Probability, color=edu, linetype=edu)) + geom_line(size=1)+
  facet_grid(cols=vars(sex), rows=vars(Transition), scale="free") +
  scale_x_continuous(limits=c(18, 90), breaks=seq(20, 90, by= 5)) + 
  labs(title= "'Better' Transitions", 
    color="Education:", linetype="Education:",
    x = "Age (years)", y="Transition probability") + 
  theme_bw() + theme(legend.position=c(0.8, 1.075), legend.direction="horizontal")
ggsave(paste0(output, "TP by edu 2.tiff"), dpi=600, width=7.5)



### Plot Transition probabilities at mean edu --------------------------------------------------------------------------
# 'Poor' Transitions
aTP_atmean_edu %>%
  filter(Transition %in% c("Abstainer->Category I", "Former->Category I",  "Category I->Category II", "Category II->Category III")) %>%
  ggplot(aes(x=age, y=Probability, color=race, linetype=race)) + geom_line(size=1)+
  facet_grid(cols=vars(sex), rows=vars(Transition), scale="free") +
  scale_x_continuous(limits=c(18, 90), breaks=seq(20, 90, by= 5)) + 
  labs(title= "'Better' Transitions", subtitle="",
    color="Race/Ethnicity:", linetype="Race/Ethnicity:",
    x = "Age (years)", y="Transition probability") + 
  theme_bw() + theme(legend.position=c(0.7, 1.1), legend.direction="horizontal") + guides(color = guide_legend(nrow = 2))
ggsave(paste0(output, "TP by race 1.tiff"), dpi=600, width=7.5)



# 'Better' Transitions 
aTP_atmean_edu %>%
  filter(Transition %in% c("Category III->Category II", "Category II->Category I", "Category I->Former")) %>%
  ggplot(aes(x=age, y=Probability, color=race, linetype=race)) + geom_line(size=1)+
  facet_grid(cols=vars(sex), rows=vars(Transition), scale="free") +
  scale_x_continuous(limits=c(18, 90), breaks=seq(20, 90, by= 5)) + 
  labs(title= "'Better' Transitions", subtitle="",
    color="Race/Ethnicity:", linetype="Race/Ethnicity:",
    x = "Age (years)", y="Transition probability") + 
  theme_bw() + theme(legend.position=c(0.7, 1.1), legend.direction="horizontal") + guides(color = guide_legend(nrow = 2))
ggsave(paste0(output, "TP by race 2.tiff"), dpi=600, width=7.5)



# TP over over multiple years ---------------------------------------------------------------------------------------------

# Function to extract TP over over multiple years
extract_TP_over_yrs <- function(model, max_years) {
  probs <- list()
  for (i in 1:max_years){    
    # extract the probabilities
    probs[[i]] <- data.frame(print(pmatrix.msm(model, t=i, ci="norm"))) %>%
      
      # modify the output presentation
      mutate(From = row.names(.)) %>%
      pivot_longer(cols = -From, names_to = "To") %>%
      separate(value, into=c("Estimate","Lower","Upper", NA), sep="\\(|\\,|\\)", convert=TRUE) %>%  # separated based on "(" "," and  ")"  convert=TRUE names variables numeric
      mutate(
        # Update CI to use original sample size
        SE = (Upper - Lower) / 3.92,
        SD = SE * sqrt(4061624),  # sample size of expanded data
        newSE = SD / sqrt(68168), # sample size of original data
        newLower = round((Estimate - (newSE * 1.96))*100, digits=1),
        newUpper = round((Estimate + (newSE * 1.96))*100, digits=1),
        Estimate = round(Estimate*100, digits=1),
        
        # Recode values
        From = recode(From, "State 1" = "Initial State: Abstainer", 
                            "State 2" = "Initial State: Former",
                            "State 3" = "Initial State: Category I",
                            "State 4" = "Initial State: Category II",
                            "State 5" = "Initial State: Category III"), 
        To = recode(To, "State.1" = "Abstainer", 
                        "State.2" = "Former",
                        "State.3" = "Category I",
                        "State.4" = "Category II",
                        "State.5" = "Category III"), 
        # Re-arrange order
        From = fct_relevel(From, "Initial State: Abstainer",  "Initial State: Former",	"Initial State: Category I",	"Initial State: Category II", "Initial State: Category III"),
        To = fct_relevel(To, "Abstainer",  "Former",	"Category I",	"Category II", "Category III"),
        
        Year = i) %>%
      select (From, To, Year, Estimate, newLower, newUpper) %>%
      na.omit() # remove NAs - transitions back to 'Abstainer'
  }
  
  probs <- do.call(rbind,probs)
  return(probs)
}

# Extract TP over multiple years and save reults 
TP_years <- extract_TP_over_yrs(alc5.msm, 10)

# Load TP over multiple years and plot
TP_years %>%
  ggplot(aes(x=Year, y=Estimate, group=To)) + geom_line(aes(color=To), size=0.5) + 
  geom_ribbon(aes(ymin=newLower, ymax=newUpper, fill=To), alpha=0.2) + 
  facet_wrap(~From) +
  theme_bw() + labs(x = "Years Follow-up", y="Transition probability", color="Transition To", fill="Transition To") +
  theme(legend.position = c(.85, 0.25)) +
  scale_y_continuous(breaks=seq(0, 100, by= 10)) + 
  scale_x_continuous(breaks=seq(0, 10, by= 2))
ggsave(paste0(output, "TP over time.tiff"), dpi=600, width=7.5)


