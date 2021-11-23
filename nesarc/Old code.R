
# SIMAH - NESARC Alcohol Transitions
# Old Plots and scripts

library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(msm)        # model transition probabilities
library(tableone)   # create descriptives table



# Extract Transition Probability over multiple years at the mean level of each covariate
predicted_TP_overtime <- function(model, max_years, original_n, expanded_n) {
  
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
        SD = SE * sqrt(expanded_n),  # sample size of expanded data
        newSE = SD / sqrt(original_n), # sample size of original data
        newLower = round((Estimate - (newSE * 1.96))*100, digits=1),
        newUpper = round((Estimate + (newSE * 1.96))*100, digits=1),
        Estimate = round(Estimate*100, digits=1),
        
        Year = i) %>%
      select (From, To, Year, Estimate, newLower, newUpper) %>%
      na.omit() # remove NAs - transitions back to 'Abstainer'
  }
  
  probs <- do.call(rbind, probs)
  return(probs)
}


# Extract Transition Probability over multiple years for each age category, at the mean level of other covariates
predicted_TP_overtime_age <- function(model, max_years, age_cat, original_n, expanded_n){
  probs <- list()
  for (i in 1:max_years){
    for (j in age_cat){
      # extract the probabilities
      probs[[paste(i,j)]] <- data.frame(print(pmatrix.msm(model, t=i, ci="norm", covariates = list(age3.factor = j)))) %>%
        
        # modify the output presentation
        mutate(From = row.names(.)) %>%
        pivot_longer(cols = -From, names_to = "To") %>%
        separate(value, into=c("Estimate","Lower","Upper", NA), sep="\\(|\\,|\\)", convert=TRUE) %>%  # separated based on "(" "," and  ")"  convert=TRUE names variables numeric
        mutate(
          # Update CI to use original sample size
          SE = (Upper - Lower) / 3.92,
          SD = SE * sqrt(expanded_n),  # sample size of expanded data
          newSE = SD / sqrt(original_n), # sample size of original data
          newLower = round((Estimate - (newSE * 1.96))*100, digits=1),
          newUpper = round((Estimate + (newSE * 1.96))*100, digits=1),
          Estimate = round(Estimate*100, digits=1),
          Year = i,
          age_cat = j) %>%
        select (From, To, Year, age_cat, Estimate, newLower, newUpper) %>%
        na.omit() # remove NAs - transitions back to 'Abstainer'
    }
  }
  
  probs <- do.call(rbind,probs)
  return(probs)
}



# Extract TP after 3 years (for internal validation)
alc5_TP_3yrs <- predicted_TP_covs (alc5.msm, 3, age_cat, sex, race, edu) %>%
  mutate(From = recode(From, "State 1" = "Abstainer",  # Rename states
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
      "Category III->Category II",	"Category II->Category I",	"Category I->Former"))

write_csv(alc5_TP_3yrs, paste0(output, "Transition Probabilities - AlcUse after 3 years.csv")) # Save TP



# Function to extract TP over 10 years (to plot)
alc5_yearly_TP <- predicted_TP_overtime (alc5.msm, 10, 68330, 4069764) %>%
  mutate(
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
    To = fct_relevel(To, "Abstainer",  "Former",	"Category I",	"Category II", "Category III"))


# Plot TP Over time
alc5_yearly_TP %>%
  ggplot(aes(x=Year, y=Estimate, group=To)) + geom_line(aes(color=To), size=0.5) + 
  geom_ribbon(aes(ymin=newLower, ymax=newUpper, fill=To), alpha=0.2) + 
  facet_wrap(~From) +
  theme_bw() + labs(x = "Years Follow-up", y="Transition probability (%)", color="Transition To", fill="Transition To") +
  theme(legend.position = c(.85, 0.25)) +
  scale_y_continuous(breaks=seq(0, 100, by= 10)) + 
  scale_x_continuous(breaks=seq(0, 10, by= 2))
ggsave(paste0(output, "AlcUse TP over time 1.tiff"), dpi=600, width=7.5, height = 5)




# Function to extract TP over 10 years for each age category
alc5_yearly_TP2 <- predicted_TP_overtime_age (alc5.msm, 10, age_cat, 68330, 4069764) %>%
  mutate(
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
    To = fct_relevel(To, "Abstainer",  "Former",	"Category I",	"Category II", "Category III"))



# Compare model fit
probabilities_alc5 <- read_csv(paste0(output, "Transition Probabilities - AlcUse after 3 years.csv")) %>% 
  mutate(cat = paste(sex, age_cat, edu, race, From, sep="_")) %>% 
  select(cat, To, Probability) %>% 
  group_by(cat) %>% 
  mutate(cumsum = cumsum(Probability)) %>%
  ungroup()


# Function to apply alcohol consumption transition probabilities
transition_alc5 <- function(data, transitions){
  selected <- unique(data$cat)
  rates <- transitions %>% filter(cat == selected)
  data$AlcUse_2_pred <- ifelse(data$prob<=rates$cumsum[1], "Abstainer",
    ifelse(data$prob<=rates$cumsum[2] & data$prob>rates$cumsum[1], "Former",
      ifelse(data$prob<=rates$cumsum[3] & data$prob>rates$cumsum[2],"Category I",
        ifelse(data$prob<=rates$cumsum[4] & data$prob>rates$cumsum[3],"Category II",
          ifelse(data$prob<=rates$cumsum[5] & data$prob>rates$cumsum[4],"Category III",NA)))))
  return(data)
}



# Get predicted alcohol use at wave 3
alc5_data <- nesarc %>%
  # Select and Rename variables of interest
  select(idnum, wave, years, age3.factor, female_wave1.factor, race_wave1.factor, edu3.factor, alc5.factor) %>% # select variables of interest 
  rename(age_cat = age3.factor,
    sex = female_wave1.factor, 
    race = race_wave1.factor,
    edu = edu3.factor,
    AlcUse = alc5.factor) %>%
  
  # Tranform to wide format
  pivot_wider(names_from="wave", values_from=c("AlcUse", "years", "age_cat", "edu")) %>%
  
  # Create variable combing level of each covariate and the state at baseline
  mutate(cat = paste(sex, age_cat_1, edu_1, race, AlcUse_1, sep="_"),
    prob = runif(nrow(.))) %>% # generate random prob
  
  # Apply the transition function
  group_by(cat) %>%
  do(transition_alc5(., probabilities_alc5)) %>% # use 'do( )' to run the function defined earlier
  ungroup() %>% 
  mutate(AlcUse_2_pred = factor(AlcUse_2_pred, levels=c("Abstainer", "Former", "Category I", "Category II", "Category III"))) %>%
  select(-cat, - prob, -years_1)


# Compare observed and predicted at the group
observed <- count(alc5_data, AlcUse_2) %>% rename(observed = n, AlcUse = AlcUse_2) 
predicted <- count(alc5_data, AlcUse_2_pred) %>% rename(predicted = n, AlcUse = AlcUse_2_pred)  
comparison_AlcUse <- full_join (observed, predicted, by="AlcUse") %>%
  mutate(difference = abs(predicted-observed),  
    diff_percent = difference/observed * 100) %>%
  adorn_totals("row") %>%  # Add row totals
  mutate(diff_percent = ifelse(AlcUse=="Total", difference/predicted * 100, diff_percent),
    predicted = ifelse(AlcUse=="Total", NA, predicted), 
    diff_percent = round(diff_percent, 2))

kableone(comparison_AlcUse)













# Extract Transition probabilities (TP) at each level of each covariate --------------------------------------------------------------------------------------------------
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
  labs(title= "'Poor' Transitions", subtitle="",
    color="Race/Ethnicity:", linetype="Race/Ethnicity:",
    x = "Age (years)", y="Transition probability") + 
  theme_bw() + theme(legend.position=c(0.7, 1.085), legend.direction="horizontal") + guides(color = guide_legend(nrow = 2))
ggsave(paste0(output, "TP by race 1.tiff"), dpi=600, width=7.5, height = 8)



# 'Better' Transitions 
aTP_atmean_edu %>%
  filter(Transition %in% c("Category III->Category II", "Category II->Category I", "Category I->Former")) %>%
  ggplot(aes(x=age, y=Probability, color=race, linetype=race)) + geom_line(size=1)+
  facet_grid(cols=vars(sex), rows=vars(Transition), scale="free") +
  scale_x_continuous(limits=c(18, 90), breaks=seq(20, 90, by= 5)) + 
  labs(title= "'Better' Transitions", subtitle="",
    color="Race/Ethnicity:", linetype="Race/Ethnicity:",
    x = "Age (years)", y="Transition probability") + 
  theme_bw() + theme(legend.position=c(0.7, 1.085), legend.direction="horizontal") + guides(color = guide_legend(nrow = 2))
ggsave(paste0(output, "TP by race 2.tiff"), dpi=600, width=7.5, height = 8)

