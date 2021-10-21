
# SIMAH - NESARC Alcohol Transitions
# Old Plots

library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(msm)        # model transition probabilities
library(tableone)   # create descriptives table

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

