
# SIMAH - NESARC Alcohol Transitions
# Extract Transition Probability Function

extractTP <- function(model, age, distinct_ages, sex, race, edu) {
  probs <- list()
  for(i in age){
    for (j in sex){
      for (l in race){
        for (k in edu){
          probs[[paste(i,j,l,k)]] <- pmatrix.msm(model, covariates=list(female_wave1=j, 
                                                                        age_scaled=i, 
                                                                        race_wave1.factor=l, 
                                                                        edu3.factor=k))
          probs[[paste(i,j,l,k)]] <- data.frame(unclass(probs[[paste(i,j,l,k)]]))
          probs[[paste(i,j,l,k)]]$StateFrom <- row.names(probs[[paste(i,j,l,k)]])
          probs[[paste(i,j,l,k)]] <-probs[[paste(i,j,l,k)]] %>% 
              pivot_longer(cols=State.1:State.5, names_to="StateTo", values_to="Probability") %>%
              mutate (StateTo = case_when(endsWith(StateTo,"1") ~ "State 1", 
                                          endsWith(StateTo,"2") ~ "State 2",
                                          endsWith(StateTo,"3") ~ "State 3",
                                          endsWith(StateTo,"4") ~ "State 4",
                                          endsWith(StateTo,"5") ~ "State 5"),
                      age = i,
                      sex = j,
                      race = l,
                      edu = k)
        }
      }
    }
  }
  probs <- do.call(rbind, probs)
  probs <- probs %>%
    mutate(Transition1 = recode(StateFrom, "State 1" = "Abstainer",
                                           "State 2" = "Former",
                                           "State 3" = "Low risk",
                                           "State 4" = "Medium risk",
                                           "State 5" = "High risk"),
            Transition2 = recode(StateTo, "State 1" = "Abstainer",
                                          "State 2" = "Former",
                                          "State 3" = "Low risk",
                                          "State 4" = "Medium risk",
                                          "State 5" = "High risk"),
            Transition = paste(Transition1, Transition2, sep=" -> "),
            sex = as.factor(ifelse(sex==1, "Women", "Men")),
            race = as.factor(race),
            edu = as.factor(edu),
            age = as.factor(age))
  
  return(probs)
}








extractTP2 <- function(model, year, sex) {
  probs <- list()
  for(i in year){
    for (j in sex){
          probs[[paste(i,j)]] <- pmatrix.msm(model, t=year, covariates=list(female_wave1=j))
          probs[[paste(i,j)]] <- data.frame(unclass(probs[[paste(i,j)]]))
          probs[[paste(i,j)]]$StateFrom <- row.names(probs[[paste(i,j)]])
          probs[[paste(i,j)]] <-probs[[paste(i,j)]] %>% 
            pivot_longer(cols=State.1:State.5, names_to="StateTo", values_to="Probability") %>%
            mutate (StateTo = case_when(endsWith(StateTo,"1") ~ "State 1", 
              endsWith(StateTo,"2") ~ "State 2",
              endsWith(StateTo,"3") ~ "State 3",
              endsWith(StateTo,"4") ~ "State 4",
              endsWith(StateTo,"5") ~ "State 5"),
              year = i,
              sex = j)
    }
  }
  probs <- do.call(rbind, probs)
  probs <- probs %>%
    mutate(Transition1 = recode(StateFrom, "State 1" = "Abstainer",
      "State 2" = "Former",
      "State 3" = "Low risk",
      "State 4" = "Medium risk",
      "State 5" = "High risk"),
      Transition2 = recode(StateTo, "State 1" = "Abstainer",
        "State 2" = "Former",
        "State 3" = "Low risk",
        "State 4" = "Medium risk",
        "State 5" = "High risk"),
      Transition = paste(Transition1, Transition2, sep=" -> "),
      sex = as.factor(ifelse(sex==1, "Women", "Men")),
      years = as.factor(years))
  
  return(probs)
}


