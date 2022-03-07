
# SIMAH - NESARC Alcohol Transitions
# Functions used



# Transition probabilities (Age not included; stratified by age)
predicted_TP_covs2 <- function(model, year, sex, race, edu) {
  probs <- list()
  for (j in sex){
    for (l in race){
      for (k in edu){
        
        # extract the probabilities
        probs[[paste(j,l,k)]] <- data.frame(print(pmatrix.msm(model, t=year, covariates = list(j, k, l)))) %>%
          
          # modify the output presentation
          mutate(From = row.names(.)) %>%
          pivot_longer(cols = -From, names_to = "To") %>%
          rename(Probability = value) %>%
          mutate(Transition = paste(From, To, sep = "->"),
            sex = j, 
            race = l,
            edu = k,
            year = year) 
      }
    }
  }
  
  probs <- do.call(rbind, probs)
  row.names(probs) <- NULL  # remove row names
  return(probs)
}



# Transition probabilities (Age as continuous)
predicted_TP_covs3 <- function(model, year, age, sex, race, edu) {
  probs <- list()
  for (i in age){
    for (j in sex){
      for (l in race){
        for (k in edu){
          
          age.c_var <- (mapping %>% filter(age==i))$age.c
          
          # extract the probabilities
          probs[[paste(i,j,l,k)]] <- data.frame(print(pmatrix.msm(model, t=year, 
            covariates = list(age.c = age.c_var, female_w1 = j, race_w1 = l, edu3.factor = k)))) %>%
            
            # modify the output presentation
            mutate(From = row.names(.)) %>%
            pivot_longer(cols = -From, names_to = "To") %>%
            rename(Probability = value) %>%
            mutate(Transition = paste(From, To, sep = "->"),
              age = i,
              sex = j, 
              race = l,
              edu = k,
              year = year) 
        }
      }
    }
  }
  
  probs <- do.call(rbind, probs)
  row.names(probs) <- NULL  # remove row names
  return(probs)
}



# Transition probabilities (Age squared)
predicted_TP_covs4 <- function(model, year, age, sex, race, edu) {
  probs <- list()
  for (i in age){
    for (j in sex){
      for (l in race){
        for (k in edu){
          
          age.c_var <- (mapping%>%filter(age==i))$age.c
          age_sq.c_var <- (mapping%>%filter(age==i))$age_sq.c
          
          # extract the probabilities
          probs[[paste(i,j,l,k)]] <- data.frame(print(pmatrix.msm(model, t=year, 
            covariates = list(age.c = age.c_var, age_sq.c = age_sq.c_var,   female_w1 = j, race_w1 = l, edu3.factor = k)))) %>%
            
            # modify the output presentation
            mutate(From = row.names(.)) %>%
            pivot_longer(cols = -From, names_to = "To") %>%
            rename(Probability = value) %>%
            mutate(Transition = paste(From, To, sep = "->"),
              age = i,
              sex = j, 
              race = l,
              edu = k,
              year = year) 
        }
      }
    }
  }
  
  probs <- do.call(rbind, probs)
  row.names(probs) <- NULL  # remove row names
  return(probs)
}








# Function to apply transition probabilities

set.seed(123)  # To get consistent results

transition_alc6 <- function(data, transitions){
  selected <- unique(data$cat)
  rates <- transitions %>% filter(cat == selected)
  data$predicted_var <- ifelse(data$prob<=rates$cumsum[1], "Abstainer",
    ifelse(data$prob<=rates$cumsum[2] & data$prob>rates$cumsum[1], "Former",
      ifelse(data$prob<=rates$cumsum[3] & data$prob>rates$cumsum[2],"Category I",
        ifelse(data$prob<=rates$cumsum[4] & data$prob>rates$cumsum[3],"Category II",
          ifelse(data$prob<=rates$cumsum[5] & data$prob>rates$cumsum[4],"Category III",
            ifelse(data$prob<=rates$cumsum[6] & data$prob>rates$cumsum[5],"Category IV", NA))))))
  return(data)
}


transition_alc5 <- function(data, transitions){
  selected <- unique(data$cat)
  rates <- transitions %>% filter(cat == selected)
  data$predicted_var <- ifelse(data$prob<=rates$cumsum[1], "Abstainer",
    ifelse(data$prob<=rates$cumsum[2] & data$prob>rates$cumsum[1], "Former",
      ifelse(data$prob<=rates$cumsum[3] & data$prob>rates$cumsum[2],"Category I",
        ifelse(data$prob<=rates$cumsum[4] & data$prob>rates$cumsum[3],"Category II",
          ifelse(data$prob<=rates$cumsum[5] & data$prob>rates$cumsum[4],"Category III",NA)))))
  return(data)
}


transition_alc5v2 <- function(data, transitions){
  selected <- unique(data$cat)
  rates <- transitions %>% filter(cat == selected)
  data$predicted_var <- ifelse(data$prob<=rates$cumsum[1], "Non-drinker",
    ifelse(data$prob<=rates$cumsum[2] & data$prob>rates$cumsum[1], "Category I",
      ifelse(data$prob<=rates$cumsum[3] & data$prob>rates$cumsum[2],"Category II",
        ifelse(data$prob<=rates$cumsum[4] & data$prob>rates$cumsum[3],"Category III",
          ifelse(data$prob<=rates$cumsum[5] & data$prob>rates$cumsum[4],"Category IV",NA)))))
  return(data)
}



  
  
  
