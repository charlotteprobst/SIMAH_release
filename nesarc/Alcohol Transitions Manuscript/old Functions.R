
# SIMAH - NESARC Alcohol Transitions
# Other Functions used


# Extract Transition Probability for each level of the covariates at a given year ---------------------------------------------
# 1) Extract Transition Probability (Age as categorical, 3 categories)
predicted_TP_covs <- function(model, year, age_cat, age_var, sex, race, edu) {
  probs <- list()
  for (i in age_cat){
    for (j in sex){
      for (l in race){
        for (k in edu){
          
          # extract the probabilities
          probs[[paste(i,j,l,k)]] <- data.frame(print(pmatrix.msm(model, t=year, 
                                                covariates = list(age_var = i,   female_wave1.factor = j, 
                                                race_wave1.factor = l, edu3.factor = k)))) %>%
            
            # modify the output presentation
            mutate(From = row.names(.)) %>%
            pivot_longer(cols = -From, names_to = "To") %>%
            rename(Probability = value) %>%
            mutate(Transition = paste(From, To, sep = "->"),
              age_cat = i,
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











# 5) Extract Transition Probability (Age with 7 categories)
predicted_TP_covs5 <- function(model, year, age_cat, sex, race, edu) {
  probs <- list()
  for (i in age_cat){
    for (j in sex){
      for (l in race){
        for (k in edu){
          
          # extract the probabilities
          probs[[paste(i,j,l,k)]] <- data.frame(print(pmatrix.msm(model, t=year, 
                          covariates = list(age7 = i,   female_wave1.factor = j, race_wave1.factor = l, edu3.factor = k)))) %>%
            
            # modify the output presentation
            mutate(From = row.names(.)) %>%
            pivot_longer(cols = -From, names_to = "To") %>%
            rename(Probability = value) %>%
            mutate(Transition = paste(From, To, sep = "->"),
              age_cat = i,
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







# Simulate Transitions -------------------------------------------------------------------

# 1) Simulate transitions (Age stratified - no age)
simulate_pop2 <- function(initial_pop, aTP, apply_transitions, years) {
  pop <- initial_pop
  for (i in 1:years) {
    pop <- pop %>% 
      mutate(year= i,
        cat = paste(sex, edu, race, predicted_var, sep="_"),
        prob = runif(nrow(.))) %>%  # generate random prob
      group_by(cat) %>%
      do(apply_transitions(., aTP)) %>% # use 'do( )' to run the function defined earlier
      ungroup() %>% 
      select (-cat, -prob) 
  }
  return(pop)
}
      
# 2) Simulate transitions (Age continuous)      
simulate_pop3 <- function(initial_pop, aTP, apply_transitions, years) {
  pop <- initial_pop
  for (i in 1:years) {
    pop <- pop %>% 
      mutate(year= i,
        # age = age + 1,
        cat = paste(age, sex, edu, race, predicted_var, sep="_"),
        prob = runif(nrow(.))) %>%  # generate random prob
      group_by(cat) %>%
        do(apply_transitions(., aTP)) %>% # use 'do( )' to run the function defined earlier
      ungroup() %>% 
      select (-cat, -prob) %>% 
      filter (age<90)
  }
  return(pop)
}
      
      





  
  
  
