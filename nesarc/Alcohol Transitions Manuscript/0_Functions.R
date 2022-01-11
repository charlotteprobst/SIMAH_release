
# SIMAH - NESARC Alcohol Transitions
# Functions used


# Table 2 - Extract Transition Probabilities (TP) and correct CI to original sample size 

predicted_TP <- function(model, year, original_n, expanded_n) {
  
  table <- data.frame(print(pmatrix.msm(model, t=year, ci="norm"))) %>%
    mutate(From = row.names(.)) %>%
    pivot_longer(cols = -From, names_to = "To") %>%
    separate(value, into=c("Estimate","Lower","Upper", NA), sep="\\(|\\,|\\)", convert=TRUE) %>%  # separated based on "(" "," and  ")"  convert=TRUE names variables numeric
    mutate(
      SE = (Upper - Lower) / 3.92,
      SD = SE * sqrt(expanded_n),  
      newSE = SD / sqrt(original_n), 
      newLower = round((Estimate - (newSE * 1.96))*100, digits=1),
      newUpper = round((Estimate + (newSE * 1.96))*100, digits=1),
      Estimate = round(Estimate*100, digits=1), 
      EstimateCI = paste0(Estimate, " (", newLower, ", ", newUpper, ")")) %>%
    select (From, To, EstimateCI) %>%
    pivot_wider(names_from = "To", values_from = "EstimateCI")
  
  return(table)
}




# Table 3 - Extract HR results, rearrange, and correct CI to original sample size 
HR_table <- function (model, original_n, expanded_n){
  
  table <- data.frame(hazard.msm(model))%>%
    mutate(transition = row.names(.)) %>%
    pivot_longer(cols=-transition) %>%
    extract(name, into=c("Variable","Type"), regex="(.*)\\.(.*)") %>%   # Separate the string (name) into the variable and type of estimate (HR, Upper, Lower), separate at last occuring period .
    pivot_wider(names_from="Type", values_from = "value") %>%
    mutate(
      SE=(log(U) - log(L))/3.92, 
      SD = SE * sqrt(expanded_n),  
      newSE = SD / sqrt(original_n),
      newLower = round(exp((log(HR)) - (newSE * 1.96)), digits=2), 
      newUpper = round(exp((log(HR)) + (newSE * 1.96)), digits=2), 
      HR = round(HR, digits=2),
      EstimateCI = paste0(HR, " (", newLower, ", ", newUpper, ")")) %>%
    select(transition, Variable, EstimateCI) %>%
    pivot_wider(names_from = "transition", values_from = EstimateCI) 

  return (table) 
}


# Extract Transition Probability for each level of the covariates at a given year ---------------------------------------------
# 1) Extract Transition Probability (Age as categorical)
predicted_TP_covs <- function(model, year, age_cat, sex, race, edu) {
  probs <- list()
  for (i in age_cat){
    for (j in sex){
      for (l in race){
        for (k in edu){
          
          # extract the probabilities
          probs[[paste(i,j,l,k)]] <- data.frame(print(pmatrix.msm(model, t=year, 
                                                covariates = list(age3.factor = i,   female_wave1.factor = j, 
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




# 2) Extract Transition Probability (Age not included; stratified by age)
predicted_TP_covs2 <- function(model, year, sex, race, edu) {
  probs <- list()
    for (j in sex){
      for (l in race){
        for (k in edu){
          
          # extract the probabilities
          probs[[paste(j,l,k)]] <- data.frame(print(pmatrix.msm(model, t=year, 
            covariates = list(female_wave1.factor = j, race_wave1.factor = l, edu3.factor = k)))) %>%
            
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




# 3) Extract Transition Probability (Age as continuous)
predicted_TP_covs3 <- function(model, year, age, sex, race, edu) {
  probs <- list()
  for (i in age){
    for (j in sex){
      for (l in race){
        for (k in edu){
          
          age.c_var <- (mapping%>%filter(age==i))$age.c
          
          # extract the probabilities
          probs[[paste(i,j,l,k)]] <- data.frame(print(pmatrix.msm(model, t=year, 
                                                covariates = list(age.c = age.c_var,   
                                                  female_wave1.factor = j, 
                                                  race_wave1.factor = l, edu3.factor = k)))) %>%
            
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




# 4) Extract Transition Probability (Age squared)
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
            covariates = list(age.c = age.c_var, age_sq.c = age_sq.c_var,    
              female_wave1.factor = j, 
              race_wave1.factor = l, edu3.factor = k)))) %>%
            
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






# Function to apply alcohol consumption transition probabilities -------------------------------------------------------

# 1) Apply alcohol consumption - alcUse 5 categories
transition_alc5 <- function(data, transitions){
  selected <- unique(data$cat)
  rates <- transitions %>% filter(cat == selected)
  data$AlcUse_pred <- ifelse(data$prob<=rates$cumsum[1], "Abstainer",
    ifelse(data$prob<=rates$cumsum[2] & data$prob>rates$cumsum[1], "Former",
      ifelse(data$prob<=rates$cumsum[3] & data$prob>rates$cumsum[2],"Category I",
        ifelse(data$prob<=rates$cumsum[4] & data$prob>rates$cumsum[3],"Category II",
          ifelse(data$prob<=rates$cumsum[5] & data$prob>rates$cumsum[4],"Category III",NA)))))
  return(data)
}

# 2) Apply alcohol consumption - alcUse 4 categories
transition_alc4 <- function(data, transitions){
  selected <- unique(data$cat)
  rates <- transitions %>% filter(cat == selected)
  data$AlcUse_pred <- ifelse(data$prob<=rates$cumsum[1], "Non-drinker",
    ifelse(data$prob<=rates$cumsum[2] & data$prob>rates$cumsum[1], "Category I",
      ifelse(data$prob<=rates$cumsum[3] & data$prob>rates$cumsum[2],"Category II",
        ifelse(data$prob<=rates$cumsum[4] & data$prob>rates$cumsum[3],"Category III",NA))))
  return(data)
}


# Simulate Transitions -------------------------------------------------------------------

# 1) Simulate transitions - AlcUse 5 categotries
alc_sim <- function(years) {
  for (i in 1:years) {
    AlcUse_basepop <- AlcUse_basepop %>% 
      mutate(year= i,
        age = age + 1,
        age_cat = case_when(age < 30 ~ "18-29",
          age >=30 & age < 50 ~ "30-49",
          age >= 50 ~ "50+"),
        cat = paste(sex, age_cat, edu, race, AlcUse_pred, sep="_"),
        prob = runif(nrow(.))) %>%  # generate random prob
      group_by(cat) %>%
        do(transition_alc5(., aTP_alc5)) %>% # use 'do( )' to run the function defined earlier
      ungroup() %>% 
      select (-cat, -prob) 
  }
  return(AlcUse_basepop)
}


# 2) Simulate transitions - AlcUse 4 categotries
alc4_sim <- function(years) {
  for (i in 1:years) {
    AlcUse4_basepop <- AlcUse4_basepop %>% 
      mutate(year= i,
        age = age + 1,
        age_cat = case_when(age < 30 ~ "18-29",
          age >=30 & age < 50 ~ "30-49",
          age >= 50 ~ "50+"),
        cat = paste(sex, age_cat, edu, race, AlcUse_pred, sep="_"),
        prob = runif(nrow(.))) %>%  # generate random prob
      group_by(cat) %>%
      do(transition_alc4(., aTP_alc4)) %>% # use 'do( )' to run the function defined earlier
      ungroup() %>% 
      select (-cat, -prob) 
  }
  return(AlcUse4_basepop)
}




# 3) Simulate transitions (Age stratified)
alc_sim2 <- function(basepop, aTP, apply_transitions, years) {
  for (i in 1:years) {
    AlcUse_basepop <- basepop %>% 
      mutate(year= i,
        cat = paste(sex, edu, race, AlcUse_pred, sep="_"),
        prob = runif(nrow(.))) %>%  # generate random prob
      group_by(cat) %>%
      do(apply_transitions(., aTP)) %>% # use 'do( )' to run the function defined earlier
      ungroup() %>% 
      select (-cat, -prob) 
  }
  return(AlcUse_basepop)
}
      


# 4) Simulate transitions (Age continuous)      
alc_sim3 <- function(basepop, aTP, apply_transitions, years) {
  for (i in 1:years) {
    AlcUse_basepop <- basepop %>% 
      mutate(year= i,
        age = age + 1,
        cat = paste(age, sex, edu, race, AlcUse_pred, sep="_"),
        prob = runif(nrow(.))) %>%  # generate random prob
      group_by(cat) %>%
      do(apply_transitions(., aTP)) %>% # use 'do( )' to run the function defined earlier
      ungroup() %>% 
      select (-cat, -prob) %>% 
      filter (age<90)
  }
  return(AlcUse_basepop)
}
      
      
      
      

# HED ---------------------------------------------------------------------------------------------------      
  
# Function to apply HED transition probabilities
transition_hed <- function(data, transitions){
  selected <- unique(data$cat)
  rates <- transitions %>% filter(cat == selected)
  data$hed_pred <- ifelse(data$prob <= rates$cumsum[1], "Non-drinker",
    ifelse(data$prob <= rates$cumsum[2] & data$prob > rates$cumsum[1], "Drinker, no HED",
      ifelse(data$prob <= rates$cumsum[3] & data$prob > rates$cumsum[2],"Occasional HED",
        ifelse(data$prob <= rates$cumsum[4] & data$prob > rates$cumsum[3],"Monthly HED",
          ifelse(data$prob <= rates$cumsum[5] & data$prob > rates$cumsum[4],"Weekly HED",NA)))))
  return(data)
}

# HED Simulate transitions 
hed_sim <- function(years) {
  for (i in 1:years) {
    hed_basepop <- hed_basepop %>% 
      mutate( year= i,
              age = age + 1,
              age_cat = case_when(age < 30 ~ "18-29",
                                  age >=30 & age < 50 ~ "30-49",
                                  age >= 50 ~ "50+"),
              cat = paste(sex, age_cat, edu, race, hed_pred, sep="_"),
              prob = runif(nrow(.))) %>%  # generate random prob
      group_by(cat) %>%
      do(transition_hed(., aTP_hed)) %>% # use 'do( )' to run the function defined earlier
      ungroup() %>% 
      select (-cat, -prob) 
  }
  return(hed_basepop)
}




