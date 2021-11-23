
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



# Extract Transition Probability for each level of the covariates at a given year
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





# Function to apply alcohol consumption transition probabilities
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

      # Alcohol Use Simulate transitions 
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