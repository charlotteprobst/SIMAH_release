
# SIMAH - NESARC Alcohol Transitions
# Data Extraction (and minor edits)



# Table 2 - Extract Transition Probabilities (aTP) and correct CI to original sample size 

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
