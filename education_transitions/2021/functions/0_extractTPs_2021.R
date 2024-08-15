# Functions to extract basic TPs (i.e. prob for whole sample from one state to another)
extractTPs_basic <- function(model, year) {
  
  table <- data.frame(print(pmatrix.msm(model, t=year, ci="norm")))
  rownames(table) <- c("LEHS", "1 year college", "2 years college", "3 years college", "college degree +")
  colnames(table) <- c("LEHS", "1 year college", "2 years college", "3 years college", "college degree +")
  table <- table %>%
    mutate(From = row.names(.)) %>%
    pivot_longer(cols = -From, names_to = "To") %>%
    separate(value, into=c("Estimate","Lower","Upper", NA), sep="\\(|\\,|\\)", convert=TRUE) %>%  # separated based on "(" "," and ")"  convert=TRUE names variables numeric
    mutate(
      newLower = round(Lower*100, digits=2),
      newUpper = round(Upper*100, digits=2),
      Estimate = round(Estimate*100, digits=2),
      EstimateCI = paste0(Estimate, " (", newLower, ", ", newUpper, ")")) %>%
    dplyr::select(From, To, EstimateCI) %>%
    pivot_wider(names_from = "To", values_from = "EstimateCI")
  
  return(table)
}

extractTPs_subgroups <- function(model,combo){
  probs <- list()
  for (i in 1:nrow(combo)) {
    agecat <- combo$agecat[i]
    sex <- combo$sex[i]
    racefinal2 <- combo$racefinal2[i]
    prob_matrix <- pmatrix.msm(model, covariates = list(agecat = agecat, sex = sex, racefinal2 = racefinal2), ci = "norm")
    prob_df <- data.frame(unclass(prob_matrix))
    prob_df$StateFrom <- row.names(prob_df)
    prob_df$agecat <- agecat
    prob_df$sex <- sex
    prob_df$racefinal2 <- racefinal2
        probs[[paste(i)]] <- prob_df
  }
  
  combined_probs <- do.call(rbind, probs)
  
  probs_long <- combined_probs %>%
    pivot_longer(
      cols = starts_with("estimates.State.") | starts_with("L.") | starts_with("U."),
      names_to = c(".value", "StateTo"),
      names_pattern = "(.*?)\\.(\\d+)",
      values_to = "count")
  return(probs_long)
}


extractTP_incl_time <- function(model,combo){
  probs <- list()
  for(i in 1:nrow(combo)){
    timevary <- combo$timevary[i]
    agecat <- combo$agecat[i]
    sex <- combo$sex[i]
    racefinal2 <- combo$racefinal2[i]
    probs[[paste(i)]] <- pmatrix.msm(model, covariates=list(agecat, sex, racefinal2, timevary))
    probs[[paste(i)]] <- data.frame(unclass(probs[[paste(i)]]))
    probs[[paste(i)]]$StateFrom <- row.names(probs[[paste(i)]])
    probs[[paste(i)]] <- probs[[paste(i)]] %>% pivot_longer(cols=State.1:State.5,
                                                            names_to="StateTo", values_to="prob") %>%
      mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
                                 endsWith(StateTo,"2") ~ "State 2",
                                 endsWith(StateTo,"3") ~ "State 3",
                                 endsWith(StateTo,"4") ~ "State 4",
                                 endsWith(StateTo,"5") ~ "State 5")) %>%
      mutate(time_period=timevary,
             age=agecat,
             sex=sex,
             race=racefinal2)
  }
  probs <- do.call(rbind,probs)
  return(probs)
}

extractTP_interaction_sex <- function(model,combo){
  probs <- list()
  for(i in 1:nrow(combo)){
    timevary <- combo$timevary[i]
    agecat <- combo$agecat[i]
    racefinal2 <- combo$racefinal2[i]
    probs[[paste(i)]] <- pmatrix.msm(model, covariates=list(agecat,racefinal2,timevary))
    probs[[paste(i)]] <- data.frame(unclass(probs[[paste(i)]]))
    probs[[paste(i)]]$StateFrom <- row.names(probs[[paste(i)]])
    probs[[paste(i)]] <- probs[[paste(i)]] %>% pivot_longer(cols=State.1:State.5,
                                                            names_to="StateTo", values_to="prob") %>%
      mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
                                 endsWith(StateTo,"2") ~ "State 2",
                                 endsWith(StateTo,"3") ~ "State 3",
                                 endsWith(StateTo,"4") ~ "State 4",
                                 endsWith(StateTo,"5") ~ "State 5")) %>%
      mutate(time_period=timevary,
             age=agecat,
             race=racefinal2)
  }
  probs <- do.call(rbind,probs)
  return(probs)
}

extractTP_interaction_race <- function(model,combo){
  probs <- list()
  for(i in 1:nrow(combo)){
    timevary <- combo$timevary[i]
    agecat <- combo$agecat[i]
    sex <- combo$sex[i]
    probs[[paste(i)]] <- pmatrix.msm(model, covariates=list(agecat,sex,timevary))
    probs[[paste(i)]] <- data.frame(unclass(probs[[paste(i)]]))
    probs[[paste(i)]]$StateFrom <- row.names(probs[[paste(i)]])
    probs[[paste(i)]] <- probs[[paste(i)]] %>% pivot_longer(cols=State.1:State.5,
                                                            names_to="StateTo", values_to="prob") %>%
      mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
                                 endsWith(StateTo,"2") ~ "State 2",
                                 endsWith(StateTo,"3") ~ "State 3",
                                 endsWith(StateTo,"4") ~ "State 4",
                                 endsWith(StateTo,"5") ~ "State 5")) %>%
      mutate(time_period=timevary,
             age=agecat,
             sex=sex)
  }
  probs <- do.call(rbind,probs)
  return(probs)
}


extractTP_interaction_race_sex <- function(model,combo){
  probs <- list()
  for(i in 1:nrow(combo)){
    timevary <- combo$timevary[i]
    agecat <- combo$agecat[i]
    probs[[paste(i)]] <- pmatrix.msm(model, covariates=list(agecat,timevary))
    probs[[paste(i)]] <- data.frame(unclass(probs[[paste(i)]]))
    probs[[paste(i)]]$StateFrom <- row.names(probs[[paste(i)]])
    probs[[paste(i)]] <- probs[[paste(i)]] %>% pivot_longer(cols=State.1:State.5,
                                                            names_to="StateTo", values_to="prob") %>%
      mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
                                 endsWith(StateTo,"2") ~ "State 2",
                                 endsWith(StateTo,"3") ~ "State 3",
                                 endsWith(StateTo,"4") ~ "State 4",
                                 endsWith(StateTo,"5") ~ "State 5")) %>%
      mutate(time_period=timevary,
             age=agecat)
  }
  probs <- do.call(rbind,probs)
  return(probs)
}


extractTP_combo_time <- function(model,combo){
  probs <- list()
  for(i in 1:nrow(combo)){
    timevary <- combo$timevary[i]
    probs[[paste(i)]] <- pmatrix.msm(model, covariates=list(timevary))
    probs[[paste(i)]] <- data.frame(unclass(probs[[paste(i)]]))
    probs[[paste(i)]]$StateFrom <- row.names(probs[[paste(i)]])
    probs[[paste(i)]] <- probs[[paste(i)]] %>% pivot_longer(cols=State.1:State.5,
                                                            names_to="StateTo", values_to="prob") %>%
      mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
                                 endsWith(StateTo,"2") ~ "State 2",
                                 endsWith(StateTo,"3") ~ "State 3",
                                 endsWith(StateTo,"4") ~ "State 4",
                                 endsWith(StateTo,"5") ~ "State 5")) %>%
      mutate(time_period=timevary)
  }
  probs <- do.call(rbind,probs)
  return(probs)
}


