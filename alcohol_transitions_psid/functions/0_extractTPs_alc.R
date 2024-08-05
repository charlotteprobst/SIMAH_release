# Functions to extract basic TPs (i.e. prob for whole sample from one state to another)
extractTPs_basic <- function(model, year) {
  
  table <- data.frame(print(pmatrix.msm(model, t=year, ci="norm")))
  rownames(table) <- c("Non-drinker", "Low risk", "Medium risk", "High risk")
  colnames(table) <- c("Non-drinker", "Low risk", "Medium risk", "High risk")
  table <- table %>%
    mutate(From = row.names(.)) %>%
    pivot_longer(cols = -From, names_to = "To") %>%
    separate(value, into=c("Estimate","Lower","Upper", NA), sep="\\(|\\,|\\)", convert=TRUE) %>%  # separated based on "(" "," and ")"  convert=TRUE names variables numeric
    mutate(
      newLower = round(Lower*100, digits=2),
      newUpper = round(Upper*100, digits=2),
      Estimate = round(Estimate*100, digits=2),
      EstimateCI = paste0(Estimate, " (", newLower, ", ", newUpper, ")")) %>%
    select (From, To, EstimateCI) %>%
    pivot_wider(names_from = "To", values_from = "EstimateCI")
  
  return(table)
}

extractTPs_subgroups <- function(model,combo){
  probs <- list()
  for(i in 1:nrow(combo)){
    age_cat <- combo$age_cat[i]
    sex <- combo$sex[i]
    race <- combo$race[i]
    education <- combo$education[i]
    probs[[paste(i)]] <- pmatrix.msm(model, covariates=list(age_cat = age_cat, sex = sex, race = race, education = education))
    probs[[paste(i)]] <- data.frame(unclass(probs[[paste(i)]]))
    probs[[paste(i)]]$StateFrom <- row.names(probs[[paste(i)]])
    probs[[paste(i)]] <- probs[[paste(i)]] %>% 
      pivot_longer(cols=State.1:State.4,names_to="StateTo", values_to="prob") %>% 
      mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
                                 endsWith(StateTo,"2") ~ "State 2",
                                 endsWith(StateTo,"3") ~ "State 3",
                                 endsWith(StateTo,"4") ~ "State 4"), 
      age_cat=age_cat,
             sex=sex,
             race=race,
             education=education)
  }
  probs <- do.call(rbind,probs) 
  return(probs)
}
# 
# extractTP_incl_time <- function(model,combo){
#   probs <- list()
#   for(i in 1:nrow(combo)){
#     timevary <- combo$timevary[i]
#     agecat <- combo$age_cat[i]
#     sex <- combo$sex[i]
#     race <- combo$race[i]
#     probs[[paste(i)]] <- pmatrix.msm(model, covariates=list(age_cat, sex, race, timevary))
#     probs[[paste(i)]] <- data.frame(unclass(probs[[paste(i)]]))
#     probs[[paste(i)]]$StateFrom <- row.names(probs[[paste(i)]])
#     probs[[paste(i)]] <- probs[[paste(i)]] %>% pivot_longer(cols=State.1:State.5,
#                                                             names_to="StateTo", values_to="prob") %>%
#       mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
#                                  endsWith(StateTo,"2") ~ "State 2",
#                                  endsWith(StateTo,"3") ~ "State 3",
#                                  endsWith(StateTo,"4") ~ "State 4")) %>%
#       mutate(time_period=timevary,
#              age=age_cat,
#              sex=sex,
#              race=race)
#   }
#   probs <- do.call(rbind,probs)
#   return(probs)
# }
# 
# extractTP_interaction_sex <- function(model,combo){
#   probs <- list()
#   for(i in 1:nrow(combo)){
#     timevary <- combo$timevary[i]
#     agecat <- combo$agecat[i]
#     racefinal2 <- combo$racefinal2[i]
#     probs[[paste(i)]] <- pmatrix.msm(model, covariates=list(agecat,racefinal2,timevary))
#     probs[[paste(i)]] <- data.frame(unclass(probs[[paste(i)]]))
#     probs[[paste(i)]]$StateFrom <- row.names(probs[[paste(i)]])
#     probs[[paste(i)]] <- probs[[paste(i)]] %>% pivot_longer(cols=State.1:State.5,
#                                                             names_to="StateTo", values_to="prob") %>%
#       mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
#                                  endsWith(StateTo,"2") ~ "State 2",
#                                  endsWith(StateTo,"3") ~ "State 3",
#                                  endsWith(StateTo,"4") ~ "State 4",
#                                  endsWith(StateTo,"5") ~ "State 5")) %>%
#       mutate(time_period=timevary,
#              age=agecat,
#              race=racefinal2)
#   }
#   probs <- do.call(rbind,probs)
#   return(probs)
# }
# 
# extractTP_interaction_race <- function(model,combo){
#   probs <- list()
#   for(i in 1:nrow(combo)){
#     timevary <- combo$timevary[i]
#     agecat <- combo$agecat[i]
#     sex <- combo$sex[i]
#     probs[[paste(i)]] <- pmatrix.msm(model, covariates=list(agecat,sex,timevary))
#     probs[[paste(i)]] <- data.frame(unclass(probs[[paste(i)]]))
#     probs[[paste(i)]]$StateFrom <- row.names(probs[[paste(i)]])
#     probs[[paste(i)]] <- probs[[paste(i)]] %>% pivot_longer(cols=State.1:State.5,
#                                                             names_to="StateTo", values_to="prob") %>%
#       mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
#                                  endsWith(StateTo,"2") ~ "State 2",
#                                  endsWith(StateTo,"3") ~ "State 3",
#                                  endsWith(StateTo,"4") ~ "State 4",
#                                  endsWith(StateTo,"5") ~ "State 5")) %>%
#       mutate(time_period=timevary,
#              age=agecat,
#              sex=sex)
#   }
#   probs <- do.call(rbind,probs)
#   return(probs)
# }
# 
# 
# extractTP_interaction_race_sex <- function(model,combo){
#   probs <- list()
#   for(i in 1:nrow(combo)){
#     timevary <- combo$timevary[i]
#     agecat <- combo$agecat[i]
#     probs[[paste(i)]] <- pmatrix.msm(model, covariates=list(agecat,timevary))
#     probs[[paste(i)]] <- data.frame(unclass(probs[[paste(i)]]))
#     probs[[paste(i)]]$StateFrom <- row.names(probs[[paste(i)]])
#     probs[[paste(i)]] <- probs[[paste(i)]] %>% pivot_longer(cols=State.1:State.5,
#                                                             names_to="StateTo", values_to="prob") %>%
#       mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
#                                  endsWith(StateTo,"2") ~ "State 2",
#                                  endsWith(StateTo,"3") ~ "State 3",
#                                  endsWith(StateTo,"4") ~ "State 4",
#                                  endsWith(StateTo,"5") ~ "State 5")) %>%
#       mutate(time_period=timevary,
#              age=agecat)
#   }
#   probs <- do.call(rbind,probs)
#   return(probs)
# }
# 
# 
# extractTP_combo_time <- function(model,combo){
#   probs <- list()
#   for(i in 1:nrow(combo)){
#     timevary <- combo$timevary[i]
#     probs[[paste(i)]] <- pmatrix.msm(model, covariates=list(timevary))
#     probs[[paste(i)]] <- data.frame(unclass(probs[[paste(i)]]))
#     probs[[paste(i)]]$StateFrom <- row.names(probs[[paste(i)]])
#     probs[[paste(i)]] <- probs[[paste(i)]] %>% pivot_longer(cols=State.1:State.5,
#                                                             names_to="StateTo", values_to="prob") %>%
#       mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
#                                  endsWith(StateTo,"2") ~ "State 2",
#                                  endsWith(StateTo,"3") ~ "State 3",
#                                  endsWith(StateTo,"4") ~ "State 4",
#                                  endsWith(StateTo,"5") ~ "State 5")) %>%
#       mutate(time_period=timevary)
#   }
#   probs <- do.call(rbind,probs)
#   return(probs)
# }


