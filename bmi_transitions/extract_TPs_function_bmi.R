# extract transition probabilities
extractTP_bmi <- function(model,combo){
  probs <- list()
  for(i in 1:nrow(combo_bmi)){
    agegroup <- combo_bmi$agegroup[i]
    sex <- combo_bmi$sex[i]
    racefinal <- combo_bmi$racefinal[i]
    educLAST <- combo_bmi$educLAST[i]
    probs[[paste(i)]] <- pmatrix.msm(model_bmi, covariates=list(sex=sex, 
                                                            agegroup=agegroup,
                                                            racefinal=racefinal,
                                                            educLAST = educLAST))
    probs[[paste(i)]] <- data.frame(unclass(probs[[paste(i)]]))
    probs[[paste(i)]]$StateFrom <- row.names(probs[[paste(i)]])
    probs[[paste(i)]] <- probs[[paste(i)]] %>% pivot_longer(cols=State.1:State.2,
                                                            names_to="StateTo", values_to="prob") %>% 
      mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
                                 endsWith(StateTo,"2") ~ "State 2")) %>% 
      mutate(agegroup=agegroup,
             sex=sex,
             racefinal=racefinal,
             educLAST = educLAST)
  }
  
  probs <- do.call(rbind,probs)

  probs <- probs  %>% mutate(Transition1 = ifelse(StateFrom=="State 1","BMI <30",
                                                  ifelse(StateFrom=="State 2", "BMI >=30",NA)),
                             Transition2 = ifelse(StateTo=="State 1","BMI <30",
                                                  ifelse(StateTo=="State 2", "BMI >=30",NA)),
                             Transition = paste(Transition1, "to",Transition2),
                             sex = ifelse(sex==1, "female","male"),
                             sex=as.factor(sex),
                             race=as.factor(racefinal),
                             age=as.factor(agegroup))
  
  return(probs)
}