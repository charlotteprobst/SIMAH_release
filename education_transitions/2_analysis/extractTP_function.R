# extract transition probabilities
extractTP <- function(model,combo,mapping){
  probs <- list()
  for(i in 1:nrow(combo)){
    selectedage <- combo$age[i]
    selectedsex <- combo$sex[i]
    selectedrace <- combo$race[i]
    selectedcollege <- combo$oneCollegeplus[i]
    agescaledvar <- as.numeric((mapping %>% filter(age==selectedage))$agescaled)
    agesqvar <- as.numeric((mapping %>% filter(age==selectedage))$agesqscaled)
    probs[[paste(i)]] <- pmatrix.msm(model, covariates=list(sex=selectedsex, agescaled=agescaledvar,
                                                                    agesqscaled=agesqvar,
                                                                    racefinal2=selectedrace,
                                                            oneCollegeplus = selectedcollege))
        probs[[paste(i)]] <- data.frame(unclass(probs[[paste(i)]]))
        probs[[paste(i)]]$StateFrom <- row.names(probs[[paste(i)]])
        probs[[paste(i)]] <- probs[[paste(i)]] %>% pivot_longer(cols=State.1:State.5,
                                                                        names_to="StateTo", values_to="prob") %>% 
          mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
                                     endsWith(StateTo,"2") ~ "State 2",
                                     endsWith(StateTo,"3") ~ "State 3",
                                     endsWith(StateTo,"4") ~ "State 4",
                                     endsWith(StateTo,"5") ~ "State 5")) %>% 
          mutate(age=selectedage,
                 sex=selectedsex,
                 racefinal=selectedrace,
                 oneCollegeplus = selectedcollege)
        
      }
  probs <- do.call(rbind,probs)
  probs <- probs  %>% mutate(Transition1 = ifelse(StateFrom=="State 1","LEHS",
                                                  ifelse(StateFrom=="State 2","SomeC1",
                                                         ifelse(StateFrom=="State 3","SomeC2",
                                                                ifelse(StateFrom=="State 4","SomeC3",
                                                                       ifelse(StateFrom=="State 5","College",NA))))),
                             Transition2 = ifelse(StateTo=="State 1","LEHS",
                                                  ifelse(StateTo=="State 2","SomeC1",
                                                         ifelse(StateTo=="State 3","SomeC2",
                                                                ifelse(StateTo=="State 4","SomeC3",
                                                                       ifelse(StateTo=="State 5","College",NA))))),
                             Transition = paste(Transition1, "->",Transition2, sep=""),
                             sex = ifelse(sex==1, "female","male"),
                             sex=as.factor(sex),
                             racefinal=as.factor(racefinal),
                             age=as.factor(age))
  return(probs)
}
