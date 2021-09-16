# extract transition probabilities
extractTP <- function(model,age,sex,race,mapping){
  probs <- list()
  for(i in age){
    for(j in sex){{
      for(l in race){
        agescaledvar <- (mapping %>% filter(age==i))$agescaled
        agesqvar <- (mapping %>% filter(age==i))$agesqscaled
        probs[[paste(i,j,l)]] <- pmatrix.msm(model, covariates=list(sex=j, agescaled=agescaledvar,
                                                                    agesqscaled=agesqvar,
                                                                    racefinal=paste(l)))
        probs[[paste(i,j,l)]] <- data.frame(unclass(probs[[paste(i,j,l)]]))
        probs[[paste(i,j,l)]]$StateFrom <- row.names(probs[[paste(i,j,l)]])
        probs[[paste(i,j,l)]] <- probs[[paste(i,j,l)]] %>% pivot_longer(cols=State.1:State.5,
                                                                        names_to="StateTo", values_to="prob") %>% 
          mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
                                     endsWith(StateTo,"2") ~ "State 2",
                                     endsWith(StateTo,"3") ~ "State 3",
                                     endsWith(StateTo,"4") ~ "State 4",
                                     endsWith(StateTo,"5") ~ "State 5")) %>% 
          mutate(age=i,
                 sex=j,
                 racefinal=l)
        
      }
    }
    }
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
