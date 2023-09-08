# extract transition probabilities
extractTP_old <- function(model,combo){
  probs <- list()
  for(i in 1:nrow(combo)){
    selectedsex <- combo$female_wave1.factor[i]
    selectedage <- combo$age7[i]
    selectededu <- combo$edu3.factor[i]
    selectedrace <- combo$race_wave1.factor[i]
    probs[[paste(i)]] <- pmatrix.msm(model, covariates=list(female_wave1.factor=selectedsex, 
                                                            age7=selectedage, edu3.factor=selectededu,
                                                            race_wave1.factor=selectedrace))
        probs[[paste(i)]] <- data.frame(unclass(probs[[paste(i)]]))
        probs[[paste(i)]]$StateFrom <- row.names(probs[[paste(i)]])
        probs[[paste(i)]] <- probs[[paste(i)]] %>% pivot_longer(cols=State.1:State.4,
                                                                        names_to="StateTo", values_to="Probability") %>% 
          mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "Non-drinker",
                                     endsWith(StateTo,"2") ~ "Low risk",
                                     endsWith(StateTo,"3") ~ "Medium risk",
                                     endsWith(StateTo,"4") ~ "High risk")) %>% 
          mutate(age_cat=selectedage,
                 sex=selectedsex,
                 race=selectedrace,
                 edu = selectededu)
        
      }
  probs <- do.call(rbind,probs)
  probs <- probs  %>% mutate(StateFrom = ifelse(StateFrom=="State 1","Non-drinker",
                                                  ifelse(StateFrom=="State 2","Low risk",
                                                         ifelse(StateFrom=="State 3","Medium risk",
                                                                ifelse(StateFrom=="State 4","High risk",NA)))),
                             race = ifelse(race=="White, non-Hispanic", "WHI",
                                           ifelse(race=="Black, non-Hispanic","BLA",
                                                  ifelse(race=="Hispanic","SPA", "OTH"))),
                             edu = ifelse(edu=="Low","LEHS",
                                          ifelse(edu=="Med", "SomeC", "College")),
                             sex = ifelse(sex=="Men","m","f"),
                             cat = paste(age_cat, sex, race, edu, StateFrom, sep="_")) %>% 
    dplyr::select(cat, StateTo, Probability) %>% 
    group_by(cat) %>% mutate(cumsum = cumsum(Probability)) %>% dplyr::select(-Probability)
    return(probs)
}

# extract transition probabilities
extractTP_new <- function(model,combo){
  probs <- list()
  for(i in 1:nrow(combo)){
    selectedsex <- combo$female_w1[i]
    selectedage <- combo$age7[i]
    selectededu <- combo$edu3[i]
    selectedrace <- combo$race_w1[i]
    probs[[paste(i)]] <- pmatrix.msm(model, covariates=list(female_w1=selectedsex, 
                                                            age7=selectedage, edu3=selectededu,
                                                            race_w1=selectedrace))
    probs[[paste(i)]] <- data.frame(unclass(probs[[paste(i)]]))
    probs[[paste(i)]]$StateFrom <- row.names(probs[[paste(i)]])
    probs[[paste(i)]] <- probs[[paste(i)]] %>% pivot_longer(cols=State.1:State.4,
                                                            names_to="StateTo", values_to="Probability") %>% 
      mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "Non-drinker",
                                 endsWith(StateTo,"2") ~ "Low risk",
                                 endsWith(StateTo,"3") ~ "Medium risk",
                                 endsWith(StateTo,"4") ~ "High risk")) %>% 
      mutate(age_cat=selectedage,
             sex=selectedsex,
             race=selectedrace,
             edu = selectededu)
    
  }
  probs <- do.call(rbind,probs)
  probs <- probs  %>% mutate(StateFrom = ifelse(StateFrom=="State 1","Non-drinker",
                                                ifelse(StateFrom=="State 2","Low risk",
                                                       ifelse(StateFrom=="State 3","Medium risk",
                                                              ifelse(StateFrom=="State 4","High risk",NA)))),
                             race = ifelse(race=="White, non-Hispanic", "WHI",
                                           ifelse(race=="Black, non-Hispanic","BLA",
                                                  ifelse(race=="Hispanic","SPA", "OTH"))),
                             edu = ifelse(edu=="Low","LEHS",
                                          ifelse(edu=="Med", "SomeC", "College")),
                             sex = ifelse(sex=="Men","m","f"),
                             cat = paste(age_cat, sex, race, edu, StateFrom, sep="_")) %>% 
    dplyr::select(cat, StateTo, Probability) %>% 
    group_by(cat) %>% mutate(cumsum = cumsum(Probability)) %>% dplyr::select(-Probability)
  exreturn(probs)
}
