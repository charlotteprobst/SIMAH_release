#####imputing the "correct" ages in individual years into the base population 
microsim <- microsim %>% 
  mutate(agecat = cut(microsim.init.age, breaks=c(0,24,34,44,54,64,79),
                      labels=c("18-24","25-34","35-44","45-54","55-64","65-79")),
         CAT = paste(microsim.init.race, microsim.init.sex, microsim.init.education, agecat, sep="_")) 

###now try and replicate the actual age profile in the census (at the moment has been constrained by categories)
indages <- read.csv("SIMAH_workplace/microsim/1_generating_population/agebyeducationdistributions.csv") %>% filter(STATE==State) %>% 
  mutate(CAT = paste(race, sex, education, agecat, sep="_")) %>% 
  select(CAT, agecat, age, percent) %>% separate(agecat, c("lower","upper"),sep=3, remove=FALSE) %>% 
  mutate(lower = parse_number(lower), upper=as.numeric(upper))

tosample <- microsim %>% group_by(CAT) %>% tally(name="tosample") %>% distinct()
microsim <- microsim %>% group_by(CAT) %>% add_tally(name="tosample")
indages <- left_join(indages, tosample)

sampleFUN <- function(data){
    cat <- unique(data$CAT)
    n <- unique(data$tosample)
    probs <- read.csv("SIMAH_workplace/microsim/1_generating_population/agebyeducationdistributions.csv") %>% filter(STATE==State) %>% 
      mutate(CAT = paste(race, sex, education, agecat, sep="_")) %>% 
      filter(CAT==cat) %>% 
      select(CAT, agecat, age, percent) %>% separate(agecat, c("lower","upper"),sep=3, remove=FALSE) %>% 
      mutate(lower = parse_number(lower), upper=as.numeric(upper))
    data$newAGE <- sample(unique(probs$lower):unique(probs$upper), size=n,
                                     prob = probs$percent, replace=T)
    return(data)
}

cats <- list()
for(i in unique(microsim$CAT)){
  cats[[paste(i)]] <- subset(microsim, CAT==i)
}

newage <- lapply(cats, sampleFUN)
newage <- do.call(rbind, newage)

####clean the dataset 
microsim <- newage %>% mutate(microsim.init.age=newAGE) %>% 
  ungroup() %>%
  select(-c(CAT, newAGE, tosample))

rm(list=setdiff(ls(), c("microsim", "cons", c(tokeep))))


