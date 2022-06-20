#' Loads eduacation transition rate data for SIMAH or CASCADE versions
#'
#' @param
#' @keywords education transition rates
#' @export
#' @examples
#' load_education_transitions
load_education_transitions <- function(SelectedState, basepop, brfss, WorkingDirectory){
  # allocate basepop and migrants a "tunnel state" within some college cat - dependent on age/sex and race
  if(SelectedState=="USA"){
    somec <- read.csv(paste0(WorkingDirectory,"somecollege_ACS.csv"))
  }else{
    somec <- read.csv(paste0(WorkingDirectory,"somecollege_ACS_states.csv")) %>% filter(STATE==SelectedState) %>%
      dplyr::select(-STATE)
  }

  somec <- somec %>% rename(microsim.init.sex=SEX,
                            microsim.init.age=AGE,
                            microsim.init.race=RACE) %>%
    mutate(microsim.init.sex=ifelse(microsim.init.sex=="F","f","m")) %>%
    mutate(agecat = cut(microsim.init.age,
                        breaks=c(0,18,19,20,21,24,29,34,39,44,49,54,59,64,100),
                        labels=c("18","19","20","21","22-24","25-29","30-34","35-39","40-44","45-49",
                                 "50-54","55-59","60-64","65+")),
           cat=paste(microsim.init.sex,agecat,microsim.init.race, sep="")) %>%
    dplyr::select(cat, EDUCdetailed, percent)

  toimpute <- basepop %>% filter(microsim.init.education=="SomeC") %>%
    mutate(agecat = cut(microsim.init.age,
                        breaks=c(0,18,19,20,21,24,29,34,39,44,49,54,59,64,100),
                        labels=c("18","19","20","21","22-24","25-29","30-34","35-39","40-44","45-49",
                                 "50-54","55-59","60-64","65+")),
           cat=paste(microsim.init.sex, agecat,microsim.init.race, sep=""))
  cats <- unique(toimpute$cat)
  catlist <- list()
  for(i in cats){
    catlist[[paste(i)]] <- toimpute %>% filter(cat==i)
  }

  samplefunction <- function(data, somec, selected){
    if(grepl("18",selected)==TRUE){
      sampled <- rep("LEHS", times=nrow(data))
    }else{
      somec <- somec %>% filter(cat==selected)
      sampled <- sample(somec$EDUCdetailed, size=nrow(data), replace=T, prob=c(somec$percent))
    }
    return(sampled)
  }
  selected <- i
  neweduc <- list()
  for(i in cats){
    neweduc[[paste(i)]] <- samplefunction(catlist[[paste(i)]], somec, i)
  }

  for(i in cats){
    catlist[[paste(i)]]$microsim.init.education <- neweduc[[paste(i)]]
  }

  catlist <- do.call(rbind,catlist)
  catlist$cat <- NULL
  basepop <- basepop %>% filter(microsim.init.education!="SomeC")
  basepop <- rbind(basepop, catlist)

  summary(as.factor(basepop$microsim.init.education))
  basepop$microsimnewED <- basepop$microsim.init.education
  basepop$microsim.init.education <- ifelse(basepop$microsim.init.education=="SomeC1"|
                                              basepop$microsim.init.education=="SomeC2"|
                                              basepop$microsim.init.education=="SomeC3","SomeC",basepop$microsimnewED)


  toimpute <- brfss %>% filter(microsim.init.education=="SomeC") %>% mutate(agecat = cut(microsim.init.age,
                                                                                         breaks=c(0,18,19,20,21,24,29,34,39,44,49,54,59,64,100),
                                                                                         labels=c("18","19","20","21","22-24","25-29","30-34","35-39","40-44","45-49",
                                                                                                  "50-54","55-59","60-64","65+")),
                                                                            cat=paste(microsim.init.sex,
                                                                                      agecat,
                                                                                      microsim.init.race, sep=""))
  cats <- unique(toimpute$cat)
  catlist <- list()
  for(i in cats){
    catlist[[paste(i)]] <- toimpute %>% filter(cat==i)
  }

  neweduc <- list()
  for(i in cats){
    neweduc[[paste(i)]] <- samplefunction(catlist[[paste(i)]], somec, i)
  }

  for(i in cats){
    catlist[[paste(i)]]$microsim.init.education <- neweduc[[paste(i)]]
  }

  catlist <- do.call(rbind,catlist)
  catlist$cat <- NULL
  # catlist$agecat <- NULL
  brfss <- brfss %>% filter(microsim.init.education!="SomeC")
  brfss <- rbind(brfss, catlist)
  summary(as.factor(brfss$microsim.init.education))
  brfss$microsimnewED <- brfss$microsim.init.education
  brfss$microsim.init.education <- ifelse(brfss$microsim.init.education=="SomeC1"|
                                            brfss$microsim.init.education=="SomeC2"|
                                            brfss$microsim.init.education=="SomeC3","SomeC",brfss$microsimnewED)
  transitions <- readRDS(paste0(WorkingDirectory,"final_ed_transitions", SelectedState, ".RDS"))
list <- list(transitions, basepop, brfss)
return(list)
}






