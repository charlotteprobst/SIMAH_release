#' fix initial values of education at individual age - for education transitions calibration
#' @param
#' @keywords microsimulation markov model implausibility
#' @export
#' @examples
#' fix initial education
fix_initial_education <- function(basepop){
  targets <- read.csv("SIMAH_workplace/microsim/education_calibration/education_targets_indage.csv") %>%
    filter(AGE<=24) %>%
    mutate(AGECAT = cut(AGE,
                        breaks=c(0,18,19,20,21,22,23,24),
                        labels=c("18","19","20","21","22","23","24"))) %>%
    mutate_at(vars(YEAR, AGECAT, RACE, SEX, EDUC), as.factor) %>%
    group_by(YEAR, AGECAT, RACE, SEX, EDUC, .drop=FALSE) %>%
    summarise(TPop=sum(TPop)) %>%
    group_by(YEAR, AGECAT, RACE, SEX) %>%
    mutate(proptarget=TPop/sum(TPop),
           YEAR= as.integer(as.character(YEAR))) %>%
    mutate(EDUC = factor(EDUC, levels=c("LEHS","SomeC","College"))) %>%
    arrange(EDUC) %>%
    mutate(
      cat = paste0(YEAR, AGECAT,RACE,SEX)) %>%
    group_by(cat, EDUC) %>% summarise(proptarget=mean(proptarget)) %>%
    ungroup() %>% group_by(cat) %>%
    mutate(cumsum=cumsum(proptarget))

  toimpute <- basepop %>% filter(microsim.init.age<=24)

  toimpute <- toimpute %>%
    mutate(YEAR=YEAR,
           AGECAT = microsim.init.age,
           RACE = ifelse(microsim.init.race=="BLA","Black",
                         ifelse(microsim.init.race=="WHI","White",
                                ifelse(microsim.init.race=="SPA","Hispanic","Other"))),
           SEX = ifelse(microsim.init.sex=="m","Men","Women"),
           cat = paste0(YEAR, AGECAT,RACE,SEX)) %>% dplyr::select(-c(AGECAT, RACE, SEX))

  transition_ed <- function(data, transitions){
    selected <- unique(data$cat)
    rates <- transitions %>% filter(cat==selected)
    data$newED <- ifelse(data$prob<=rates$cumsum[1], "LEHS",
                         ifelse(data$prob<=rates$cumsum[2] & data$prob>rates$cumsum[1], "SomeC",
                                ifelse(data$prob<=rates$cumsum[3] & data$prob>rates$cumsum[2],"College",NA)))
    return(data)
  }
  toimpute$prob <- runif(nrow(toimpute))
  toimpute <- toimpute %>% ungroup() %>% group_by(cat) %>% do(transition_ed(., targets)) %>%
    mutate(microsim.init.education = newED) %>%
    ungroup() %>%
    dplyr::select(-c(cat, prob, newED))

  tojoin <- basepop %>% filter(microsim.init.age>=25)
  basepop <- rbind(toimpute, tojoin)
  return(basepop)
}
