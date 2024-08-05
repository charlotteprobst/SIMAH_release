#' Loads eduacation transition rate data for SIMAH or CASCADE versions
#'
#' @param
#' @keywords education transition rates
#' @export
#' @examples
#' load_education_transitions
fix_years_somecollege <- function(basepop){
  # allocate basepop and migrants a "tunnel state" within some college cat - dependent on age/sex and race
  somec <- read.csv(paste0("SIMAH_workplace/microsim/education_calibration/somecollege_ACS.csv"))

  somec <- somec %>% rename(sex=SEX,
                            age=AGE,
                            race=RACE) %>%
    mutate(sex=ifelse(sex=="Women","f","m"),
           race=ifelse(race=="Other","Others",race)) %>%
    mutate(agecat = cut(age,
                        breaks=c(0,18,19,20,21,24,29,34,39,44,49,54,59,64,100),
                        labels=c("18","19","20","21","22-24","25-29","30-34","35-39","40-44","45-49",
                                 "50-54","55-59","60-64","65+")),
           cat=paste(sex,agecat,race, sep="")) %>%
    dplyr::select(cat, EDUCdetailed, sum) %>%
    group_by(cat, EDUCdetailed) %>%
    summarise(sum=sum(sum)) %>%
    ungroup() %>%
    group_by(cat) %>%
    mutate(percent=sum/sum(sum)) %>% filter(EDUCdetailed=="SomeC1")

  toimpute <- basepop %>% filter(education=="SomeC") %>%
    mutate(agecat = cut(age,
                        breaks=c(0,18,19,20,21,24,29,34,39,44,49,54,59,64,100),
                        labels=c("18","19","20","21","22-24","25-29","30-34","35-39","40-44","45-49",
                                 "50-54","55-59","60-64","65+")),
           cat=paste(sex, agecat,race, sep=""))

  toimpute <- left_join(toimpute, somec, by=c("cat"))
  toimpute$random <- runif(nrow(toimpute))
  toimpute$education_detailed <- ifelse(toimpute$random <= toimpute$percent, "SomeC1",
                                             "SomeC2")

  toimpute <- toimpute %>% dplyr::select(-c(random,percent,cat,EDUCdetailed,sum))

  basepop <- basepop %>% filter(education!="SomeC") %>% mutate(education_detailed=education)
  basepop <- rbind(basepop, toimpute)

 return(basepop)
}






