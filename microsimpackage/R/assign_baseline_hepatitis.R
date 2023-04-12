#' Allocate individuals a chronic hepatitis B or C status - based on NHANES data
#'
#' @param
#' @keywords assign hepatitis
#' @export
#' @examples
#' assign_baseline_hepatitis
assign_baseline_hepatitis <- function(basepop){
  hepatitis <- read.csv("SIMAH_workplace/nhanes/hepatitis_b_c_prevalence.csv") %>% 
    rename(microsim.init.sex = Sex, microsim.init.race = Race)
  
  basepop <- basepop %>% mutate(Agecat = cut(microsim.init.age,
                                                 breaks=c(0,34,64,100),
                                                 labels=c("18-24","35-64","65+")))
  basepop <- left_join(basepop, hepatitis)
  basepop$prob <- runif(nrow(basepop))
  basepop$chronicB <- ifelse(basepop$prob<basepop$PrevalenceHepB, 1,0)
  basepop$chronicC <- ifelse(basepop$prob<basepop$PrevalenceHepC, 1,0)
  
  # summary(as.factor(basepop$chronicB))/nrow(basepop)*100
  # summary(as.factor(basepop$chronicC))/nrow(basepop)*100
  basepop <- basepop %>% dplyr::select(-c(Agecat, prob, PrevalenceHepB, PrevalenceHepC))
  return(basepop)
}
