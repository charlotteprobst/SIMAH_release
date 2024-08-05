#' Setup education data for simulation
#'
#'This function sets up the education transition parameters for the simulation
#' @param
#' @keywords education
#' @export
#' @examples
#' setup_education
setup_education <- function(basepop,y){
  basepop$agecat <- ifelse(basepop$age==18, "18",
                           ifelse(basepop$age==19, "19",
                                  ifelse(basepop$age==20, "20",
                                         ifelse(basepop$age==21, "21",
                                                ifelse(basepop$age>=22 & basepop$age<=24, "22-24",
                                                       ifelse(basepop$age>=25 & basepop$age<=29, "25-29","30+"))))))
  basepop$state <- ifelse(basepop$education_detailed=="LEHS", 1,
                          ifelse(basepop$education_detailed=="SomeC1",2,
                                 ifelse(basepop$education_detailed=="SomeC2",3,
                                        ifelse(basepop$education_detailed=="SomeC3",4,
                                               ifelse(basepop$education_detailed=="College",5, NA)))))
  basepop$year <- ifelse(y<=2019, "1999-2019",
                         ifelse(y==2020|y==2021, "2019-2021", NA))
  basepop$racecat <- ifelse(basepop$race=="Black", "black",
                            ifelse(basepop$race=="White","white",
                                   ifelse(basepop$race=="Others", "other",
                                          ifelse(basepop$race=="Hispanic","hispanic",NA))))
  basepop$cat <- paste(basepop$agecat, basepop$sex,
                       basepop$racecat,
                       "STATEFROM", basepop$state, sep="_")
  basepop$racecat <- NULL
  basepop$prob <- runif(nrow(basepop))
  return(basepop)
}
