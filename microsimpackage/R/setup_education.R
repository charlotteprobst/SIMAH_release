#' Setup education data for simulation
#'
#'This function sets up the education transition parameters for the simulation
#' @param
#' @keywords education
#' @export
#' @examples
#' setup_education
setup_education <- function(basepop,y){
  basepop$agecat <- ifelse(basepop$microsim.init.age==18, "18",
                           ifelse(basepop$microsim.init.age==19, "19",
                                  ifelse(basepop$microsim.init.age==20, "20",
                                         ifelse(basepop$microsim.init.age==21, "21",
                                                ifelse(basepop$microsim.init.age>=22 & basepop$microsim.init.age<=24, "22-24",
                                                       ifelse(basepop$microsim.init.age>=25 & basepop$microsim.init.age<=29, "25-29","30+"))))))
  basepop$state <- ifelse(basepop$microsimnewED=="LEHS", 1,
                          ifelse(basepop$microsimnewED=="SomeC1",2,
                                 ifelse(basepop$microsimnewED=="SomeC2",3,
                                        ifelse(basepop$microsimnewED=="SomeC3",4,
                                               ifelse(basepop$microsimnewED=="College",5, NA)))))
  basepop$year <- ifelse(y<=2006, "1999-2006",
                         ifelse(y>=2007 & y<=2013, "2007-2013",
                         ifelse(y>=2014,"2014-2019",NA)))
  basepop$year <- "1999-2019"
  # basepop$year <- ifelse(y<=2006, "1999-2005",
  #                        ifelse(y>=2007 & y<=2013, "2006-2011",
  #                               ifelse(y>=2014,"2012-2017",NA)))
  basepop$racecat <- ifelse(basepop$microsim.init.race=="BLA", "black",
                            ifelse(basepop$microsim.init.race=="WHI","white",
                                   ifelse(basepop$microsim.init.race=="OTH", "other",
                                          ifelse(basepop$microsim.init.race=="SPA","hispanic",NA))))
  # basepop$cat <- paste(basepop$year, basepop$agecat, basepop$microsim.init.sex,
  #                      basepop$racecat,
  #                      "STATEFROM", basepop$state, sep="_")
  basepop$cat <- paste(basepop$agecat, basepop$microsim.init.sex,
                       basepop$racecat,
                       "STATEFROM", basepop$state, sep="_")
  basepop$racecat <- NULL
  basepop$prob <- runif(nrow(basepop))
  return(basepop)
}
