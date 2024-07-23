education_setup <- function(basepop,y){
  basepop$agecat <- ifelse(basepop$microsim.init.age<=21, "18-21",
                           ifelse(basepop$microsim.init.age>21 & basepop$microsim.init.age<=27, "22-27",
                                  ifelse(basepop$microsim.init.age>=28, "28-34", NA)))
  basepop$state <- ifelse(basepop$microsimnewED=="LEHS", 1,
                          ifelse(basepop$microsimnewED=="SomeC1",2,
                                 ifelse(basepop$microsimnewED=="SomeC2",3,
                                        ifelse(basepop$microsimnewED=="SomeC3",4,
                                               ifelse(basepop$microsimnewED=="College",5, NA)))))
  basepop$year <- ifelse(y<=2005, "1999-2005",
                         ifelse(y>=2006 & y<=2011, "2006-2011",
                         ifelse(y>=2012,"2012-2017",NA)))
  basepop$racecat <- ifelse(basepop$microsim.init.race=="BLA", "black",
                            ifelse(basepop$microsim.init.race=="WHI","white",
                                   ifelse(basepop$microsim.init.race=="OTH", "other",
                                          ifelse(basepop$microsim.init.race=="SPA","hispanic",NA))))
  basepop$cat <- paste(basepop$year, basepop$microsim.init.age, basepop$microsim.init.sex,
                       basepop$racecat,
                       "STATEFROM", basepop$state, sep="_")
  basepop$racecat <- NULL
  basepop$prob <- runif(nrow(basepop))
  return(basepop)
}