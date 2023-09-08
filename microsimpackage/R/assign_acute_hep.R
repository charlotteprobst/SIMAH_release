#' Assign new acute cases of hepatitis in each year of the simulation
#' based on IHME data on incidence of new hepatitis B and C infections
#' @param
#' @keywords assign acute hepatitis
#' @export
#' @examples
#' assign acute hepatitis - throughout the simulation
assign_acute_hep <- function(microsim, Hep, distribution, y){
    microsim$agecat <- cut(microsim$microsim.init.age,
                           breaks=c(0,19,24,29,34,39,44,49,54,59,64,69,74,79,85),
                           labels=c("15-19","20-24","25-29","30-34","35-39","40-44",
                                    "45-49","50-54","55-59","60-64","65-69","70-74",
                                    "75-79","80 plus"))
    microsim$agecat <- as.factor(microsim$agecat)
    Hep$agecat <- as.factor(Hep$agecat)
    microsim$gpdcat <- cut(microsim$microsim.init.alc.gpd,
                           breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,
                                    70,75,80,85,90,95,100,105,110,115,120,
                                    125, 130, 135, 140, 150, 1000),
                           labels=c("0-5","5-10","10-15","15-20","20-25","25-30",
                                    "30-35","35-40","40-45","45-50","50-55","55-60",
                                    "60-65","65-70","70-75","75-80","80-85","85-90",
                                    "90-95","95-100","100-105","105-110","110-115","115-120",
                                    "120-125","125-130","130-135","135-140","140-150",">150"))
    microsim$gpdcat <- ifelse(microsim$microsim.init.alc.gpd==0, "0-5", as.character(microsim$gpdcat))
    # assign each individual from microsim a conditional probability of getting HepB / HepC
    # subset Hep by the correct year
    Hep <- Hep %>% dplyr::filter(year==y) %>% dplyr::select(-c(year))
    microsim <- left_join(microsim, Hep, by=c("microsim.init.sex","agecat"))
    microsim <- left_join(microsim, distribution, by=c("microsim.init.sex","gpdcat"))

    # subset individuals that already have chronic HBV or HCV - can't get it again,
    # is this correct?
    chronic <- microsim %>% mutate(chronic = chronicB + chronicC) %>% filter(chronic>0)
    microsim <- microsim %>% mutate(chronic = chronicB + chronicC) %>% filter(chronic==0)
    microsim <- microsim %>% group_by(microsim.init.sex, agecat) %>%
      mutate(factor=1/(sum(unique(percentage))),
             newpercentage = percentage*factor) %>% ungroup() %>%
      group_by(newpercentage, microsim.init.sex, agecat) %>% add_tally(name="drinkcat") %>% ungroup() %>%
      group_by(microsim.init.sex, agecat) %>%
      mutate(riskHepC = HepC*newpercentage / drinkcat,
             riskHepB = HepB*newpercentage / drinkcat)
    # renormalise percentage to be between 0 and 1
    # sample n number of Hep B / Hep C cases per category according to the drinking distribution %s
    options(digits=22)
    microsim$prob <- runif(nrow(microsim))
    # assign a 0 chance for people that already have chronic HBV or HCV
    microsim$riskHepB <- ifelse(microsim$chronicB==1, 0, microsim$riskHepB)
    microsim$riskHepC <- ifelse(microsim$chronicC==1, 0, microsim$riskHepC)
    microsim$HepBstatus <- ifelse(microsim$prob < microsim$riskHepB, 1, 0)
    microsim$prob <- runif(nrow(microsim))
    microsim$HepCstatus <- ifelse(microsim$prob < microsim$riskHepC, 1, 0)
    microsim <- rbind(microsim, chronic)
    microsim <- microsim %>% dplyr::select(-c(gpdcat, percentage, riskHepB, riskHepC, prob,
                                              percentage, factor, newpercentage, drinkcat, HepB, HepC, chronic))
    return(microsim)
  }
