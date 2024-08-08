#' Processes BRFSS data for new migrants and 18-year-olds entering the simulation
#'
#' @param
#' @keywords brfss
#' @export
#' @examples
#' process_brfss
process_brfss <- function(brfss, SelectedState){

# match the selected state to a region - for state level modelling
  division1 <- c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont")
  division2 <- c("New Jersey","New York","Pennsylvania")
  division3 <- c("Illinois","Indiana","Michigan","Ohio","Wisconsin")
  division4 <- c("Iowa","Kansas","Minnesota","Missouri","Nebraska","North Dakota","South Dakota")
  division5 <- c("Delaware","Florida","Georgia","Maryland","North Carolina","South Carolina","Virginia","DC","West Virginia")
  division6 <- c("Alabama","Kentucky","Mississippi","Tennessee")
  division7 <- c("Arkansas","Louisiana","Oklahoma","Texas")
  division8 <- c("Arizona","Colorado","Idaho","Montana","Nevada","New Mexico","Utah","Wyoming")
  division9 <- c("Alaska","California","Hawaii","Oregon","Washington")

  selectedregion <- ifelse(!is.na(match(SelectedState,division1)), "division1",
                         ifelse(!is.na(match(SelectedState,division2)),"division2",
                                ifelse(!is.na(match(SelectedState,division3)),"division3",
                                       ifelse(!is.na(match(SelectedState,division4)),"division4",
                                              ifelse(!is.na(match(SelectedState,division5)),"division5",
                                                     ifelse(!is.na(match(SelectedState,division6)),"division6",
                                                            ifelse(!is.na(match(SelectedState,division7)),"division7",
                                                                   ifelse(!is.na(match(SelectedState,division8)),"division8",
                                                                          ifelse(!is.na(match(SelectedState,division9)),"division9",
                                                                                 "USA")))))))))
brfss <- brfss %>%
  filter(region==selectedregion) %>%
  filter(age_var<=79) %>% filter(YEAR>=1999) %>%
  rename(education = education_summary,
         drinkingstatus=drinkingstatus,
         alc_gpd=gramsperday_upshifted,
         BMI = BMI,
         income = household_income,
         age=age_var) %>%
  mutate(race = recode(race_eth,"White"="White",
                                     "Black"="Black", "Hispanic"="Hispanic", "Other"="Others"),
         sex = recode(sex_recode,"Male"="m","Female"="f"),
         agecat = cut(age,
                      breaks=c(0,24,34,44,54,64,79),
                      labels=c("18.24","25.34","35.44","45.54","55.64","65.79")),
         formerdrinker = ifelse(drinkingstatus_detailed=="Former drinker", 1,0)) %>%
  dplyr::select(YEAR, State, region, race, age,
                sex, education, drinkingstatus,
                alc_gpd, formerdrinker, BMI, income, agecat)
  return(brfss)
}

