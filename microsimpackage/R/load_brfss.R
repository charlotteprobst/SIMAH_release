#' Loads BRFSS data for new migrants and 18-year-olds
#'
#' @param
#' @keywords brfss
#' @export
#' @examples
#' load_brfss
load_brfss <- function(model="SIMAH", SelectedState, WorkingDirectory){
  # match the selected state to a region
#   division1 <- c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont")
#   division2 <- c("New Jersey","New York","Pennsylvania")
#   division3 <- c("Illinois","Indiana","Michigan","Ohio","Wisconsin")
#   division4 <- c("Iowa","Kansas","Minnesota","Missouri","Nebraska","North Dakota","South Dakota")
#   division5 <- c("Delaware","Florida","Georgia","Maryland","North Carolina","South Carolina","Virginia","DC","West Virginia")
#   division6 <- c("Alabama","Kentucky","Mississippi","Tennessee")
#   division7 <- c("Arkansas","Louisiana","Oklahoma","Texas")
#   division8 <- c("Arizona","Colorado","Idaho","Montana","Nevada","New Mexico","Utah","Wyoming")
#   division9 <- c("Alaska","California","Hawaii","Oregon","Washington")
#
#   selectedregion <- ifelse(!is.na(match(SelectedState,division1)), "division1",
#                          ifelse(!is.na(match(SelectedState,division2)),"division2",
#                                 ifelse(!is.na(match(SelectedState,division3)),"division3",
#                                        ifelse(!is.na(match(SelectedState,division4)),"division4",
#                                               ifelse(!is.na(match(SelectedState,division5)),"division5",
#                                                      ifelse(!is.na(match(SelectedState,division6)),"division6",
#                                                             ifelse(!is.na(match(SelectedState,division7)),"division7",
#                                                                    ifelse(!is.na(match(SelectedState,division8)),"division8",
#                                                                           ifelse(!is.na(match(SelectedState,division9)),"division9",
#                                                                                  "USA")))))))))
# if(model=="SIMAH"){
selectedregion <- "USA"
brfss <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_2000_2022_final.RDS") %>%
  filter(region==selectedregion) %>%
  filter(age_var<=79) %>% filter(YEAR>=1999) %>%
  rename(microsim.init.education = education_summary,
         microsim.init.drinkingstatus=drinkingstatus,
         microsim.init.alc.gpd=gramsperday_upshifted,
         microsim.init.BMI = BMI,
         microsim.init.income = household_income,
         microsim.init.age=age_var) %>%
  mutate(microsim.init.race = recode(race_eth,"White"="WHI",
                                     "Black"="BLA", "Hispanic"="SPA", "Other"="OTH"),
         microsim.init.sex = recode(sex_recode,"Male"="m","Female"="f"),
         agecat = cut(microsim.init.age,
                      breaks=c(0,24,34,44,54,64,79),
                      labels=c("18.24","25.34","35.44","45.54","55.64","65.79")),
         formerdrinker = ifelse(drinkingstatus_detailed=="Former drinker", 1,0)) %>%
  dplyr::select(brfssID, YEAR, State, region, microsim.init.race, microsim.init.age,
                microsim.init.sex, microsim.init.education, microsim.init.drinkingstatus,
                microsim.init.alc.gpd, formerdrinker, microsim.init.BMI, microsim.init.income, agecat)
#
# select <- brfss %>% group_by(YEAR, microsim.init.race, agecat, microsim.init.sex,
#                              microsim.init.education) %>%
#   tally() %>% mutate(tosample = (n*0.1))
#
# sampled <- left_join(brfss, select) %>%
#   group_by(YEAR, microsim.init.race, agecat, microsim.init.sex, microsim.init.education) %>%
#   do(dplyr::sample_n(.,size=unique(tosample), replace=FALSE)) %>%
#   dplyr::select(-c(n,tosample))
# saveRDS(sampled, "SIMAH_workplace/microsim/1_input_data/BRFSS_subset_SIMAH.RDS")
# brfss <- readRDS(paste0(WorkingDirectory,"BRFSS_subset_SIMAH.RDS"))
# }else if(model=="CASCADE"){
#   brfssorig <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_reweighted_upshifted_1984_2020.RDS") %>%
#     filter(age_var<=80) %>% filter(State==SelectedState) %>%
#     mutate(microsim.init.race = recode(race_eth,"White"="WHI",
#                                        "Black"="BLA", "Hispanic"="SPA", "Other"="OTH"),
#            microsim.init.sex = recode(sex_recode,"Male"="m","Female"="f"),
#            microsim.init.education = education_summary,
#            agecat = cut(age_var,
#                         breaks=c(0,24,34,44,54,64,100),
#                         labels=c("18.24","25.34","35.44","45.54","55.64","65.79")),
#            formerdrinker = ifelse(drinkingstatus_detailed=="Former drinker", 1,0),
#            microsim.init.BMI = ifelse(BMI<15, 15,
#                                       ifelse(BMI>50, 50, BMI))) %>%
#     rename(microsim.init.age = age_var,
#            microsim.init.drinkingstatus=drinkingstatus,
#            microsim.init.alc.gpd=gramsperday,
#            microsim.init.income = household_income) %>%
#     dplyr::select(YEAR, State, region, microsim.init.race, microsim.init.age,
#                   microsim.init.sex, microsim.init.education, microsim.init.drinkingstatus,
#                   microsim.init.alc.gpd, formerdrinker, microsim.init.income, agecat,
#                   microsim.init.BMI)
#   brfss <- read_rds("SIMAH_workplace/microsim/1_input_data/brfss_subset.RDS")
# }
  return(brfss)
}

