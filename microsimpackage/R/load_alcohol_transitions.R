#' Loads alcohol transition rate data for SIMAH or CASCADE versions
#'
#' @param
#' @keywords alcohol transition rates
#' @export
#' @examples
#' load_alcohol_transitions
load_alcohol_transitions <- function(SelectedState, basepop,brfss, WorkingDirectory){
# AlctransitionProbability <- read.csv(paste0(WorkingDirectory,"SIMAH_workplace/microsim/1_input_data/AlcUse4, Ages7 2-yr TP.csv")) %>%
#   mutate(sex = recode(sex, "Men"="m","Women"="f"),
#          race = recode(race, "Hispanic"="SPA", "Black, non-Hispanic"="BLA",
#                        "Other, non-Hispanic"="OTH","White, non-Hispanic"="WHI"),
#          edu = recode(edu, "Low"="LEHS","Med"="SomeC","High"="College"),
#          age_cat = gsub("Ages ", "", age_cat),
#          From = recode(From, "Abstainer"="Lifetime abstainer", "Former"="Former drinker",
#                        "Category I" = "Low risk", "Category II" = "Medium risk",
#                        "Category III" = "High risk"),
#          To = recode(To, "Abstainer"="Lifetime abstainer", "Former"="Former drinker",
#                      "Category I" = "Low risk", "Category II" = "Medium risk",
#                      "Category III" = "High risk")) %>%
#   mutate(cat = paste(age_cat, sex, race, edu, From, sep="_")) %>%
#   dplyr::select(cat, To, Probability) %>% group_by(cat) %>%
#   mutate(cumsum = cumsum(Probability))
  # add "filename" to function call - i.e. final_alc_transitions ######### TO DO
AlctransitionProbability <- read_rds(paste0(WorkingDirectory, "final_alc_transitions", SelectedState, ".RDS"))
# set up the grams per day categories for the baseline population and brfss donor populations
basepop <- basepop %>%
  mutate(AlcCAT = ifelse(formerdrinker==1, "Former drinker",
                         ifelse(formerdrinker!=1 & microsim.init.alc.gpd==0, "Lifetime abstainer",
                                ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>0 &
                                         microsim.init.alc.gpd<=40, "Low risk",
                                       ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>0 &
                                                microsim.init.alc.gpd<=20, "Low risk",
                                              ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>40 &
                                                       microsim.init.alc.gpd<=60, "Medium risk",
                                                     ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>20 &
                                                              microsim.init.alc.gpd<=40, "Medium risk",
                                                            ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>60,
                                                                   "High risk",
                                                                   ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>40,
                                                                          "High risk", NA)))))))),
         AlcCAT = ifelse(AlcCAT=="Former drinker", "Non-drinker",
                         ifelse(AlcCAT=="Lifetime abstainer", "Non-drinker",AlcCAT))
         )
brfss <- brfss %>%
  mutate(AlcCAT = ifelse(formerdrinker==1, "Former drinker",
                         ifelse(formerdrinker!=1 & microsim.init.alc.gpd==0, "Lifetime abstainer",
                                ifelse(formerdrinker!=1 & microsim.init.sex=="m" & microsim.init.alc.gpd>0 &
                                         microsim.init.alc.gpd<=40, "Low risk",
                                       ifelse(formerdrinker!=1 & microsim.init.sex=="f" & microsim.init.alc.gpd>0 &
                                                microsim.init.alc.gpd<=20, "Low risk",
                                              ifelse(formerdrinker!=1 & microsim.init.sex=="m" & microsim.init.alc.gpd>40 &
                                                       microsim.init.alc.gpd<=60, "Medium risk",
                                                     ifelse(formerdrinker!=1 & microsim.init.sex=="f" & microsim.init.alc.gpd>20 &
                                                              microsim.init.alc.gpd<=40, "Medium risk",
                                                            ifelse(formerdrinker!=1 & microsim.init.sex=="m" & microsim.init.alc.gpd>60,
                                                                   "High risk",
                                                                   ifelse(formerdrinker!=1 & microsim.init.sex=="f" & microsim.init.alc.gpd>40,
                                                                          "High risk", NA)))))))),
         AlcCAT=ifelse(AlcCAT=="Former drinker","Non-drinker",
                       ifelse(AlcCAT=="Lifetime abstainer","Non-drinker",AlcCAT))
         )
list <- list(AlctransitionProbability, basepop, brfss)
return(list)
}
