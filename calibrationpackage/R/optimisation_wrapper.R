#' function to run optimisation process running microsimulation and extracting uncertainty
#' @param
#' @keywords microsimulation optimisation wrapper code
#' @export
#' @examples
optimisation_wrapper <- function(x,model,targets,variance,maxyear,threshold){
  # first convert the values of x to probabilities for the model
  covariates <- data.frame(expand.grid(female_w1=c("Men","Women"),
                                       # age7=c("18-20","21-25","26-29","30-39",
                                       #        "40-49","50-64","65+"),
                                       age3 = c("18-24","25-64","65+"),
                                       edu3=c("Low","Med","High"),
                                       race_w1=c("White, non-Hispanic",
                                                 "Black, non-Hispanic",
                                                 "Hispanic",
                                                 "Other, non-Hispanic")))
  covariates$cat <- paste(covariates$age3, covariates$female_w1, covariates$race_w1, covariates$edu3, sep="_")
  x <- data.frame(t(x))
  probs <- convert_to_probability(x, model, covariates)

  # format for calibration - label categories and put into list format
  probs <- probs %>%
    pivot_longer(cols=State.1:State.4,
                 names_to="StateTo", values_to="prob") %>%
    mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
                               endsWith(StateTo,"2") ~ "State 2",
                               endsWith(StateTo,"3") ~ "State 3",
                               endsWith(StateTo,"4") ~ "State 4")) %>%
    separate(cov, into=c("age","sex","race","edu"), sep="_") %>%
    mutate(sex=ifelse(sex=="Men", "m","f"),
           race=case_when(grepl("Black", race) ~ "BLA",
                          grepl("White", race) ~ "WHI",
                          grepl("Other", race) ~ "OTH",
                          grepl("Hispanic", race) ~ "SPA"),
           edu=case_when(edu=="Low" ~ "LEHS",
                         edu=="Med" ~ "SomeC",
                         edu=="High" ~ "College"),
           StateFrom=case_when(StateFrom==1 ~ "Non-drinker",
                               StateFrom==2 ~ "Low risk",
                               StateFrom==3 ~ "Medium risk",
                               StateFrom==4 ~ "High risk"),
           StateTo=case_when(grepl(1,StateTo) ~ "Non-drinker",
                             grepl(2,StateTo) ~ "Low risk",
                             grepl(3,StateTo) ~ "Medium risk",
                             grepl(4,StateTo) ~ "High risk"))
  # now put into format for TPs in microsimulation
  probs <- probs %>%
    mutate(cat = paste(age, sex, race, edu, StateFrom, sep="_")) %>%
    group_by(cat) %>% mutate(cumsum=cumsum(prob)) %>%
    dplyr::select(cat, StateTo, cumsum)

  # now run the microsimulation
  # first sample a random education model to run
  education_model_num <- sample(1:300,1)
  education_transitions <- education_transitionsList[[education_model_num]]

  output <- run_microsim_alt(sample(1:100,1),1,basepop,brfss,
                   death_counts,
                   updatingeducation, education_transitions,
                   migration_rates,
                   updatingalcohol, probs,
                   catcontmodel, drinkingdistributions,
                   base_counts, diseases, lhs, sesinteraction,
                   policy=0, percentreduction=0.1, year_policy, inflation_factors,
                   age_inflated,
                   update_base_rate,
                   minyear=2000, maxyear=maxyear, output="alcohol",
                   targets, variance,threshold)
  if(is.null(output)){
    output <- Inf
  }
return(output)
}
