#' Transitions alcohol consumption of individuals - through regression model
#' @param
#' @keywords microsimulation, alcohol
#' @export
#' @examples
#' transition_alcohol_ordinal_regression
transition_alcohol_ordinal_regression <- function(data, model,y){
 # create a dataset with the required variables dummy coded
  data_prediction <- data %>%
    mutate(agecat = cut(microsim.init.age,
                        breaks=c(0,24,64,100),
                        labels=c("18-24","25-64","65+")),
           race = case_when(microsim.init.race=="BLA" ~ "Black, non-Hispanic",
                            microsim.init.race=="WHI" ~ "White, non-Hispanic",
                            microsim.init.race=="OTH" ~ "Other, non-Hispanic",
                            microsim.init.race=="SPA" ~ "Hispanic"),
           sex = case_when(microsim.init.sex=="f" ~ "Women",
                           microsim.init.sex=="m" ~ "Men"),
           ed = case_when(microsim.init.education=="LEHS" ~ "Low",
                          microsim.init.education=="SomeC" ~ "Med",
                          microsim.init.education=="College" ~ "High"),
           ed = ifelse(ed=="High" & agecat=="18-24", "Med",ed),
           strata = paste(agecat, sex, race, ed, sep="_"),
           # microsim.init.alc.gpd = ceiling(microsim.init.alc.gpd),
           abstainer = ifelse(AlcCAT=="Non-drinker" & formerdrinker==0, 1,0)) %>%
    dplyr::select(strata, brfssID, agecat, microsim.init.sex,
                  microsim.init.race, microsim.init.education,
                  microsim.init.alc.gpd,
                  AlcCAT, formerdrinker) %>%
    mutate(female.factor_2Women = ifelse(microsim.init.sex=="f", 1,0),
           `lagged_age25-64` = ifelse(agecat=="25-64", 1,0),
           `lagged_age65+` = ifelse(agecat=="65+", 1,0),
           `race.factor_2Black, non-Hispanic` = ifelse(microsim.init.race=="BLA",1,0),
           `race.factor_2Hispanic` = ifelse(microsim.init.race=="SPA",1,0),
           `race.factor_2Other, non-Hispanic` = ifelse(microsim.init.race=="OTH",1,0),
           lagged_educationLow = ifelse(microsim.init.education=="LEHS", 1,0),
           lagged_educationMed = ifelse(microsim.init.education=="SomeC", 1,0),
           formerdrinker = ifelse(formerdrinker==1,1,0),
           abstainer = ifelse(microsim.init.alc.gpd==0, 1,0),
           cat1_lag = ifelse(AlcCAT=="Low risk", 1,0),
           cat2_lag = ifelse(AlcCAT=="Medium risk", 1,0),
           cat3_lag = ifelse(AlcCAT=="High risk", 1,0),
           alc_scaled = (microsim.init.alc.gpd-mean(microsim.init.alc.gpd)) / sd(microsim.init.alc.gpd))

  # replicate the model call
  design_matrix <- model.matrix(~ cat1_lag + `lagged_age25-64` + `lagged_age65+` + cat2_lag + cat3_lag +
                                  female.factor_2Women + lagged_educationLow + lagged_educationMed +
                                  `race.factor_2Black, non-Hispanic` + `race.factor_2Hispanic` + `race.factor_2Other, non-Hispanic` +
                                  cat1_lag*`lagged_age25-64` + cat1_lag*`lagged_age65+` +
                                  `lagged_age25-64`*cat2_lag + `lagged_age65+`*cat2_lag + `lagged_age25-64`*cat3_lag + `lagged_age65+`*cat3_lag +
                                  cat1_lag*female.factor_2Women + cat2_lag*female.factor_2Women + cat3_lag*female.factor_2Women +
                                  `lagged_age25-64`*female.factor_2Women + `lagged_age65+`*female.factor_2Women +
                                  lagged_educationLow*female.factor_2Women + lagged_educationMed*female.factor_2Women +
                                  `race.factor_2Black, non-Hispanic`*female.factor_2Women + `race.factor_2Hispanic`*female.factor_2Women + `race.factor_2Other, non-Hispanic`*female.factor_2Women +
                                  cat1_lag*lagged_educationLow + cat1_lag*lagged_educationMed + cat2_lag*lagged_educationLow + cat2_lag*lagged_educationMed + cat3_lag*lagged_educationLow + cat3_lag*lagged_educationMed +
                                  `lagged_age25-64`*lagged_educationLow + `lagged_age65+`*lagged_educationLow + `lagged_age25-64`*lagged_educationMed +
                                  `lagged_age25-64`*female.factor_2Women*lagged_educationLow + `lagged_age65+`*female.factor_2Women*lagged_educationLow + `lagged_age25-64`*female.factor_2Women*lagged_educationMed,
                                data=data_prediction)

  coefs <- model %>% dplyr::select(name, Value) %>%
    pivot_wider(names_from=name, values_from=Value)

  to <- length(coefs)-3
  linearcoefs <- coefs[1:to]
  linearcoefs <- as.numeric(linearcoefs)

  data_prediction$linearpred <- as.numeric(design_matrix %*% linearcoefs)

  coef_names <- names(coefs)[1:to]
  design_matrix <- as.data.frame(design_matrix)

  # Calculate the linear predictor using named coefficients
  data_prediction$linearpred <- rowSums(sapply(coef_names, function(name) coefs[name] * design_matrix[, name]))



#
#   data_prediction$linearpred <-
#     as.numeric(coefs['cat1_lag'])*data_prediction$cat1 +
#     as.numeric(coefs['cat2_lag'])*data_prediction$cat2 +
#     as.numeric(coefs['cat3_lag'])*data_prediction$cat3 +
#     as.numeric(coefs['female.factor_2Women'])*data_prediction$Women +
#     as.numeric(coefs['lagged_age25-64'])*data_prediction$age2564 +
#     as.numeric(coefs['lagged_age65+'])*data_prediction$age65 +
#     as.numeric(coefs['lagged_educationLow'])*data_prediction$edulow +
#     as.numeric(coefs['lagged_educationMed'])*data_prediction$edumed +
#     as.numeric(coefs['race.factor_2Black, non-Hispanic'])*data_prediction$raceblack +
#     as.numeric(coefs['race.factor_2Hispanic'])*data_prediction$racehispanic +
#     as.numeric(coefs['race.factor_2Other, non-Hispanic'])*data_prediction$raceother +
#     as.numeric(coefs['cat1_lag:lagged_age25-64'])*data_prediction$cat1*data_prediction$age2564 +
#     as.numeric(coefs['cat1_lag:lagged_age65+'])*data_prediction$cat1*data_prediction$age65 +
#     as.numeric(coefs['lagged_age25-64:cat2_lag'])*data_prediction$age2564*data_prediction$cat2 +
#     as.numeric(coefs['lagged_age65+:cat2_lag'])*data_prediction$age65*data_prediction$cat2 +
#     as.numeric(coefs['lagged_age25-64:cat3_lag'])*data_prediction$age2564*data_prediction$cat3 +
#     as.numeric(coefs['lagged_age65+:cat3_lag'])*data_prediction$age65*data_prediction$cat3 +
#     as.numeric(coefs['cat1_lag:female.factor_2Women'])*data_prediction$cat1*data_prediction$Women +
#     as.numeric(coefs['cat2_lag:female.factor_2Women'])*data_prediction$cat2*data_prediction$Women +
#     as.numeric(coefs['cat3_lag:female.factor_2Women'])*data_prediction$cat3*data_prediction$Women +
#     as.numeric(coefs['lagged_age25-64:female.factor_2Women'])*data_prediction$age2564*data_prediction$Women +
#     as.numeric(coefs['lagged_age65+:female.factor_2Women'])*data_prediction$age65*data_prediction$Women +
#     as.numeric(coefs['female.factor_2Women:lagged_educationLow'])*data_prediction$Women*data_prediction$edulow +
#     as.numeric(coefs['female.factor_2Women:lagged_educationMed'])*data_prediction$Women*data_prediction$edumed +
#     as.numeric(coefs['female.factor_2Women:race.factor_2Black, non-Hispanic'])*data_prediction$Women*data_prediction$raceblack +
#     as.numeric(coefs['female.factor_2Women:race.factor_2Hispanic'])*data_prediction$Women*data_prediction$racehispanic +
#     as.numeric(coefs['female.factor_2Women:race.factor_2Other, non-Hispanic'])*data_prediction$Women*data_prediction$raceother +
#     as.numeric(coefs['cat1_lag:lagged_educationLow'])*data_prediction$cat1*data_prediction$edulow +
#     as.numeric(coefs['cat1_lag:lagged_educationMed'])*data_prediction$cat1*data_prediction$edumed +
#     as.numeric(coefs['cat2_lag:lagged_educationLow'])*data_prediction$cat2*data_prediction$edulow +
#     as.numeric(coefs['cat2_lag:lagged_educationMed'])*data_prediction$cat2*data_prediction$edumed +
#     as.numeric(coefs['cat3_lag:lagged_educationLow'])*data_prediction$cat3*data_prediction$edulow +
#     as.numeric(coefs['cat3_lag:lagged_educationMed'])*data_prediction$cat3*data_prediction$edumed +
#     as.numeric(coefs['lagged_age25-64:lagged_educationLow'])*data_prediction$age2564*data_prediction$edulow +
#     as.numeric(coefs['lagged_age65+:lagged_educationLow'])*data_prediction$age65*data_prediction$edulow +
#     as.numeric(coefs['lagged_age25-64:lagged_educationMed'])*data_prediction$age2564*data_prediction$edumed +
#     # as.numeric(coefs['lagged_age65+:lagged_educationMed'])*data_prediction$age65*data_prediction$edumed +
#     as.numeric(coefs['lagged_age25-64:female.factor_2Women:lagged_educationLow'])*data_prediction$age2564*data_prediction$Women*data_prediction$edulow +
#     as.numeric(coefs['lagged_age65+:female.factor_2Women:lagged_educationLow'])*data_prediction$age65*data_prediction$Women*data_prediction$edulow +
#     as.numeric(coefs['lagged_age25-64:female.factor_2Women:lagged_educationMed'])*data_prediction$age2564*data_prediction$Women*data_prediction$edumed
#     # as.numeric(coefs['lagged_age65+:female.factor_2Women:lagged_educationMed'])*data_prediction$age65*data_prediction$Women*data_prediction$edumed
#
  intercept1 <- as.numeric(coefs["Non-drinker|Low risk"])
  intercept2 <- as.numeric(coefs["Low risk|Medium risk"])
  intercept3 <- as.numeric(coefs["Medium risk|High risk"])

  data_prediction$p1 <- 1 / (1 + exp(-(intercept1 - data_prediction$linearpred)))
  data_prediction$p2 <- 1 / (1 + exp(-(intercept2 - data_prediction$linearpred)))
  data_prediction$p3 <- 1 / (1 + exp(-(intercept3 - data_prediction$linearpred)))

  # Calculate probabilities for each category
  data_prediction$prob_cat1 <- data_prediction$p1
  data_prediction$prob_cat2 <- data_prediction$p2 - data_prediction$p1
  data_prediction$prob_cat3 <- data_prediction$p3 - data_prediction$p2
  data_prediction$prob_cat4 <- 1 - data_prediction$p3


  data_prediction$cprob_nondrinker <- data_prediction$prob_cat1
  data_prediction$cprob_lowrisk <- data_prediction$cprob_nondrinker + data_prediction$prob_cat2
  data_prediction$cprob_mediumrisk <- data_prediction$cprob_lowrisk + data_prediction$prob_cat3
  data_prediction$cprob_highrisk <- data_prediction$cprob_mediumrisk + data_prediction$prob_cat4

  data_prediction$cprob_highrisk <- ifelse(data_prediction$cprob_highrisk<1, 1,
                                           ifelse(data_prediction$cprob_highrisk>1, 1, data_prediction$cprob_highrisk))

  data_prediction$random <- runif(nrow(data_prediction))

  data_prediction$newcat <- ifelse(data_prediction$random <= data_prediction$cprob_nondrinker, "Non-drinker",
                                   ifelse(data_prediction$random>data_prediction$cprob_nondrinker & data_prediction$random<= data_prediction$cprob_lowrisk, "Low risk",
                                          ifelse(data_prediction$random>data_prediction$cprob_lowrisk & data_prediction$random<=data_prediction$cprob_mediumrisk, "Medium risk",
                                                 ifelse(data_prediction$random>data_prediction$cprob_mediumrisk, "High risk", NA))))

  data_prediction$totransitioncont <- ifelse(data_prediction$AlcCAT==data_prediction$newcat, 0,
                                             ifelse(data_prediction$newcat=="Non-drinker", 0,
                                             ifelse(data_prediction$AlcCAT!=data_prediction$newcat, 1, NA)))

  data$AlcCAT <- data_prediction$newcat
  data$totransitioncont <- data_prediction$totransitioncont
  return(data)
}
