#' Transitions alcohol consumption of individuals - through regression model
#' @param
#' @keywords microsimulation, alcohol
#' @export
#' @examples
#' transition_alcohol_ordinal_regression
transition_alcohol_ordinal_regression <- function(data, model,y){
 # create a dataset with the required variables dummy coded
  data_prediction <- data %>%
    mutate(agecat = cut(age,
                        breaks=c(0,24,64,100),
                        labels=c("18-24","25-64","65+")),
           race = case_when(race=="Black" ~ "Black, non-Hispanic",
                            race=="White" ~ "White, non-Hispanic",
                            race=="Others" ~ "Other, non-Hispanic",
                            race=="Hispanic" ~ "Hispanic"),
           sex = case_when(sex=="f" ~ "Women",
                           sex=="m" ~ "Men"),
           ed = case_when(education=="LEHS" ~ "Low",
                          education=="SomeC" ~ "Med",
                          education=="College" ~ "High"),
           ed = ifelse(ed=="High" & agecat=="18-24", "Med",ed),
           strata = paste(agecat, sex, race, ed, sep="_"),
           # alc_gpd = ceiling(alc_gpd),
           abstainer = ifelse(alc_cat=="Non-drinker" & formerdrinker==0, 1,0)) %>%
    dplyr::select(strata, agecat, sex,
                  race, education,
                  alc_gpd,
                  alc_cat, formerdrinker) %>%
    mutate(Women = ifelse(sex=="f", 1,0),
           age2564 = ifelse(agecat=="25-64", 1,0),
           age65 = ifelse(agecat=="65+", 1,0),
           raceblack = ifelse(race=="BLA",1,0),
           racehispanic = ifelse(race=="SPA",1,0),
           raceother = ifelse(race=="OTH",1,0),
           edulow = ifelse(education=="LEHS", 1,0),
           edumed = ifelse(education=="SomeC", 1,0),
           formerdrinker = ifelse(formerdrinker==1,1,0),
           abstainer = ifelse(alc_gpd==0, 1,0),
           cat1 = ifelse(alc_cat=="Low risk", 1,0),
           cat2 = ifelse(alc_cat=="Medium risk", 1,0),
           cat3 = ifelse(alc_cat=="High risk", 1,0),
           alc_scaled = (alc_gpd-mean(alc_gpd)) / sd(alc_gpd))

  # add 0s for reference cat
  # reference <- data.frame(cat="Non-drinker", t(rep(0,ncol(model)-1)))
  # reference <- data.frame(t(rep(0,ncol(model)-1)),cat="Non-drinker")

  # names(reference) <- names(model)
  # model <- rbind(model, reference)

  time <- y-2000

  coefs <- model %>% dplyr::select(name, Value) %>%
    pivot_wider(names_from=name, values_from=Value)


  data_prediction$linearpred <-
    as.numeric(coefs['cat1_lag'])*data_prediction$cat1 +
    as.numeric(coefs['cat2_lag'])*data_prediction$cat2 +
    as.numeric(coefs['cat3_lag'])*data_prediction$cat3 +
    as.numeric(coefs['female.factor_2Women'])*data_prediction$Women +
    as.numeric(coefs['lagged_age25-64'])*data_prediction$age2564 +
    as.numeric(coefs['lagged_age65+'])*data_prediction$age65 +
    as.numeric(coefs['lagged_educationLow']) * data_prediction$edulow +
    as.numeric(coefs['lagged_educationMed']) * data_prediction$edumed +
    as.numeric(coefs['race.factor_2Black, non-Hispanic']) * data_prediction$raceblack +
    as.numeric(coefs['race.factor_2Hispanic']) * data_prediction$racehispanic +
    as.numeric(coefs['race.factor_2Other, non-Hispanic']) * data_prediction$raceother +
    as.numeric(coefs['lagged_age25-64:female.factor_2Women'])*data_prediction$Women*data_prediction$age2564 +
    as.numeric(coefs['lagged_age65+:female.factor_2Women'])*data_prediction$Women*data_prediction$age65 +
    as.numeric(coefs['female.factor_2Women:lagged_educationLow'])*data_prediction$Women*data_prediction$edulow +
    as.numeric(coefs['female.factor_2Women:lagged_educationMed'])*data_prediction$Women*data_prediction$edumed +
    as.numeric(coefs['female.factor_2Women:race.factor_2Black, non-Hispanic'])*data_prediction$Women*data_prediction$raceblack +
    as.numeric(coefs['female.factor_2Women:race.factor_2Hispanic'])*data_prediction$Women*data_prediction$racehispanic +
    as.numeric(coefs['female.factor_2Women:race.factor_2Other, non-Hispanic'])*data_prediction$Women*data_prediction$raceother+
    as.numeric(coefs['cat1_lag:lagged_age25-64'])*data_prediction$age2564*data_prediction$cat1+
    as.numeric(coefs['cat1_lag:lagged_age65+'])*data_prediction$age65*data_prediction$cat1+
    as.numeric(coefs['lagged_age25-64:cat2_lag'])*data_prediction$age2564*data_prediction$cat2+
    as.numeric(coefs['lagged_age65+:cat2_lag'])*data_prediction$age65*data_prediction$cat2+
    as.numeric(coefs['lagged_age25-64:cat3_lag'])*data_prediction$age2564*data_prediction$cat3+
    as.numeric(coefs['lagged_age65+:cat3_lag'])*data_prediction$age65*data_prediction$cat3+
    as.numeric(coefs['cat1_lag:lagged_educationLow'])*data_prediction$edulow*data_prediction$cat1+
    as.numeric(coefs['cat1_lag:lagged_educationMed'])*data_prediction$edumed*data_prediction$cat1+
    as.numeric(coefs['cat2_lag:lagged_educationLow'])*data_prediction$edulow*data_prediction$cat2+
    as.numeric(coefs['cat2_lag:lagged_educationMed'])*data_prediction$edumed*data_prediction$cat2+
    as.numeric(coefs['cat3_lag:lagged_educationLow'])*data_prediction$edulow*data_prediction$cat3+
    as.numeric(coefs['cat3_lag:lagged_educationMed'])*data_prediction$edumed*data_prediction$cat3


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

  data_prediction$totransitioncont <- ifelse(data_prediction$alc_cat==data_prediction$newcat, 0,
                                             ifelse(data_prediction$newcat=="Non-drinker", 0,
                                             ifelse(data_prediction$alc_cat!=data_prediction$newcat, 1, NA)))

  data$alc_cat <- data_prediction$newcat
  data$totransitioncont <- data_prediction$totransitioncont
  return(data)
}
