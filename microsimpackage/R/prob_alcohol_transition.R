#' Transitions alcohol consumption of individuals - through regression model
#' @param
#' @keywords microsimulation, alcohol
#' @export
#' @examples
#' extract probabilities to transition from alcohol transition ordinal regression
prob_alcohol_transition <- function(data, model){
  print("get probabilities to stop drinking")
  
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
  data_prediction <- data_prediction %>%
    mutate(prob_nondrinker = p1,
           prob_low = p2 - p1,
           prob_med = p3 - p2,
           prob_high = 1 - p3) %>% 
    group_by(alc_cat, sex, agecat, race, education) %>%
    summarise(prob_nondrinker = mean(prob_nondrinker),
              prob_low = mean(prob_low),
              prob_med = mean(prob_med),
              prob_high = mean(prob_high))
  
  return(data_prediction)
}
