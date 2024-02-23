
model <- read_rds("SIMAH_workplace/nesarc/Models/msm3b.RDS")

originalsample <- 34165
inflatedsample <- 341650

originalsample <- 1
inflatedsample <- 1

# first sample from the markov model to get nsamples new estimates
samples <- sample_from_markov(model, nsamples, inflation=1, originalsample, inflatedsample)

# every age sex race combination
covariates <- data.frame(expand.grid(age7=c("18-20","21-25","26-29","30-39","40-49","50-64","65+"),
                                     female_w1=c("Men","Women"),
                                     race_w1 = c("White, non-Hispanic", "Black, non-Hispanic",
                                                 "Other, non-Hispanic", "Hispanic"),
                                     edu3 = c("Low","Med","High")))
covariates$cat <- paste(covariates$age7, covariates$female_w1, covariates$race_w1, covariates$edu3, sep="_")

probs <- convert_to_probability(samples, model, covariates)

# convert to the correct format for using in modelling

transitionsList <- list()
for(i in 1:length(unique(estimates$SampleNum))){
  transitionsList[[paste(i)]] <- probs %>% filter(SampleNum==i) %>%
    mutate(sex = ifelse(sex=="Men", "m","f"),
           race = recode(race, "Black, non-Hispanic"="BLA",
                         "White, non-Hispanic"="WHI",
                         "Other, non-Hispanic"="OTH",
                         "Hispanic"="SPA"),
           educ = recode(educ, "High"="College",
                         "Med"="SomeC","Low"="LEHS"),
           # StateFrom=recode(StateFrom, "1"="Lifetime abstainer",
           #                  "2"="Former drinker", "3"="Low risk",
           #                  "4"="Medium risk", "5"="High risk"),
           StateFrom=recode(StateFrom, "1"="Non-drinker",
                             "2"="Low risk",
                            "3"="Medium risk", "4"="High risk"),
           # StateTo = recode(StateTo, "1"="Lifetime abstainer",
           #                  "2"="Former drinker", "3"="Low risk",
           #                  "4"="Medium risk", "5"="High risk"),
           StateTo=recode(StateTo, "1"="Non-drinker",
                            "2"="Low risk",
                            "3"="Medium risk", "4"="High risk"),

           cat = paste(age, sex, race, educ, StateFrom, sep="_")) %>%
    group_by(cat) %>% mutate(cumsum=cumsum(prob)) %>%
    dplyr::select(cat, StateTo, cumsum)
}

rm(data, model, Samples, probs)
saveRDS(transitionsList, "SIMAH_workplace/microsim/1_input_data/transitionslist_newTP.RDS")
