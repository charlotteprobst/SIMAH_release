# script that 

model <- readRDS("SIMAH_workplace/education_transitions/final_models/formodel_model_alltimes2005_age18-34_agecats.RDS")

originalsample <- 9064
inflatedsample <- 2057084

originalsample <- 1
inflatedsample <- 1

# first sample from the markov model to get nsamples new estimates
samples <- sample_from_markov(model, nsamples, inflation=1, originalsample, inflatedsample)

# now convert each new sampled estimate into transition probabilities
#first specify the covariates for the model
# every age sex race combination
# note these have to be in exactly the same format of the covariates specified in the model
# if unsure of this run model$covariates to check
covariates <- data.frame(expand.grid(agecat=c("18","19","20","21","22-24","25-29","30+"),
                                     sex=c(0,1),
                                     racefinal2=c("white","black","hispanic","other")))
covariates$cat <- paste(covariates$agecat, covariates$sex, covariates$racefinal2, sep="_")


probs <- convert_to_probability(samples, model, covariates)

transitionsList <- list()
for(i in 1:length(unique(estimates$SampleNum))){
  transitionsList[[paste(i)]] <- probs %>% filter(SampleNum==i) %>%
    mutate(sex = ifelse(sex=="male", "m","f"),
           cat = paste(time,age, sex, race, "STATEFROM", StateFrom, sep="_")) %>%
    group_by(cat) %>% mutate(cumsum=cumsum(prob)) %>%
    dplyr::select(cat, StateTo, cumsum)
}
