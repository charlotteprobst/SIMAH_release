adjust_se <- function(models, data, type){

source("SIMAH_code/education_transitions/2_analysis/1_setup_markov_model.R")
source("SIMAH_code/education_transitions/2_analysis/3_adjust_se.R")

covs <- lapply(models, extract_covariance, type="baseline")

covs <- Filter(function(x) {nrow(x)>1}, covs)

meancovs <- Reduce("+", covs)/length(covs)

missingcovs <- lapply(covs, compare_missing_covariance, meancovs=meancovs)

missingcovs <- Reduce("+", missingcovs)/(length(covs)-1)

finite_simulation <- missingcovs / length(cov)

total_error = meancovs + missingcovs+finite_simulation
diagtotal_error <- diag(matrix(total_error))

se_pool <- sqrt(abs(diagtotal_error[[1]]))
se_pool <- as.data.frame(se_pool)
se_pool$name <- names(covs[[1]])
se_pool <- se_pool %>% 
  separate(name, into=c("Variable","Transition"),sep="_")

coefs <- lapply(models, extract_coefficients)

for(i in 1:length(coefs)){
  coefs[[paste(i)]]$imp <- i
}

coefs <- Filter(function(x) {length(x)>1}, coefs)

coefs_pool <- do.call(rbind,coefs) %>% 
  group_by(Variable, Transition) %>% summarise(mean_pooled=mean(value)) %>% 
  drop_na() %>%
  pivot_wider(names_from=Transition, values_from=mean_pooled) %>% 
  dplyr::select(-c(`LEHS->LEHS`,`SomeC1->SomeC1`,`SomeC2->SomeC2`,
                                              `SomeC3->SomeC3`)) %>%
  pivot_longer(cols=`LEHS->SomeC1`:`SomeC3->College`, names_to="Transition",values_to="mean_pool")

coefs_pool <- left_join(coefs_pool, se_pool) %>% filter(Variable!="base") %>% 
  mutate(Lower = log(mean_pool) - (se_pool*1.96), 
         Upper = log(mean_pool) + (se_pool*1.96),
         Lower = exp(Lower),
         Upper = exp(Upper))
# now adjust for sample size 
origsamplesize <- as.numeric(length(unique(data$uniqueID)))
modelsamplesize <- as.numeric(length(unique(data$newID)))

coefs_final <- coefs_pool %>% 
  mutate(SD = se_pool*(sqrt(modelsamplesize)),
       newSE = SD / (sqrt(origsamplesize)),
       newLower = (log(mean_pool))-(newSE*1.96),
       newUpper = (log(mean_pool))+(newSE*1.96),
       newLower = exp(newLower),
       newUpper = exp(newUpper),
       newLower = round(newLower, digits=2),
       newUpper = round(newUpper, digits=2),
       mean_pool = round(mean_pool, digits=2)) %>% rename(Estimate=mean_pool) %>% 
  select(-c(Lower, Upper, se_pool,
                                                           SD, newSE)) %>% 
  mutate(EstimateCI = paste(Estimate, " (", newLower, ",", newUpper, ")", sep="")) %>% select(Variable, Transition, EstimateCI) %>% 
  pivot_wider(names_from=Transition, values_from=EstimateCI) %>% 
  filter(Variable!="agescaled") %>% filter(Variable!="agesqscaled")

return(coefs_final)
}
