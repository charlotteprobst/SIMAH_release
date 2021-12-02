Sample_Probs <- function(data, model, nsamples){
  estimates <- model$estimates.t
  covmat <- data.frame(model$covmat)
  covmat <- covmat + covmat*2
  samples <- mvrnorm(n=nsamples, estimates, covmat)
  x <- model
  
  age <- sort(unique(data$age3.factor))
  sex <- unique(data$female_wave1.factor)
  race <- unique(data$race_wave1.factor)
  educ <- unique(data$edu3.factor)
  # every age sex race combination
  combinations <- expand.grid(age,sex,race,educ)
  names(combinations) <- c("age","sex","race","educ")
  combinations <- data.frame(combinations)
  options(digits=3)
  combinations$cat <- paste(combinations$age, combinations$sex, combinations$race, combinations$educ, sep="_")
  # plist <- list()
  # plist <- extract_for_estimates(estimates, combinations, x, setupQ, msm.fixdiag.qmatrix,
  #                                msm.parse.covariates, MatrixExp)
  options(scipen=999)
  
  sampleList <- as.list(as.data.frame(t(samples)))
  names(sampleList) <- 1:nrow(samples)
  allsamples <- list()
  for(k in 1:nrow(samples)){
    estimates <- sampleList[[k]]
    allsamples[[paste(k)]] <- extract_for_estimates(estimates, combinations, x, setupQ, msm.fixdiag.qmatrix,
                                                    msm.parse.covariates, MatrixExp)
    allsamples[[paste(k)]]$SampleNum <- k
  }
  allsamples <- do.call(rbind,allsamples)
  allsamples <- allsamples %>% mutate(StateTo=parse_number(StateTo)) %>% 
    dplyr::select(SampleNum, StateFrom, StateTo, age, sex, race, educ, prob)
  SampleNum <- 1:nrow(samples)
  samples <- data.frame(cbind(SampleNum, samples))
  list <- list(allsamples, samples)
  return(list)
}
