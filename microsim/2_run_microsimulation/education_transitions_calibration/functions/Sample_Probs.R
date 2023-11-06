Sample_Probs <- function(model, nsamples, TimePeriod, inflation,original,inflated){
  estimates <- model$estimates.t
  covmat <- model$covmat
  SEs <- diag(covmat)
  SDs <- SEs * sqrt(inflated)
  newSEs = SDs / (sqrt(original))
  # work out the magnitude of the difference 
  magnitude <- newSEs/SEs
  covmat <- covmat*magnitude

  # adjust the covariance matrix - first estimate standard deviations 
  
  samples <- mvrnorm(n=nsamples, estimates, covmat)
  x <- model
  sex <- c(0,1)
  race <- c("white","black","hispanic","other")
  age <- c("18","19","20","21-25","26+")
  # every age sex race combination
  combinations <- expand.grid(age,sex,race)
  names(combinations) <- c("age","sex","race")
  combinations <- data.frame(combinations)
  options(digits=3)
  combinations$cat <- paste(combinations$age, combinations$sex, combinations$race, sep="_")
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
  allsamples <- allsamples %>% mutate(StateTo=parse_number(StateTo),
                                      sex = ifelse(sex==1,"female","male"),
                                      time = TimePeriod,
                                      inflation = inflation) %>% 
    dplyr::select(SampleNum, inflation, StateFrom, StateTo, time, age, sex, race, prob)
  SampleNum <- 1:nrow(samples)
  samples <- data.frame(cbind(SampleNum, inflation, TimePeriod, samples))
  list <- list(allsamples, samples)
  return(list)
}
