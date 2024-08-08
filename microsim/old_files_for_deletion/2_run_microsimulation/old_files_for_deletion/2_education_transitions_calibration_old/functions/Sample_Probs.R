Sample_Probs <- function(model, nsamples, inflation,original,inflated){
  estimates <- model$estimates.t
  covmat <- model$covmat
  # calculate the difference between the original and inflated sample sizes
  SEs <- diag(covmat)
  SDs <- SEs * sqrt(inflated)
  newSEs = SDs / (sqrt(original))
  # work out the magnitude of the difference
  magnitude <- newSEs/SEs
  # inflate the covariance matrix due to sample sized difference
  # based on magnitude of the difference calculated above
  covmat <- covmat*magnitude

  # now further inflate the cov matrix - due to pre-calculated difference between ACS and PSID
  # this was estimated to be 30x difference but is an adjustable parameter above
  covmat <- covmat*inflation

  # now sample from multivariate normal distribution
  samples <- mvrnorm(n=nsamples, estimates, covmat)

  x <- model
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
