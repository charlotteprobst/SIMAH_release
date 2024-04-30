#' function to convert the covariates to the correct format for markov model calibration
#' @param
#' @keywords microsimulation markov model
#' @export
#' @examples
sample_from_markov <- function(model, nsamples, inflation, originalsample,inflatedsample){
  estimates <- model$estimates
  covmat <- model$covmat
  # calculate the difference between the original and inflated sample sizes
  SEs <- diag(covmat)
  SDs <- SEs * sqrt(inflatedsample)
  newSEs = SDs / (sqrt(originalsample))
  # work out the magnitude of the difference
  magnitude <- newSEs/SEs
  # inflate the covariance matrix due to sample sized difference
  # based on magnitude of the difference calculated above
  covmat <- covmat*magnitude
  diag(covmat) <- newSEs

  # now further inflate the cov matrix - due to pre-calculated difference between ACS and PSID
  # this was estimated to be 30x difference but is an adjustable parameter above
  covmat <- covmat*inflation
  # now sample from multivariate normal distribution
  samples <- mvrnorm(n=nsamples, estimates, covmat)

  # lhs_samples <- randomLHS(nsamples, 54)
  #
  # # Scale samples to the desired range
  # scaled_lhs_samples <- t(apply(lhs_samples, 1, function(x) -3 + (3 - -3) * x))
  #
  # colnames(scaled_lhs_samples) <- colnames(samples)
  #
  # samples <- scaled_lhs_samples

  # # name the parameters
  # parameters <- expand.grid(transition=1:6,
  #                           covariate=names(model$Qmatrices)[1:length(model$Qmatrices)-1])
  # parameters$name <- paste0("transition", parameters$transition, "-", parameters$covariate)
  # colnames(samples) <- parameters$name
  # samples <- data.frame(samples)
  # race_columns <- grep("race", colnames(samples), ignore.case = TRUE, value = TRUE)
  #
  #
  #
  # for (race_col in race_columns) {
  #   range_values <- unlist(ranges[race_col])
  #   for (transition_col in names(samples)) {
  #     if (grepl(race_col, transition_col, ignore.case = TRUE)) {
  #       samples[[transition_col]] <- sample(range_values, nrow(samples), replace = TRUE)
  #     }
  #   }
  # }
  #
  #
  #   sampleslong <- samples %>%
  #   data.frame() %>%
  #   pivot_longer(1:ncol(.)) %>%
  #   separate(name, into=c("transition","covariate", sep=11))


  samplenums <- data.frame(samplenum=1:nrow(samples))
  newsamples <- cbind(samplenums, samples)
  return(newsamples)
}
