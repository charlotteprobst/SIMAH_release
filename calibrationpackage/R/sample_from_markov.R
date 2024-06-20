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

  lhs_samples <- randomLHS(nsamples, ncol(samples))
  #
  # # Scale samples to the desired range
  scaled_lhs_samples <- t(apply(lhs_samples, 1, function(x) min(samples) + (max(samples) - min(samples)) * x))
  #
  colnames(scaled_lhs_samples) <- colnames(samples)
  #
  samples <- scaled_lhs_samples
  samplenums <- data.frame(samplenum=1:nrow(samples))
  newsamples <- cbind(samplenums, samples)
  return(newsamples)
}
