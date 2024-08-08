#' function for resampling the risk function parameters for mortality
#' @param
#' @keywords microsimulation multinomial model model implausibility
#' @export
#' @examples
#' resample_mortality_lhs
resample_mortality_lhs <- function(models, topsamples, nsamples){
  # resample the multinomial logistic regression
  for(i in 1:nsamples){
    models[[i]]$samplenum <- i
  }

  models <- models %>% bind_rows() %>% filter(samplenum %in% topsamples)
  models$samplenum <- NULL

  means <- colMeans(models, na.rm=T)

  standard_error <- function(x) {
    sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
  }

  ses <- sapply(models, standard_error)

  # check order is the same
  names(means)==names(ses)

  # save a copy of the names
  names <- names(means)

  means <- as.numeric(means)
  ses <- as.numeric(ses)

  # Generate Latin Hypercube Sampling
  generate_lhs <- function(means, ses, nsamples) {
    num_parameters <- length(means)
    lhs_sample <- randomLHS(nsamples, num_parameters)
    scaled_sample <- matrix(nrow = nsamples, ncol = num_parameters)

    for (i in 1:num_parameters) {
      scaled_sample[, i] <- lhs_sample[, i] * ses[i] + means[i]
    }

    return(scaled_sample)
  }

  # Generate samples
  samples <- generate_lhs(means, ses, nsamples)

  # Output samples
  samples <- data.frame(samples)

  colnames(samples) <- names

  samples$sample <- 1:nrow(samples)
  samples$BASERATE_YEAR <- round(samples$BASERATE_YEAR)

return(samples)
}
