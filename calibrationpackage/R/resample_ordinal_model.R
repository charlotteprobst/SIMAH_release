#' function for resampling the multinomial logistic regression model for alcohol
#' @param
#' @keywords microsimulation multinomial model model implausibility
#' @export
#' @examples
#' resample_ordinal_model
resample_ordinal_model <- function(models, topsamples, nsamples){
  # resample the multinomial logistic regression
  for(i in 1:nsamples){
    models[[i]]$samplenum <- i
  }

  multinom_models <- models %>% bind_rows() %>% filter(samplenum %in% topsamples)

  means <- multinom_models %>% group_by(name) %>%
    summarise(mean = mean(Value)) %>%
    # summarise(across(`X.Intercept.`:female.factor_2Women.race.factor_2Other..non.Hispanic, mean)) %>%
    # pivot_longer(X.Intercept.:female.factor_2Women.race.factor_2Other..non.Hispanic) %>%
    # mutate(name=paste(name, cat, sep="_")) %>% dplyr::select(-cat) %>%
    pivot_wider(names_from=name, values_from=mean)

  ses <-  multinom_models %>% group_by(name) %>%
    summarise(se = std.error(Value)) %>%
    # summarise(across(`X.Intercept.`:female.factor_2Women.race.factor_2Other..non.Hispanic, std.error)) %>%
    # pivot_longer(X.Intercept.:female.factor_2Women.race.factor_2Other..non.Hispanic) %>%
    # mutate(name=paste(name, cat, sep="_")) %>% dplyr::select(-cat) %>%
    pivot_wider(names_from=name, values_from=se)

  # check order is the same
  colnames(means)==colnames(ses)

  # save a copy of the names
  names <- colnames(means)

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

  first <- names[1]
  last <- tail(names, n=1)

  lhs <- samples %>%
    pivot_longer(first:last) %>%
    rename(Value = value)

return(lhs)
}
