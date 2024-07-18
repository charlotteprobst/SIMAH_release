#' function for resampling the multinomial logistic regression model for alcohol
#' @param
#' @keywords microsimulation multinomial model model implausibility
#' @export
#' @examples
#' resample_multinom_model
resample_multinom_model <- function(models, topsamples, nsamples){

  # now resample the multinomial logistic regression
  for(i in 1:nsamples){
    transitionsList[[i]]$samplenum <- i
  }
  multinom_models <- transitionsList %>% bind_rows() %>% filter(samplenum %in% topsamples)

  means <- multinom_models %>% group_by(cat) %>%
    summarise(across(`X.Intercept.`:female.factor_2Women.race.factor_2Other..non.Hispanic, mean)) %>%
    pivot_longer(X.Intercept.:female.factor_2Women.race.factor_2Other..non.Hispanic) %>%
    mutate(name=paste(name, cat, sep="_")) %>% dplyr::select(-cat) %>%
    pivot_wider(names_from=name, values_from=value)

  ses <-  multinom_models %>% group_by(cat) %>%
    summarise(across(`X.Intercept.`:female.factor_2Women.race.factor_2Other..non.Hispanic, std.error)) %>%
    pivot_longer(X.Intercept.:female.factor_2Women.race.factor_2Other..non.Hispanic) %>%
    mutate(name=paste(name, cat, sep="_")) %>% dplyr::select(-cat) %>%
    pivot_wider(names_from=name, values_from=value)

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
    mutate(cat = case_when(
      str_detect(name, "High risk") ~ "High risk",
      str_detect(name, "Medium risk") ~ "Medium risk",
      str_detect(name, "Low risk") ~ "Low risk"),
      name = gsub("_High risk", "", name),
      name = gsub("_Medium risk", "", name),
      name = gsub("_Low risk", "", name)) %>% pivot_wider(names_from=name, values_from=value)

  # save samples - for wave 1 in Output Directory
  write.csv(lhs, paste0(OutputDirectory, "/lhs_regression-1",".csv"))

  transitionsList <- list()
  for(i in unique(lhs$sample)){
    transitionsList[[paste(i)]] <- lhs %>% filter(sample==i) %>%
      ungroup() %>%
      dplyr::select(-sample)
  }






# samples <- newsamples %>% dplyr::select(-c(SampleNum, inflation, TimePeriod)) %>%
#   mutate_all(as.numeric)
# estimates <- colMeans(samples)
# cov <- cov(samples)
# samples <- mvrnorm(n=nsamples, estimates, cov)
# x <- model
# sex <- c(0,1)
# race <- c("white","black","hispanic","other")
# age <- c("18","19","20","21","22-24","25-29","30+")
# # every age sex race combination
# combinations <- expand.grid(age,sex,race)
# names(combinations) <- c("age","sex","race")
# combinations <- data.frame(combinations)
# options(digits=3)
# combinations$cat <- paste(combinations$age, combinations$sex, combinations$race, sep="_")
# # plist <- list()
# # plist <- extract_for_estimates(estimates, combinations, x, setupQ, msm.fixdiag.qmatrix,
# #                                msm.parse.covariates, MatrixExp)
# options(scipen=999)
#
# sampleList <- as.list(as.data.frame(t(samples)))
# names(sampleList) <- 1:nrow(samples)
# allsamples <- list()
# for(k in 1:nrow(samples)){
#   estimates <- sampleList[[k]]
#   allsamples[[paste(k)]] <- extract_for_estimates(estimates, combinations, x, setupQ, msm.fixdiag.qmatrix,
#                                                   msm.parse.covariates, MatrixExp)
#   allsamples[[paste(k)]]$SampleNum <- k
# }
# allsamples <- do.call(rbind,allsamples)
# TimePeriod <- "1999-2019"
# inflation <- 1
# allsamples <- allsamples %>% mutate(StateTo=parse_number(StateTo),
#                                     sex = ifelse(sex==1,"female","male"),
#                                     time = TimePeriod,
#                                     inflation = inflation) %>%
#   dplyr::select(SampleNum, inflation, StateFrom, StateTo, time, age, sex, race, prob)
# SampleNum <- 1:nrow(samples)
# samples <- data.frame(cbind(SampleNum, inflation, TimePeriod, samples))
# probs <- allsamples
# estimates <- samples
#
# transitionsList <- list()
# for(i in 1:length(unique(estimates$SampleNum))){
#   transitionsList[[paste(i)]] <- probs %>% filter(SampleNum==i) %>%
#     mutate(sex = ifelse(sex=="male", "m","f"),
#            cat = paste(time,age, sex, race, "STATEFROM", StateFrom, sep="_")) %>%
#     group_by(cat) %>% mutate(cumsum=cumsum(prob)) %>%
#     dplyr::select(cat, StateTo, cumsum)
}
