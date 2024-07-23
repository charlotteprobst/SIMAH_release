#' function to convert new sampled markov paramters to transition probabilities
#' this outputs a TP
#' @param
#' @keywords microsimulation markov model
#' @export
#' @examples
#' sample_from_markov
convert_to_probability <- function(samples, model, covariates){
  # samples$samplenum <- NULL
  sampleList <- as.list(as.data.frame(t(samples)))
  names(sampleList) <- 1:nrow(samples)
  # loop through each sample and covariate level to convert to a set of TPs
  # save output list
  plist <- list()
  plist_main <- list()
  for(s in 1:nrow(samples)){
    estimates <- sampleList[[s]]
    for(k in levels(as.factor(covariates$cat))){
      # filter on current covariate combination
      combination <- covariates %>% filter(cat==k)
      # get current cov in list format
      combination$cat <- NULL
      currentcov <- as.list(combination)
      covlist <- factorcov2numeric.msm(currentcov, model, model$qcmodel)

      ni <- model$qmodel$npars
      nc <- length(covlist)
      se <- lse <- fixed <- numeric(ni)

      Qmatrices <- setup_Q(model, estimates)

      # for(x in 1:length(Qmatrices)){
      #   print(Qmatrices[[x]] == model$Qmatrices[[x]])
      # }

      logest <- Qmatrices[[1]]
      for (j in seq_len(nc)) {
        logest <- logest + Qmatrices[[j + 1]] * covlist[[j]]
      }

      mat <- exp(logest)
      mat[model$qmodel$imatrix == 0] <- 0
      diag(mat) <- 0
      diag(mat) <- - rowSums(mat)
      p <- MatrixExp(mat, 1)

      plist[[paste(k)]] <- p
      plist[[paste(k)]] <- data.frame(unclass(plist[[paste(k)]]))
      plist[[paste(k)]]$StateFrom <- 1:nrow(plist[[paste(k)]])
      plist[[paste(k)]] <- plist[[paste(k)]] %>% mutate(cov=k)
    }
    plist_main[[paste(s)]] <- bind_rows(plist) %>% mutate(samplenum=s)
    s+1
  }
  plist_main <- bind_rows(plist_main)
  return(plist_main)
  }
