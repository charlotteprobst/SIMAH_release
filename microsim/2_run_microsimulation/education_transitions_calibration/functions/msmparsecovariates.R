
msm.parse.covariates <- function(x, covariates, mod, consider.center=TRUE){
  nc <- mod$ncovs
  if (nc == 0){
    if (is.list(covariates) && (length(covariates) > 0))
      warning("Ignoring covariates - no covariates in this part of the model")
    return(list())
  }
  if (consider.center) {
    ## no adjustment needed: baseline is what we want
    if (!is.list(covariates) &&
        ((covariates==0 && !x$center) || (covariates=="mean" && x$center)))
      return(list())
  }
  if (!is.list(covariates)) {
    covlist <- list()
    if (covariates == 0) {
      for (i in 1:nc)
        covlist[[mod$covlabels[i]]] <- 0
    }
    else if (covariates == "mean") {
      for (i in 1:nc)
        covlist[[mod$covlabels[i]]] <- mod$covmeans[i]
    }
    else stop("covariates argument must be 0, \"mean\", or a list of values for each named covariate")
  }
  else {
    ## Check supplied list of covariate values, convert factors to numeric contrasts, expand interactions, set unknown values to zero.
    covlist <- factorcov2numeric.msm(covariates, x, mod)
  }
  if (x$center && consider.center)
    for (i in 1:nc)
      covlist[[mod$covlabels[i]]] <- covlist[[mod$covlabels[i]]] - mod$covmeans[i]            
    covlist
}
