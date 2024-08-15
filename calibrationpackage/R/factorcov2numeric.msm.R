#' function to convert the covariates to the correct format for markov model calibration
#' @param
#' @keywords microsimulation markov model
#' @export
#' @examples
#' convert covariates to correct format for markov model
factorcov2numeric.msm <- function(covariates, x, mod=NULL) {
  if (is.null(mod)) mod <- x$qcmodel
  covdata.mf <- x$data$mf[attr(x$data$mf,"covnames")]
  covnames.mm <- mod$covlabels
  covfactor <- sapply(covdata.mf, is.factor)
  covfactorlevels <- lapply(covdata.mf, levels)
  covnames <- names(covdata.mf)

  if (is.null(names(covariates))) {
    if (length(covariates)!=length(covnames)) stop("Expected covariate list of length ",length(covnames))
    names(covariates) <- covnames
  }
  all.covnames <- union(covnames.mm,covnames) # including both factor and contrast names
  miss.covs <- ! names(covariates) %in% all.covnames
  if (any(miss.covs)){
    plural <- if(sum(miss.covs)>1) "s" else ""
    warning("Covariate",plural," \"", paste(names(covariates)[which(!names(covariates)  %in% all.covnames)], collapse=", "), "\" unknown, ignoring")
  }
  cfac <- covariates[names(covariates) %in% covnames[which(covfactor)]]
  cnum <- covariates[! names(covariates) %in% covnames[which(covfactor)]]
  cfac.new <- list()
  for (i in seq_along(cfac)) {
    levs.i <- covfactorlevels[[names(cfac)[[i]]]]
    cfac.i <- rep(0, length(levs.i))
    if (! cfac[[i]] %in% levs.i) stop("Level \"", cfac[[i]], "\" of covariate ", names(cfac)[[i]], " unknown")
    cfac.i[match(cfac[[i]], levs.i)] <- 1
    names(cfac.i) <- paste(names(cfac)[[i]], levs.i, sep="")
    cfac.i <- as.list(cfac.i[-1])
    cfac.new <- c(cfac.new, cfac.i)
  }
  covlabels.noint <- covnames.mm[setdiff(seq_along(covnames.mm), grep(":", covnames.mm))]
  covs.out <- as.list(numeric(length(covlabels.noint)))
  names(covs.out) <- covlabels.noint
  covs.out[names(cnum)] <- cnum
  covs.out[names(cfac.new)] <- cfac.new
  ## fixme when called from bootstrap, hasn't worked.
  ## is it because covfactor is all false?  is covdata.mf wrong?  yes both numeric
  covs.out <- expand_interactions_msm(covs.out, covnames.mm)
  covs.out
  return(covs.out)
}
