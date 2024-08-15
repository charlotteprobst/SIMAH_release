#'
#' @param
#' @keywords microsimulation markov model
#' @export
#' @examples
#' converts interaction effects for covariate list - may not be needed
expand_interactions_msm <- function(covariates, covlabels){
  cn.nointer <- names(covariates)
  elist <- strsplit(covlabels, ":")
  elist <- lapply(elist, function(x)covariates[x])
  elist <- lapply(elist, function(x)prod(unlist(x)))
  names(elist) <- covlabels
  elist
}
