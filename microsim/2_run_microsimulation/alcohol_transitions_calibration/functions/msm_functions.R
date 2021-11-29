msm.fixdiag.qmatrix <- function(qmatrix)
{
  diag(qmatrix) <- 0
  diag(qmatrix) <- - rowSums(qmatrix)
  qmatrix
}

msm.form.output <- function(x, whichp)
{
  model <- if (whichp=="intens") x$qmodel else x$emodel
  cmodel <- if (whichp=="intens") x$qcmodel else x$ecmodel
  p <- x$paramdata
  Matrices <- MatricesSE <- MatricesL <- MatricesU <- MatricesFixed <- list()
  basename <- if (whichp=="intens") "logbaseline" else "logitbaseline"
  fixedpars.logical <- p$constr %in% p$constr[p$fixedpars]
  for (i in 0:cmodel$ncovs) {
    matrixname <- if (i==0) basename else cmodel$covlabels[i] # name of the current output matrix.
    mat <- t(model$imatrix) # state matrices filled by row, while R fills them by column.
    if (whichp=="intens")
      parinds <- if (i==0) which(p$plabs=="qbase") else which(p$plabs=="qcov")[(i-1)*model$npars + 1:model$npars]
    if (whichp=="misc")
      parinds <- if (i==0) which(p$plabs=="p") else which(p$plabs=="hcov")[i + cmodel$ncovs*(1:model$npars - 1)]
    if (any(parinds)) mat[t(model$imatrix)==1] <- p$params[parinds]
    else mat[mat==1] <- Inf ## if no parinds are "p", then there are off-diag 1s in ematrix
    mat <- t(mat)
    dimnames(mat) <- dimnames(model$imatrix)
    fixed <- array(FALSE, dim=dim(model$imatrix))
    if (p$foundse && !p$fixed){
      intenscov <- p$covmat[parinds, parinds]
      intensse <- sqrt(diag(as.matrix(intenscov)))
      semat <- lmat <- umat <- t(model$imatrix)
      if (any(parinds)){
        semat[t(model$imatrix)==1] <- intensse
        lmat[t(model$imatrix)==1] <- p$ci[parinds,1]
        umat[t(model$imatrix)==1] <- p$ci[parinds,2]
        fixed[t(model$imatrix)==1] <- fixedpars.logical[parinds]
      }
      else semat[semat==1] <- lmat[lmat==1] <- umat[umat==1] <- Inf
      semat <- t(semat); lmat <- t(lmat); umat <- t(umat); fixed <- t(fixed)
      diag(semat) <- diag(lmat) <- diag(umat) <- 0
      for (i in 1:nrow(fixed)){
        foff <- fixed[i,-i][model$imatrix[i,-i]==1]
        fixed[i,i] <- (length(foff)>1) && all(foff)
      }
      if (whichp=="misc")
        fixed[which(x$hmodel$model==match("identity", .msm.HMODELS)),] <- TRUE
      
      dimnames(semat)  <- dimnames(mat)
    }
    else {
      semat <- lmat <- umat <- NULL
    }
    Matrices[[matrixname]] <- mat
    MatricesSE[[matrixname]] <- semat
    MatricesL[[matrixname]] <- lmat
    MatricesU[[matrixname]] <- umat
    MatricesFixed[[matrixname]] <- fixed
  }
  nam <- if(whichp=="intens") "Qmatrices" else "Ematrices"
  x[[nam]] <- Matrices; x[[paste0(nam, "SE")]] <- MatricesSE; x[[paste0(nam, "L")]] <- MatricesL
  x[[paste0(nam, "U")]] <- MatricesU; x[[paste0(nam, "Fixed")]] <- MatricesFixed
  x
}

msm.fixdiag.qmatrix <- function(qmatrix)
{
  diag(qmatrix) <- 0
  diag(qmatrix) <- - rowSums(qmatrix)
  qmatrix
}

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
  covs.out <- expand.interactions.msm(covs.out, covnames.mm)
  covs.out
}

expand.interactions.msm <- function(covariates, covlabels){
  cn.nointer <- names(covariates)
  elist <- strsplit(covlabels, ":")
  elist <- lapply(elist, function(x)covariates[x])
  elist <- lapply(elist, function(x)prod(unlist(x)))
  names(elist) <- covlabels
  elist
}
