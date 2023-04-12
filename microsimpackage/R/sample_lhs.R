#' Samples parameters using a latin hypercube space filling design, or takes point estimates.
#'
#' This function generates a latin hypercube for a set number of parameters
#' @param 
#' PE refers to point estimates.  When set to 1, sample point estimates, otherwsie sample from latin hypercube.
#' @keywords latin hypercube
#' @export
#' @examples
#' @description 
#' Each prior contains the type of distribution, the point estimate and the variance


#' sample_lhs
sample_lhs <- function(N_SAMPLES, PE, DISEASES=diseases){
  # setup the list of priors for all possible disease outcomes
    prior <- list(                          
      LVDC <- list(
        c("qnorm", 0.03655479, 0.01144372), #MBLIVER1
        c("qnorm", -0.00011081, 0.00011053), #MBLIVER2
        c("qnorm", 0.06064636, 0.00649378), #FBLIVER1
        c("qnorm", -0.00031181, 0.00005372), #FBLIVER2
        c("qnorm", 0.8297913, 0.3016328)), #LIVER FORMER DRINKERS
      
      HLVDC <- list(        
        c("qnorm", 0.02603471, 0.00071320), #BHEPATITIS1 
        c("qnorm", -0.00008898, 0.00000872)), #BHEPATITIS2

      AUD <- list(
        c("qnorm", 0.0319, 0.0017), #AUD MEN
        c("qnorm", 0.0343 , 0.0014)) #AUD ALL
  )
  
  # name the disease parameters
  names(prior) <- c("LVDC", "HLVDC", "AUD")
  names(prior$LVDC) <- c("B_LIVER1_MEN","B_LIVER2_MEN",
                    "B_LIVER1_WOMEN","B_LIVER2_WOMEN", "LIVER_FORMERDRINKER")
  names(prior$HLVDC) <- c("B_HEPATITIS1","B_HEPATITIS2")
  names(prior$AUD) <- c("B_AUD1_MEN", "B_AUD1_ALL")
  
  prior <- prior %>% keep(names(.) %in% DISEASES) # keep only the requested diseases (specified in model_settings)
  prior <- unlist(prior, recursive=FALSE) # convert the lists of priors into vectors
  names(prior) <- gsub("^.*\\.","", names(prior)) # remove duplicated disease names (generated during the listing and unlisting process)
  
  # If sampling from LHS, generate the template for the lhs sampling procedure (returns a dataframe with the percentile to sample for each parameter, on each sample run)
  if(PE==0){ 
    N_PRIORS <- length(prior)
    set.seed(as.numeric(Sys.time()))
    lhsSampleUniforms <- maximinLHS(N_SAMPLES, N_PRIORS)
    
  # Generate an empty matrix that can be populated with the sampled parameter values  
  lhsSample <- matrix(nrow = N_SAMPLES, ncol = N_PRIORS)
    
  # Sample parameter estimates using the prior list [i.e. distribution function (eg. qnorm), point estimate and variance] and the lhs sampling template (i.e. the percentiles to sample)
  for(i in 1:N_PRIORS) {
      lhsSample[, i] <- eval(call(prior[[i]][1], lhsSampleUniforms[, i], 
                                  as.numeric(prior[[i]][2]), as.numeric(prior[[i]][3])))
    }
  
  # Generate a column with the sample number
  lhsSample <- lhsSample %>% as.data.frame(lhsSample) %>%
      mutate(SampleNum = 1:N_SAMPLES, .before = "V1")
  
  # Rename the columns in the lhsSample table to align with the names of the parameters
  names(lhsSample) <- c("SampleNum", names(prior))
  
  # Save the lhsSample table
  #write.csv(lhsSample, paste0("SIMAH_workplace/microsim/2_output_data", "lhsSamples_wave", WAVE, ".csv"), row.names=F)

  # Convert the lhsSample table into list format
    list <- list()
  for(i in 1:nrow(lhsSample)){
    list[[paste(i)]] <- lhsSample %>% filter(SampleNum==i) %>% dplyr::select(-SampleNum)
  }
  lhsSample <- list
  }
  
  # If requiring point estimates only, use as specified in the priors (midpoint for uniform) 
  else if (PE==1){
    lhsSample <- list()
    prior_new <- data.frame(prior)
    prior_new <- transpose(prior_new)
    prior_new$PE <- ifelse(prior_new$V1=="qnorm",prior_new$V2,
                           ifelse(prior_new$V1=="qunif", (as.numeric(prior_new$V2)+as.numeric(prior_new$V3))/2, NA))
    lhsSample[[1]] <- prior_new$PE
    names(lhsSample[[1]]) <- names(prior)
  }

  return(lhsSample)
}
