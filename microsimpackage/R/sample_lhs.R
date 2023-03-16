#' Samples parameters using a latin hypercube space filling design
#'
#' This function generates a latin hypercube for a set number of parameters
#' @param 
#' PE refers to point estimates.  When set to 1, sample point estimates, otherwsie sample from latin hypercube.
#' @keywords latin hypercube
#' @export
#' @examples
#' sample_lhs
sample_lhs <- function(N_SAMPLES, PE, DISEASES=diseases){
    prior <- list(
      LVDC <- list(
        c("qnorm", 0.02603471, 0.00071320), #BHEPATITIS1
        c("qnorm", -0.00008898, 0.00000872), #BHEPATITIS2
        c("qnorm", 0.03655479, 0.01144372), #MBLIVER1
        c("qnorm", -0.00011081, 0.00011053), #MBLIVER2
        c("qnorm", 0.06064636, 0.00649378), #FBLIVER1
        c("qnorm", -0.00031181, 0.00005372), #FBLIVER2
        c("qunif", 1.2336, 4.9818)), # LIVER FORMER DRINKERS)
      
      AUD <- list(
        c("qnorm", 0.0319, 0.0017), #AUD MEN
        c("qnorm", 0.0343 , 0.0014)) #AUD ALL
  )
  
  # name the disease parameters
  names(prior) <- c("LVDC", "AUD")
  names(prior$LVDC) <- c("B_HEPATITIS1","B_HEPATITIS2","B_LIVER1_MEN","B_LIVER2_MEN",
                    "B_LIVER1_WOMEN","B_LIVER2_WOMEN", "LIVER_FORMERDRINKER")
  names(prior$AUD) <- c("B_AUD1_MEN", "B_AUD1_ALL")
  
  prior <- prior %>% keep(names(.) %in% DISEASES) # keep only the requested diseases (specified in model_settings)
  prior <- unlist(prior, recursive=FALSE) # convert the lists of priors into vectors
  names(prior) <- gsub("^.*\\.","", names(prior)) # remove duplicated disease names (generated during the listing and unlisting process)
  
  # If sampling from LHS, select priors:
  if(PE==0){
    N_PRIORS <- length(prior)
    set.seed(as.numeric(Sys.time()))
    lhsSampleUniforms <- maximinLHS(N_SAMPLES, N_PRIORS)
    lhsSample <- matrix(nrow = N_SAMPLES, ncol = N_PRIORS)
    
    for(i in 1:N_PRIORS) {
      lhsSample[, i] <- eval(call(prior[[i]][1], lhsSampleUniforms[, i],
                                  as.numeric(prior[[i]][2]), as.numeric(prior[[i]][3])))
    }
      
    lhsSample <- lhsSample %>% as.data.frame(lhsSample) %>%
      mutate(SampleNum = 1:N_SAMPLES, .before = "V1")
    names(lhsSample) <- c("SampleNum", names(prior))
    # Save selected priors
    write.csv(lhsSample, paste0("SIMAH_workplace/microsim/2_output_data", "lhsSamples_wave", WAVE, ".csv"), row.names=F)   
      
    list <- list()

  for(i in 1:nrow(lhsSample)){
    list[[paste(i)]] <- lhsSample %>% filter(SampleNum==i) %>% dplyr::select(-SampleNum)
  }

  lhsSample <- list
  
  # else if taking point estimates only, extract these from the prior distributions list
  }else if(PE==1){
    lhsSample <- list()
    lhsSample[[1]] <- as.data.frame(prior)[-c(1,3),]

  }

  return(lhsSample)
}
