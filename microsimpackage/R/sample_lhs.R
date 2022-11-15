#' Samples parameters using a latin hypercube space filling design
#'
#' This function generates a latin hypercube for a set number of parameters
#' @param
#' @keywords latin hypercube
#' @export
#' @examples
#' sample_lhs
sample_lhs <- function(N_SAMPLES, PE){
  if(PE==0){
  prior <- list(
                # c("qtruncnorm", 0.0227, 0.0111), #BETA_MALE_MORTALITY
                # c("qtruncnorm", 0.0397, 0.05370378), #BETA_FEMALE_MORTALITY
                # c("qtruncnorm", 2.56, 2.15), #BETA_FORMER_DRINKERS_MEN
                # c("qtruncnorm", 2.56, 2.15), #BETA_FORMER_DRINKERS_WOMEN
                c("qtruncnorm", 0.0099, 0.0009) #BETA_HEPATITIS
                # c("qunif", 1,3) #decay speed
                # c("qunif", 18, 25) #decay length
  )
  names(prior) <- c(
                    # "BETA_MALE_MORTALITY", "BETA_FEMALE_MORTALITY",
                    # "BETA_FORMER_DRINKERS_MEN", "BETA_FORMER_DRINKERS_WOMEN",
                    "BETA_HEPATITIS"
                    # "DECAY_SPEED"
                    # "DECAY_LENGTH"
                    )
  N_PRIORS <- length(prior)
  set.seed(as.numeric(Sys.time()))
  lhsSampleUniforms <- maximinLHS(N_SAMPLES, N_PRIORS)
  lhsSample <- matrix(nrow = N_SAMPLES, ncol = N_PRIORS)

  for(i in 1:N_PRIORS) {
    if(names(prior[i])=="BETA_FORMER_DRINKERS_MEN" |
       names(prior[i])=="BETA_FORMER_DRINKERS_WOMEN"){
      lhsSample[, i] <- eval(call(prior[[i]][1], lhsSampleUniforms[, i],
                                  a=1, b=Inf,
                                  as.numeric(prior[[i]][2]), as.numeric(prior[[i]][3])))
    }else if(prior[[i]][1]=="qtruncnorm" & names(prior[i])=="IRR_correlation"){
      lhsSample[, i] <- eval(call(prior[[i]][1], lhsSampleUniforms[, i],
                                  a=0, b=1,
                                  as.numeric(prior[[i]][2]), as.numeric(prior[[i]][3])))
    }else if(prior[[i]][1]=="qtruncnorm"){
      lhsSample[, i] <- eval(call(prior[[i]][1], lhsSampleUniforms[, i],
                                  a=0, b=Inf,
                                  as.numeric(prior[[i]][2]), as.numeric(prior[[i]][3])))
    }else{
      lhsSample[, i] <- eval(call(prior[[i]][1], lhsSampleUniforms[, i],
                                  as.numeric(prior[[i]][2]), as.numeric(prior[[i]][3])))

    }
  }

  lhsSample <- as.data.frame(lhsSample)
  version <- c(1:N_SAMPLES)
  lhsSample <- cbind(version, lhsSample)
  names(lhsSample) <- c("SampleNum",names(prior))
  # Save selected priors
  # write.csv(lhsSample, paste0("SIMAH_workplace/microsim/2_output_data", OutputDirectory, "lhsSamples_wave", WAVE, ".csv"), row.names=F)

  list <- list()

  for(i in 1:nrow(lhsSample)){
    list[[paste(i)]] <- lhsSample %>% filter(SampleNum==i) %>% dplyr::select(-SampleNum)
  }

  lhsSample <- list
  }else if(PE==1){
    lhsSample <- list()
    lhsSample[[1]] <- data.frame(
                                # "BETA_MALE_MORTALITY"=0.0227414,
                                #  "BETA_FEMALE_MORTALITY"=0.0396643,
                                #  "BETA_FEMALE_MORBIDITY"=0.0439704,
                                #  "BETA_FORMER_DRINKERS_MEN"=2.56,
                                #  "BETA_FORMER_DRINKERS_WOMEN"=2.56,
                                #  "METABOLIC_BETA1_MALE"=-1.02011,
                                #  "METABOLIC_BETA2_MALE"=-0.1274623,
                                #  "METABOLIC_BETA1_FEMALE"=3.03,
                                #  "METABOLIC_BETA2_FEMALE"=-4.31,
                                 "BETA_HEPATITIS"=0.009854
                                 # "THRESHOLD"=100000,
                                 # "THRESHOLD_MODIFIER"=0.66,
                                 # "IRR_correlation"=0.72,
                                 # "DECAY_SPEED"=1,
                                 # "DECAY_LENGTH"=20
    )
  }

  return(lhsSample)
}
