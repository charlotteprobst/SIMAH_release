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
  prior <- list(c("qnorm", 0.02603471, 0.00071320), #BHEPATITIS1
                c("qnorm", -0.00008898, 0.00000872), #BHEPATITIS2
                c("qnorm", 0.03655479, 0.01144372), #MBLIVER1
                c("qnorm", -0.00011081, 0.00011053), #MBLIVER2
                c("qnorm", 0.06064636, 0.00649378), #FBLIVER1
                c("qnorm", -0.00031181, 0.00005372), #FBLIVER2
                c("qunif", 1.2336, 4.9818) # LIVER FORMER DRINKERS
  )
  names(prior) <- c("B_HEPATITIS1","B_HEPATITIS2","B_LIVER1_MEN","B_LIVER2_MEN",
                    "B_LIVER1_WOMEN","B_LIVER2_WOMEN", "FORMERDRINKER"
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
    lhsSample[[1]] <- data.frame(B_HEPATITIS1 = 0.02603471, B_HEPATITIS2 = -0.00008898,
                                 B_LIVER1_MEN = 0.03655479, B_LIVER2_MEN = -0.00011081,
                                 B_LIVER1_WOMEN = 0.06064636, B_LIVER2_WOMEN = -0.00031181,
                                 FORMERDRINKER=2.4790
    )

  }

  return(lhsSample)
}
