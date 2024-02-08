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
        c("qnorm", 0.04167190, 0.00974742), #MBLIVER1 SD 0.00974742
        c("qnorm", -0.00011339, 0.00009910), #MBLIVER2 SD 0.00009910
        c("qnorm", 0.06438698, 0.00567568), #FBLIVER1 SD 0.00567568
        c("qnorm", -0.00033988, 0.00004845), #FBLIVER2 SD 0.00004845
        c("qnorm", 0.8297913, 0.3016328)), #LIVER FORMER DRINKERS SD 0.3016328

      HLVDC <- list(
        c("qnorm", 0.02603471, 0.00071320), #BHEPATITIS1 SD 0.00071320
        c("qnorm", -0.00008898, 0.00000872)), #BHEPATITIS2 SD 0.00000872

      AUD <- list(
        c("qnorm", 0.0319, 0.0017), #BAUD MEN SD 0.0017
        c("qnorm", 0.0343, 0.0014), #BAUD ALL SD 0.0014
        c("qnorm", 0.5306283, 0.2328008), #AUD FORMER DRINKERS MEN SD 0.2328008
        c("qnorm", 1.289233, 0.3406336)), #AUD FORMER DRINKERS WOMEN SD 0.3406336

      IJ <- list(
        c("qnorm", 0.01100787, 0.0032), #BIJ MEN SD 0.0032
        c("qnorm", 0.04919477 , 0.0221), #BIJ WOMEN SD 0.0221
        c("qnorm", 0.3929201, 0.1620372), #IJ FORMER DRINKERS MEN SD 0.1620372
        c("qnorm", 0.5068176, 0.3721027)),#IJ FORMER DRINKERS WOMEN SD 0.3721027

      DM <- list(
        c("qnorm", -0.002661, 0.001506), #DM MEN SD 0.001506
        c("qnorm", -0.02561281 , 0.00446956), #DM1 WOMEN SD 0.00446956
        c("qnorm", 0.04322303 , 0.01065573), #DM2 WOMEN SD 0.01065573
        c("qnorm", 0.256114, 0.11839), #DM FORMER DRINKERS MEN SD 0.11839
        c("qnorm", 0.029170, 0.034375)), #DM FORMER DRINKERS WOMEN SD 0.034375

      IHD <- list(
        #age-specific formula beta estimates:
        # c("qnorm", 0, 0), #IHD1 SD 0.00
        # c("qnorm", -0.051293294, 0.122015927), #IHD2 SD 0.122015927
        # c("qnorm", 0.039220713, 0.128415981), #IHD3 SD 0.128415981
        # c("qnorm", 0.067658648, 0.131539114), #IHD4 SD 0.131539114
        # c("qnorm", 0.139761942, 0.145289088), #IHD5 SD 0.145289088
        # c("qnorm", 0.371563556, 0.150731717)), #IHD FORMER DRINKERS SD 0.150731717
        c("qnorm", 0.2151113, 0.1117997), #IHD1 MEN SD 0.1117997
        c("qnorm", -0.1508229, 0.07424866), #IHD2 MEN SD 0.07424866
        c("qnorm", -0.1743534, 0.07603185), #IHD3 MEN SD 0.07603185
        c("qnorm", -0.1165338, 0.07667997), #IHD4 MEN SD 0.07667997
        c("qnorm", 0.0295588, 0.0862533), #IHD5 MEN SD 0.0862533
        c("qnorm", -0.04082199, 0.1207367), #IHD1 WOMEN SD 0.00
        c("qnorm", -0.210721, 0.103435), #IHD2 WOMEN SD 0.103435
        c("qnorm", -0.04082199, 0.1207367), #IHD3 WOMEN SD 0.1207367
        c("qnorm", 0.0295588, 0.141823), #IHD4 WOMEN SD 0.141823
        c("qnorm", 0.07696104, 0.2151188), #IHD5 WOMEN SD 0.2151188
        c("qnorm", 0.3148107, 0.10191197), #IHD FORMER DRINKERS MEN SD 0.10191197
        c("qnorm", 0.1988509, 0.12977504)), #IHD FORMER DRINKERS WOMEN SD 0.12977503

      ISTR <- list(
        c("qnorm", -0.105360516, 0.02837389), #ISTR1 SD 0.02837389
        c("qnorm", -0.083381609, 0.02775583), #ISTR2 SD 0.02775583
        c("qnorm", 0.076961041, 0.03311521), #ISTR3 SD 0.03311521
        c("qnorm", 0.131028262, 0.05792282), #ISTR4 SD 0.05792282
        c("qnorm", 0.00, 0.00)), #ISTR FORMER DRINKERS SD 0.00

      HYPHD <- list(
        c("qnorm", 0.0055863, 0.0008473), #HYPHD MEN SD 0.0008473
        c("qnorm", 0.0069739, 0.0025082), #HYPHD WOMEN SD 0.0025082
        c("qnorm", 0.048790, 0.1083886)), #HYPHD FORMER DRINKERS SD 0.1083886

      MVACC <- list(
        c("qnorm", 0.00299550897979837, 0.00050867822), #MVACC1 SD 0.00050867822
        c("qnorm", 0.959350221334602, 0.227875857649849), #MVACC2 SD 0.227875857649849
        c("qnorm", 0.00, 0.00)), #MVACC FORMER DRINKERS SD 0.00
        #NHIS results
        # c("qnorm", -0.261364764, 0.134600189), #MVACC1 MEN SD 0.134600189
        # c("qnorm", -0.544727175, 0.229576937), #MVACC2 MEN SD 0.229576937
        # c("qnorm", 0.357674444, 0.207168144), #MVACC3 MEN SD 0.207168144
        # c("qnorm", -0.235722334, 0.158593933), #MVACC1 WOMEN SD 0.158593933
        # c("qnorm", 0.31481074, 0.433702775), #MVACC2 WOMEN SD 0.433702775
        # c("qnorm", -0.0618754, 0.1883335758), #MVACC FORMER DRINKERS MEN SD 0.1883335758
        # c("qnorm", 0.26236426, 0.28251579)), #MVACC FORMER DRINKERS WOMEN SD 0.28251579

      UIJ <- list(
        #WHO formula beta estimates
        # c("qnorm", 0.00199800266267306, 0.000509186), #UIJ SD 0.000509186
        # c("qnorm", 0.00, 0.00)), #UIJ FORMER DRINKERS SD 0.00
        #Results for cat 1
        #c("qnorm", -0.116533816, 0.09879663), #UIJ1 MEN SD 0.09879663
        #c("qnorm", -0.162518929, 0.092421667), #UIJ1 WOMEN SD 0.092421667
        c("qnorm", 0.00, 0.00), #UIJ1 MEN SD 0.00
        c("qnorm", 0.371563556, 0.158103223), #UIJ2 MEN SD 0.158103223
        c("qnorm", 0.732367894, 0.167293723), #UIJ3 MEN SD 0.167293723
        c("qnorm", 0.00, 0.00), #UIJ1 WOMEN SD 0.00
        c("qnorm", 0.751416089, 0.205669471), #UIJ2 WOMEN SD 0.205669471
        c("qnorm", 0.00, 0.00)), #UIJ FORMER DRINKERS SD 0.00

      ALL <- list(
        c("qunif", 0, 0.05), #BASE RATE FACTOR - MEN
        c("qunif", 0, 0.05), #BASE RATE FACTOR - WOMEN
        c("qunif", 2006, 2011)) #BASE RATE YEAR TO IMPLEMENT
      )

  # name the disease parameters
  names(prior) <- c("LVDC", "HLVDC", "AUD", "IJ", "DM", "IHD", "ISTR", "HYPHD", "MVACC", "UIJ", "ALL")
  names(prior$LVDC) <- c("B_LIVER1_MEN","B_LIVER2_MEN",
                    "B_LIVER1_WOMEN","B_LIVER2_WOMEN", "LIVER_FORMERDRINKER")
  names(prior$HLVDC) <- c("B_HEPATITIS1","B_HEPATITIS2")
  names(prior$AUD) <- c("B_AUD1_MEN", "B_AUD1_ALL","AUD_FORMERDRINKER_MEN","AUD_FORMERDRINKER_WOMEN")
  names(prior$IJ) <- c("B_SUICIDE_MEN", "B_SUICIDE_WOMEN","SUICIDE_FORMERDRINKER_MEN","SUICIDE_FORMERDRINKER_WOMEN")
  names(prior$DM) <- c("B_DM_MEN", "B_DM1_WOMEN","B_DM2_WOMEN","DM_FORMERDRINKER_MEN","DM_FORMERDRINKER_WOMEN")
  names(prior$IHD) <- c("B_IHD1_MEN", "B_IHD2_MEN", "B_IHD3_MEN", "B_IHD4_MEN", "B_IHD5_MEN", 
                        "B_IHD1_WOMEN", "B_IHD2_WOMEN", "B_IHD3_WOMEN", "B_IHD4_WOMEN", "B_IHD5_WOMEN", "IHD_FORMERDRINKER_MEN", "IHD_FORMERDRINKER_WOMEN")
  names(prior$ISTR) <- c("B_ISTR1", "B_ISTR2","B_ISTR3","B_ISTR4","ISTR_FORMERDRINKER")
  names(prior$HYPHD) <- c("B_HYPHD_MEN", "B_HYPHD_WOMEN","HYPHD_FORMERDRINKER")
  names(prior$MVACC) <- c("B_MVACC1", "B_MVACC2", "MVACC_FORMERDRINKER")
  names(prior$UIJ) <- c("B_UIJ1_MEN", "B_UIJ2_MEN", "B_UIJ3_MEN", "B_UIJ1_WOMEN", "B_UIJ2_WOMEN", "UIJ_FORMERDRINKER")
  names(prior$ALL) <- c("BASERATEFACTOR_MEN","BASERATEFACTOR_WOMEN","BASERATE_YEAR")

  prior <- prior %>% keep(names(.) %in% c(DISEASES,"ALL")) # keep only the requested diseases (specified in model_settings)
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
