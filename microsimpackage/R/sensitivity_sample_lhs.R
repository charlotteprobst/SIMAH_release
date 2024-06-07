#' Samples parameters using a latin hypercube space filling design, or takes point estimates.
#'
#' This function generates a latin hypercube for a set number of parameters
#' Disease outcomes with significant interaction effect: LVDC, AUD, IHD 
#' @param
#' PE refers to point estimates.  When set to 1, sample point estimates, otherwsie sample from latin hypercube.
#' @keywords latin hypercube
#' @export
#' @examples
#' @description
#' Each prior contains the type of distribution, the point estimate and the variance

#' sample_lhs
sensitivity_sample_lhs <- function(N_SAMPLES, PE, DISEASES=diseases){
  # setup the list of priors for all possible disease outcomes
    prior <- list(
      LVDC <- list(
        c("qnorm", 0.7646768, 0.1055486), #B_LIVER_LEHS SD 0.1055486
        c("qnorm", 0.4490353, 0.1127937), #B_LIVER_SomeC SD 0.1127937
        c("qnorm", 0.06381951, 0.007754167), #B_LIVER_GPD SD 0.007754167
        c("qnorm", 0.000099995, 0.008875784), #B_LIVER_LEHSxGPD SD 0.008875784
        c("qnorm",  0.04468652, 0.01209888), #B_LIVER_SomeCxGPD SD 0.01209888
        c("qnorm", 0.7793249, 0.2074557), #LIVER_FD_LEHS SD 0.2074557
        c("qnorm", 0.5364934, 0.2380034), #LIVER_FD_SomeC SD 0.2380034
        c("qnorm", 1.033184,  0.3418635), #LIVER_FD SD  0.3418635
        c("qnorm", -0.6348783,  0.380269), #LIVER_LEHSxFD SD  0.380269
        c("qnorm", 0.3418635, 0.4293024)), #LIVER_SomeCxFD SD 0.4293024
      
        #results including former drinkers in continuous grams per day
        # c("qnorm", 0.70650753, 0.100516439), #B_LIVER_LEHS SD 0.100516439
        # c("qnorm", 0.396895157, 0.1060469776), #B_LIVER_SomeC SD 0.1060469776
        # c("qnorm", 0.00319489, 0.00038145156), #B_LIVER_GPD SD 0.00038145156
        # c("qnorm", 0, 0.000459183797), #B_LIVER_LEHSxGPD SD 0.000459183797
        # c("qnorm",  0.002297359, 0.000610840), #B_LIVER_SomeCxGPD SD 0.000610840

        # c("qnorm", 0.6805684, 0.1226193), #B_LIVER_LEHS_MEN SD 0.1226193
        # c("qnorm", 0.3594212, 0.1369978), #B_LIVER_SomeC_MEN SD 0.1369978
        # c("qnorm", 0.7436502, 0.1632077), #B_LIVER_LEHS_WOMEN SD 0.1632077
        # c("qnorm", 0.4489715, 0.1710205), #B_LIVER_SomeC_WOMEN SD  0.1710205
        # c("qnorm", 0.003095205, 0.0004831719), #B_LIVER_GPD_MEN SD 0.0004831719
        # c("qnorm", 0.002895803, 0.0008393614), #B_LIVER_GPD_WOMEN SD 0.0008393614
        # c("qnorm", -0.000100005, 0.0005357949), #B_LIVER_LEHSxGPD_MEN SD 0.0005357949
        # c("qnorm", 0.002397125, 0.0007125759), #B_LIVER_SomeCxGPD_MEN SD 0.0007125759
        # c("qnorm", 0.001099395, 0.0009428886), #B_LIVER_LEHSxGPD_WOMEN SD 0.0009428886
        # c("qnorm",  0.004390348, 0.001320722), #B_LIVER_SomeCxGPD_WOMEN SD 0.001320722

      HLVDC <- list(
        c("qnorm", 0.02603471, 0.00071320), #BHEPATITIS1 SD 0.00071320
        c("qnorm", -0.00008898, 0.00000872)), #BHEPATITIS2 SD 0.00000872

      AUD <- list(
        c("qnorm", 0.78732035, 0.114789), #B_AUD_LEHS SD 0.114789
        c("qnorm", 0.555206, 0.12325817), #B_AUD_SomeC SD  0.12325817
        c("qnorm", 0.05344598, 0.00527199), #B_AUD_GPD SD  0.00527199
        c("qnorm", -0.0038072, 0.006094266), #B_AUD_LEHSxGPD SD 0.006094266
        c("qnorm", 0.025180299, 0.0084577), #B_AUD_SomeCxGPD SD 0.0084577
        c("qnorm", 1.21491274, 0.38280031), #AUD_FD_LEHS SD 0.38280031
        c("qnorm",  0.8586616, 0.448253556), #AUD_FD_SomeC SD 0.448253556
        c("qnorm", 1.73342389,  0.498620239), #AUD_FD SD  0.498620239
        c("qnorm", -0.94160854,  0.5626876), #AUD_LEHSxFD SD  0.5626876
        c("qnorm", -1.3470736, 0.65711184)), #AUD_SomeCxFD SD 0.65711184

        # c("qnorm", 0.8109302, 0.2989031), #B_AUD_LEHS_MEN SD 0.2989031
        # c("qnorm", 0.577175, 0.2689286), #B_AUD_SomeC_MEN SD 0.2689286
        # c("qnorm", 0.7857264, 0.5431378), #B_AUD_LEHS_WOMEN SD 0.5431378
        # c("qnorm", 0.5141401, 0.4247704), #B_AUD_SomeC_WOMEN SD  0.4247704
        # c("qnorm", 0.0517382, 0.006734694), #B_AUD_GPD_MEN SD 0.006734694
        # c("qnorm", 0.05515084, 0.01196429), #B_AUD_GPD_WOMEN SD 0.01196429
        # c("qnorm", -0.005113049, 0.007091837), #B_AUD_LEHSxGPD_MEN SD 0.007091837
        # c("qnorm", 0.0259601, 0.009897959), #B_AUD_SomeCxGPD_MEN SD 0.009897959
        # c("qnorm", 0.009752291, 0.01405612), #B_AUD_LEHSxGPD_WOMEN SD  0.01405612
        # c("qnorm", 0.05713619, 0.01941327), #B_AUD_SomeCxGPD_WOMEN SD 0.01941327
        # c("qnorm", 1.261298, 0.4550207), #AUD_FD_LEHS_MEN SD 0.4550207
        # c("qnorm", 0.7884574, 0.5362027), #AUD_FD_SomeC_MEN SD 0.5362027
        # c("qnorm", 1.427916, 0.6434523), #AUD_FD_LEHS_WOMEN SD 0.6434523
        # c("qnorm", 1.264127, 0.8417455), #AUD_FD_SomeC_WOMEN SD 0.8417455
        # c("qnorm", 1.560248,  0.5724836), #AUD_FD_MEN SD  0.5724836
        # c("qnorm", 1.912501,  0.9801991), #AUD_FD_WOMEN SD  0.9801991
        # c("qnorm", -1.108663,  0.6564949), #AUD_LEHSxFD_MEN SD  0.6564949
        # c("qnorm", -1.660731, 0.799871), #AUD_SomeCxFD_MEN SD 0.799871
        # c("qnorm", -0.6348783,  1.052252), #AUD_LEHSxFD_WOMEN SD  1.052252
        # c("qnorm",  -0.7985077, 1.239748)), #AUD_SomeCxFD_WOMEN SD 1.239748

      IJ <- list(
        c("qnorm", 0.01100787, 0.0032), #BIJ MEN SD 0.0032
        c("qnorm", 0.04919477 , 0.0221), #BIJ WOMEN SD 0.0221
        c("qnorm", 0.3929201, 0.1620372), #IJ FORMER DRINKERS MEN SD 0.1620372
        c("qnorm", 0.5068176, 0.3721027)),#IJ FORMER DRINKERS WOMEN SD 0.3721027
      
      DM <- list(
        c("qnorm", -0.002661, 0.001506), #DM MEN SD 0.001506
        c("qnorm", 0.00, 0.00), #DM MEN OFF SD 0.00
        c("qnorm", -0.02561281 , 0.00446956), #DM1 WOMEN SD 0.00446956
        c("qnorm", 0.04322303 , 0.01065573), #DM2 WOMEN SD 0.01065573
        c("qnorm", 0.256114, 0.11839), #DM FORMER DRINKERS MEN SD 0.11839
        c("qnorm", 0.029170, 0.034375)), #DM FORMER DRINKERS WOMEN SD 0.034375

      IHD <- list(
        c("qnorm", 0.2468601, 0.05558992), #B_IHD_LEHS SD 0.05558992
        c("qnorm", 0.2623643, 0.06485565), #B_IHD_SomeC SD 0.06485565
        c("qnorm", -0.4780358, 0.06152093), #B_IHD_CAT1 SD 0.06152093
        c("qnorm", -0.3710637, 0.1140342), #B_IHD_CAT2 SD 0.1140342
        c("qnorm", -0.01005034, 0.2004439), #B_IHD_CAT3 SD 0.2004439
        c("qnorm", -0.1053605, 0.2606253), #B_IHD_CAT4 SD 0.2606253
        c("qnorm", 0.1570037, 0.0715056), #B_IHD_LEHSxCAT1 SD 0.0715056
        c("qnorm", 0.0861777, 0.1387765), #B_IHD_LEHSxCAT2 SD 0.1387765
        c("qnorm", -0.2744368, 0.2433351), #B_IHD_LEHSxCAT3 SD 0.2433351
        c("qnorm", 0.2700271, 0.291498), #B_IHD_LEHSxCAT4 SD 0.291498
        c("qnorm", 0.09531018, 0.08074429), #B_IHD_SomeCxCAT1 SD 0.08074429
        c("qnorm", -0.1165338, 0.1525094), #B_IHD_SomeCxCAT2 SD 0.1525094
        c("qnorm", 0.10436, 0.2590978), #B_IHD_SomeCxCAT3 SD 0.2590978
        c("qnorm", -0.0618754, 0.3021823), #B_IHD_SomeCxCAT4 SD 0.3021823
        c("qnorm", 0.0861777,  0.09770211), #IHD_FD SD  0.09770211
        c("qnorm", -0.03045921,  0.1077588), #IHD_LEHSxFD SD  0.1077588
        c("qnorm", 0.09531018, 0.1232587)), #IHD_SomeCxFD SD 0.1232587

        # c("qnorm", 0.1655144, 0.07338828), #B_IHD_LEHS_MEN SD 0.07338828
        # c("qnorm", 0.3148107, 0.09054446), #B_IHD_SomeC_MEN SD 0.09054446
        # c("qnorm", 0.2926696, 0.08164473), #B_IHD_LEHS_WOMEN SD 0.08164473
        # c("qnorm", 0.2070142, 0.08827594), #B_IHD_SomeC_WOMEN SD 0.08827594
        # c("qnorm", -0.4155154, 0.076723), #B_IHD_CAT1_MEN SD 0.076723
        # c("qnorm", -0.3566749, 0.1303127), #B_IHD_CAT2_MEN SD 0.1303127
        # c("qnorm", -0.03045921, 0.2029148), #B_IHD_CAT3_MEN SD 0.2029148
        # c("qnorm", -0.1508229, 0.2647928), #B_IHD_CAT4_MEN SD 0.2647928
        # c("qnorm", -0.6931472, 0.1055006), #B_IHD_CAT1_WOMEN SD 0.1055006
        # c("qnorm", -0.5447272, 0.2647928), #B_IHD_CAT2_WOMEN SD 0.2647928
        # c("qnorm", -0.05129329, 0.07260935), #B_IHD_LEHSxCAT1_MEN SD 0.07260935
        # c("qnorm", 0, 0.1065651), #B_IHD_LEHSxCAT2_MEN SD 0.1065651
        # c("qnorm", -0.0618754, 0.146327), #B_IHD_LEHSxCAT3_MEN SD 0.146327
        # c("qnorm", 0.3784364, 0.1460508), #B_IHD_LEHSxCAT4_MEN SD 0.1460508
        # c("qnorm", -0.08338161, 0.08064286), #B_IHD_SomeCxCAT1_MEN SD 0.08064286
        # c("qnorm", -0.2613648, 0.1346002), #B_IHD_SomeCxCAT2_MEN SD 0.1346002
        # c("qnorm", 0.3220835, 0.1585004), #B_IHD_SomeCxCAT3_MEN SD 0.1585004
        # c("qnorm", 0.04879016, 0.1906159), #B_IHD_SomeCxCAT4_MEN SD 0.1906159
        # c("qnorm", -0.09431068, 0.0863076), #B_IHD_LEHSxCAT1_WOMEN SD 0.0863076
        # c("qnorm", -0.2613648, 0.09098851), #B_IHD_LEHSxCAT2_WOMEN SD 0.09098851
        # c("qnorm", -0.1743534, 0.1815042), #B_IHD_SomeCxCAT1_WOMEN SD 0.1815042
        # c("qnorm", -0.210721, 0.205818), #B_IHD_SomeCxCAT2_WOMEN SD 0.205818
        # c("qnorm", 0.05826891,  0.1161195), #IHD_FD_MEN SD  0.1161195
        # c("qnorm", 0.1222176,  0.1800321), #IHD_FD_WOMEN SD  0.1800321
        # c("qnorm", 0.2468601,  0.07510615), #IHD_LEHSxFD_MEN SD  0.07510615
        # c("qnorm", 0.1988509, 0.1017286), #IHD_SomeCxFD_MEN SD 0.1017286
        # c("qnorm", 0.4054651,  0.09788906), #IHD_LEHSxFD_WOMEN SD  0.09788906
        # c("qnorm", 0.05826891, 0.1327894)), #IHD_SomeCxFD_WOMEN SD 0.1327894

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
      
      UIJ <- list(
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
  # names(prior) <- c("LVDC", "AUD", "IHD", "ALL")
  names(prior) <- c("LVDC", "HLVDC", "AUD", "IJ", "DM", "IHD", "ISTR", "HYPHD", "MVACC", "UIJ", "ALL")
  names(prior$LVDC) <- c("B_LIVER_LEHS","B_LIVER_SomeC","B_LIVER_GPD","B_LIVER_LEHSxGPD","B_LIVER_SomeCxGPD",
                         "LIVER_FD_LEHS","LIVER_FD_SomeC","LIVER_FD","LIVER_LEHSxFD","LIVER_SomeCxFD")
  names(prior$HLVDC) <- c("B_HEPATITIS1","B_HEPATITIS2")
  names(prior$AUD) <- c("B_AUD_LEHS","B_AUD_SomeC","B_AUD_GPD","B_AUD_LEHSxGPD","B_AUD_SomeCxGPD",
                        "AUD_FD_LEHS","AUD_FD_SomeC","AUD_FD", "AUD_LEHSxFD","AUD_SomeCxFD")
  names(prior$IJ) <- c("B_SUICIDE_MEN", "B_SUICIDE_WOMEN","SUICIDE_FORMERDRINKER_MEN","SUICIDE_FORMERDRINKER_WOMEN")
  names(prior$DM) <- c("B_DM_MEN", "B_DM_MEN_OFF", "B_DM1_WOMEN","B_DM2_WOMEN","DM_FORMERDRINKER_MEN","DM_FORMERDRINKER_WOMEN")
  names(prior$IHD) <- c("B_IHD_LEHS", "B_IHD_SomeC", "B_IHD_CAT1", "B_IHD_CAT2", "B_IHD_CAT3", "B_IHD_CAT4",
                        "B_IHD_LEHSxCAT1", "B_IHD_LEHSxCAT2", "B_IHD_LEHSxCAT3", "B_IHD_LEHSxCAT4",
                        "B_IHD_SomeCxCAT1", "B_IHD_SomeCxCAT2", "B_IHD_SomeCxCAT3", "B_IHD_SomeCxCAT4",
                        "IHD_FD", "IHD_LEHSxFD","IHD_SomeCxFD")
  names(prior$ISTR) <- c("B_ISTR1", "B_ISTR2","B_ISTR3","B_ISTR4","ISTR_FORMERDRINKER")
  names(prior$HYPHD) <- c("B_HYPHD_MEN", "B_HYPHD_WOMEN","HYPHD_FORMERDRINKER")
  names(prior$MVACC) <- c("B_MVACC1", "B_MVACC2", "MVACC_FORMERDRINKER")
  names(prior$UIJ) <- c("B_UIJ1_MEN", "B_UIJ2_MEN", "B_UIJ3_MEN", "B_UIJ1_WOMEN", "B_UIJ2_WOMEN", 
                        "UIJ_FORMERDRINKER")
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
