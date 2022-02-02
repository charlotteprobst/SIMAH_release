# setting up new samples of parameter settings 
# first define  parameter means 
# Code for generating samples from joint prior distribution over the ABM inputs
# Mark Strong
# 26.3.18
sex <- "b"
if(PE==0){
  if(mortality==1 & sex=="b"){
    prior <- list(c("qtruncnorm", 0.0227, 0.0111), #BETA_MALE_MORTALITY
                  c("qtruncnorm", 0.0397, 0.05370378), #BETA_FEMALE_MORTALITY
                  c("qtruncnorm", 2.56, 2.15), #BETA_FORMER_DRINKERS_MEN
                  c("qtruncnorm", 2.56, 2.15), #BETA_FORMER_DRINKERS_WOMEN
                  c("qnorm", -1.020, 0.3083), #METABOLIC_BETA1_MALE
                  c("qnorm", -0.1275, 0.0440), #METABOLIC_BETA2_MALE
                  # c("qnorm", 3.0397, 1.0536), #METABOLIC_BETA1_MALE
                  # c("qnorm", -4.3109, 2.2322), #METABOLIC_BETA2_MALE
                  c("qnorm", 3.0397, 1.0536), #METABOLIC_BETA1_FEMALE
                  c("qnorm", -4.3109, 2.2322), #METABOLIC_BETA2_FEMALE
                  c("qtruncnorm", 0.0099, 0.0009), #BETA_HEPATITIS
                  c("qunif", 50000, 300000), #THRESHOLD
                  c("qunif", 0.5, 1), #THRESHOLD MODIFIER
                  c("qtruncnorm", 0.72, 0.2),#correlation for IRR
                  c("qunif", 1,3) #decay speed 
    #               # c("qunif", 50,50) #RR cap
    #               # c("qunif", 0.76, 1.06),  # M 18-20 M
    #               # c("qunif", 1.07, 1.36),  # M 21-25 M
    #               # c("qunif", 0.98, 1.19),  # M 26-30 M
    #               # c("qunif", 1.00, 1.14),  # M 31-40 M
    #               # c("qunif", 0.89, 1.04),  # M 51-60 M
    #               # c("qunif", 0.85, 1.11),  # M 61-70 M
    #               # c("qunif", 0.72, 1.08),  # M 71+ M
    #               # c("qunif", 0.72, 1.04),  # M 18-20 F
    #               # c("qunif", 1.09, 1.45), # M 21-25 F
    #               # c("qunif", 0.99, 1.27),  # M 26-30 F
    #               # c("qunif", 0.99, 1.18),  # M 31-40 F
    #               # c("qunif", 0.94, 1.11),  # M 51-60 F
    #               # c("qunif", 0.86, 1.12),  # M 61-70 F
    #               # c("qunif", 0.70, 1.02)  # M 71+ F
    )
    names(prior) <- c("BETA_MALE_MORTALITY", "BETA_FEMALE_MORTALITY",
                      "BETA_FORMER_DRINKERS_MEN", "BETA_FORMER_DRINKERS_WOMEN",
                      "METABOLIC_BETA1_MALE", "METABOLIC_BETA2_MALE",
                      "METABOLIC_BETA1_FEMALE", "METABOLIC_BETA2_FEMALE",
                      "BETA_HEPATITIS", "THRESHOLD", "THRESHOLD_MODIFIER", "IRR_correlation",
                      "DECAY_SPEED")
                      # "M_18.20","M_21.25","M_26.30","M_31.40","M_51.60","M_61.70","M_71.", "max_IRR",
                      # "former_18.24","former_25.34","former_35.44","former_45.54","former_55.64",
                      # "former_65.74","former75.")
    
  }else if(mortality==1 & sex=="m"){
  prior <- list(c("qtruncnorm", 0.0227, 0.0111), #BETA_MALE_MORTALITY
                c("qtruncnorm", 2.56, 2.15), #BETA_FORMER_DRINKERS_MEN
                c("qnorm", -1.020, 0.3083), #METABOLIC_BETA1_MALE 0.3083
                c("qnorm", -0.1275, 0.0440), #METABOLIC_BETA2_MALE 0.0440
                c("qtruncnorm", 0.0099, 0.0009), #BETA_HEPATITIS
                c("qunif", 50000, 300000), #THRESHOLD
                c("qtruncnorm", 0.72, 0.1), #correlation for IRR
                c("qnorm", 0.901, 0),  # M 18-20 M
                c("qnorm", 1.205, 0),  # M 21-25 M
                c("qnorm", 1.086, 0),  # M 26-30 M
                c("qnorm", 1.069, 0),  # M 31-40 M
                c("qnorm", 0.963, 0),  # M 51-60 M
                c("qnorm", 0.969, 0),  # M 61-70 M
                c("qnorm", 0.88, 0),  # M 71+ M
                c("qunif", 0.1,0.8), #max IRR allowed
                c("qnorm", 9.17, 0),  # Former drinkers 18-24
                c("qnorm", 10.05, 0),  # Former drinkers 25-34
                c("qnorm", 10.68, 0),  # Former drinkers 35-44
                c("qnorm", 10.78, 0),  # Former drinkers 45-54
                c("qnorm", 11.49, 0),  # Former drinkers 55-64
                c("qnorm", 10.93, 0),  # Former drinkers 65-74
                c("qnorm", 11.13, 0)  # Former drinkers 75+
                
                )
  names(prior) <- c("BETA_MALE_MORTALITY", "BETA_FORMER_DRINKERS_MEN",
                    "METABOLIC_BETA1_MALE", "METABOLIC_BETA2_MALE",
                    "BETA_HEPATITIS", "THRESHOLD", "IRR_correlation",
                    "M_18.20","M_21.25","M_26.30","M_31.40","M_51.60","M_61.70","M_71.", "max_IRR",
                    "former_18.24","former_25.34","former_35.44","former_45.54","former_55.64",
                    "former_65.74","former75.")
  }else if(mortality==1 & sex=="f"){
    prior <- list(c("qtruncnorm", 0.0397, 0.05370378), #BETA_FEMALE_MORTALITY
                  c("qtruncnorm", 2.56, 2.15), #BETA_FORMER_DRINKERS_WOMEN
                  c("qnorm", 3.0397, 1.0536), #METABOLIC_BETA1_FEMALE
                  c("qnorm", -4.3109, 2.2322), #METABOLIC_BETA2_FEMALE
                  c("qtruncnorm", 0.0099, 0.0009), #BETA_HEPATITIS
                  c("qunif", 50000, 300000), #THRESHOLD
                  c("qtruncnorm", 0.72, 0.1), #correlation for IRR
                  c("qnorm", 0.867, 0),  # M 18-20 F
                  c("qnorm", 1.26, 0), # M 21-25 F
                  c("qnorm", 1.125, 0),  # M 26-30 F
                  c("qnorm", 1.082, 0),  # M 31-40 F
                  c("qnorm", 1.019, 0),  # M 51-60 F
                  c("qnorm", 0.982, 0),  # M 61-70 F
                  c("qnorm", 0.841, 0), # M 71+ F
                  c("qunif", 0.1,0.8), #max IRR allowed
                  
                  c("qnorm", 7.05, 0),  # Former drinkers 18-24
                  c("qnorm", 8.15, 0),  # Former drinkers 25-34
                  c("qnorm", 8.57, 0),  # Former drinkers 35-44
                  c("qnorm", 9.24, 0),  # Former drinkers 45-54
                  c("qnorm", 9.44, 0),  # Former drinkers 55-64
                  c("qnorm", 9.55, 0),  # Former drinkers 65-74
                  c("qnorm", 9.84, 0)  # Former drinkers 75+
    )
    names(prior) <- c("BETA_FEMALE_MORTALITY", "BETA_FORMER_DRINKERS_WOMEN",
                      "METABOLIC_BETA1_FEMALE", "METABOLIC_BETA2_FEMALE",
                      "BETA_HEPATITIS", "THRESHOLD", "THRESHOLD_IRR_correlation",
                      "F_18.20","F_21.25","F_26.30","F_31.40","F_51.60","F_61.70","F_71.","max_IRR",
                      "former_18.24","former_25.34","former_35.44","former_45.54","former_55.64",
                      "former_65.74","former75.")
    
  }else if(mortality==0){  
    prior <- list(c("qtruncnorm", 0.0227, 0.0111), #BETA_MALE_MORTALITY
                                   c("qtruncnorm", 0.0397, 0.05370378), #BETA_FEMALE_MORTALITY
                                   c("qtruncnorm", 2.56, 2.15), #BETA_FORMER_DRINKERS_MEN                                   c("qnorm", 2.56, 2.15), #BETA_FORMER_DRINKERS
                                   c("qtruncnorm", 2.56, 2.15), #BETA_FORMER_DRINKERS_WOMEN                                   c("qnorm", 2.56, 2.15), #BETA_FORMER_DRINKERS
                                   c("qnorm", -1.020, 0.3083), #METABOLIC_BETA1_MALE
                                   c("qnorm", -0.1275, 0.0440), #METABOLIC_BETA2_MALE
                                   c("qnorm", 3.0397, 1.0536), #METABOLIC_BETA1_FEMALE
                                   c("qnorm", -4.3109, 2.2322), #METABOLIC_BETA2_FEMALE
                                   c("qtruncnorm", 0.0099, 0.0009), #BETA_HEPATITIS
                                   c("qunif", 50000, 200000), #THRESHOLD
                                   c("qunif", 0.5, 1), #THRESHOLD MODIFIER
                                   c("qtruncnorm", 0.72, 0.2),#correlation for IRR
                                   c("qunif", 1,3) #decay speed 
                  
                  ) 
    
    names(prior) <- c("BETA_MALE_MORTALITY", "BETA_FEMALE_MORTALITY",
                      "BETA_FORMER_DRINKERS_MEN", "BETA_FORMER_DRINKERS_WOMEN",
                      "METABOLIC_BETA1_MALE", "METABOLIC_BETA2_MALE",
                      "METABOLIC_BETA1_FEMALE", "METABOLIC_BETA2_FEMALE",
                      "BETA_HEPATITIS", "THRESHOLD", "THRESHOLD_MODIFIER", "IRR_correlation",
                      "DECAY_SPEED")
    
  }
  
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
# names(lhsSample) <- c("SampleNum","BETA_MALE_MORTALITY","BETA_FEMALE_MORTALITY",
#                       "BETA_FORMER_DRINKERS_MEN","BETA_FORMER_DRINKERS_WOMEN",
#                       "METABOLIC_BETA1_MALE","METABOLIC_BETA2_MALE",
#                       "METABOLIC_BETA1_FEMALE","METABOLIC_BETA2_FEMALE",
#                       "BETA_HEPATITIS",
#                       "THRESHOLD", "THRESHOLD_MODIFIER","IRR_correlation")
#                       # "M_18.20","M_21.25","M_26.30","M_31.40","M_51.60","M_61.70","M_71.",
#                       # "F_18.20","F_21.25","F_26.30","F_31.40","F_51.60","F_61.70","F_71.")
names(lhsSample) <- c("SampleNum",names(prior))
# Save selected priors
write.csv(lhsSample, paste("SIMAH_workplace/microsim/2_output_data/calibration_output/lhsSamples_wave", WAVE, ".csv", sep=""), row.names=F)

list <- list()

for(i in 1:nrow(lhsSample)){
  list[[paste(i)]] <- lhsSample %>% filter(SampleNum==i) %>% dplyr::select(-SampleNum)
}

lhsSample <- list
}

if(PE==1){
  lhsSample <- list()
  lhsSample[[1]] <- data.frame("BETA_MALE_MORTALITY"=0.0227414,
                                      "BETA_FEMALE_MORTALITY"=0.0396643,
                                      "BETA_FEMALE_MORBIDITY"=0.0439704,
                                      "BETA_FORMER_DRINKERS_MEN"=2.56,
                                      "BETA_FORMER_DRINKERS_WOMEN"=2.56,
                                      "METABOLIC_BETA1_MALE"=-1.02011,
                                      "METABOLIC_BETA2_MALE"=-0.1274623,
                                      "METABOLIC_BETA1_FEMALE"=3.03,
                                      "METABOLIC_BETA2_FEMALE"=-4.31,
                                      "BETA_HEPATITIS"=0.009854,
                                      "THRESHOLD"=100000,
                                      "THRESHOLD_MODIFIER"=0.66,
                                      "IRR_correlation"=0.72,
                               "DECAY_SPEED"=1
                               )
}

