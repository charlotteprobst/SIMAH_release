
# PATHWAY 1 assign people who have a cirrhosis threshold of above 100kg a RR for cirrhosis 
# based on the dose-response by Roerecke meta-analysis 
# data <- ca
# these betas can/ will be exposed to calibration process  
# BETA_MALE_MORTALITY <- 0.0227414
# BETA_FEMALE_MORTALITY <- 0.0396643
# BETA_FEMALE_MORBIDITY <- 0.0439704

# if(mortality==0){
# CirrhosisHeavyUse <- function(data,lhsSample,sex){
#   data$gpd <- ifelse(data$microsim.init.alc.gpd>200, 200, data$microsim.init.alc.gpd)
#   # data$gpd <- data$microsim.init.alc.gpd
#   BETA_MALE_MORTALITY <- lhsSample["BETA_MALE_MORTALITY"]
#   BETA_FEMALE_MORTALITY <- lhsSample["BETA_FEMALE_MORTALITY"]
#   BETA_FORMER_DRINKERS <- lhsSample["BETA_FORMER_DRINKERS"]
#   data$RRHeavyUse <- ifelse(data$Cirrhosis_risk==1 &
#                            data$microsim.init.sex=="m" & 
#                              data$drinkingstatus2!=1,
#                          exp(BETA_MALE_MORTALITY*data$gpd),
#                          ifelse(data$Cirrhosis_risk==1 &
#                                   data$microsim.init.sex=="f" & 
#                                   data$drinkingstatus2!=1,
#                                 exp(BETA_FEMALE_MORTALITY*data$gpd),
#                                 ifelse(data$drinkingstatus2==1, BETA_FORMER_DRINKERS,1)))
# 
#   data$gpd <- NULL
#   return(data)
# }

# }
# if(mortality==1){
CirrhosisHeavyUse <- function(data,lhsSample,sex){
    if(sex=="b"){
      BETA_MALE_MORTALITY <- as.numeric(lhsSample["BETA_MALE_MORTALITY"])
      BETA_FORMER_DRINKERS_MEN <- as.numeric(lhsSample["BETA_FORMER_DRINKERS_MEN"])
      BETA_FEMALE_MORTALITY <- as.numeric(lhsSample["BETA_FEMALE_MORTALITY"])
      BETA_FORMER_DRINKERS_WOMEN <- as.numeric(lhsSample["BETA_FORMER_DRINKERS_WOMEN"]) 
    }else if(sex=="m"){
    BETA_MALE_MORTALITY <- lhsSample["BETA_MALE_MORTALITY"]
    BETA_FEMALE_MORTALITY <- 0.0397
    BETA_FORMER_DRINKERS_MEN <- lhsSample["BETA_FORMER_DRINKERS_MEN"]
    BETA_FORMER_DRINKERS_WOMEN <- 2.56
    }else if(sex=="f"){
      BETA_MALE_MORTALITY <- 0.0227
      BETA_FEMALE_MORTALITY <- lhsSample["BETA_FEMALE_MORTALITY"]
      BETA_FORMER_DRINKERS_MEN <- 2.56
      BETA_FORMER_DRINKERS_WOMEN <- lhsSample["BETA_FORMER_DRINKERS_WOMEN"] 
    }
    data$gpd <- ifelse(data$microsim.init.alc.gpd>200, 200, data$microsim.init.alc.gpd)
    data$RRHeavyUse <- ifelse(data$Cirrhosis_risk==1 & 
                                data$microsim.init.sex=="m" & 
                                data$microsim.init.drinkingstatus==1,
                              exp(BETA_MALE_MORTALITY*data$gpd),
                              ifelse(data$Cirrhosis_risk==1 &
                                       data$microsim.init.sex=="f" &
                                       data$microsim.init.drinkingstatus==1,
                                     exp(BETA_FEMALE_MORTALITY*data$gpd),
                                     ifelse(data$formerdrinker==1 & data$microsim.init.sex=="m" & 
                                              data$Cirrhosis_risk==1,
                                            BETA_FORMER_DRINKERS_MEN,
                                            ifelse(data$formerdrinker==1 & data$microsim.init.sex=="f" & 
                                                     data$Cirrhosis_risk==1,
                                                   BETA_FORMER_DRINKERS_WOMEN,1))))
    # modify the risk of former drinkers depending on how many years since they drank
    decay_length= as.numeric(lhsSample["DECAY_LENGTH"])
    decay_speed=as.numeric(lhsSample["DECAY_SPEED"])
    t=0:100
    risk_multiplier=1-exp(-(1/decay_speed)*(decay_length-t))
  
    risk <- data.frame(yearsincedrink=as.numeric(t), risk_multiplier=risk_multiplier)
    risk$risk_multiplier <- ifelse(risk$yearsincedrink==0, 1, 
                                   ifelse(risk$yearsincedrink>=decay_length, 0,
                                          risk$risk_multiplier))
    data <- left_join(data, risk, by=c("yearsincedrink"))
    data$RRHeavyUse <- ifelse(data$formerdrinker==1, data$RRHeavyUse*data$risk_multiplier,
                              data$RRHeavyUse)
    data$RRHeavyUse <- ifelse(data$RRHeavyUse<1, 1, data$RRHeavyUse)
    data <- data %>% dplyr::select(-c(risk_multiplier,gpd))
    return(data)
  }


# data <- CirrhosisHeavyUse(data)

# PATHWAY 2: METABOLIC AND CIRRHOSIS INTERACTION 

# set up betas (for calibration)

# METABOLIC_BETA1_MALE <- -1.02011
#   
# METABOLIC_BETA2_MALE <- -0.1274623
# 
# METABOLIC_BETA1_FEMALE <- 3.03973
#   
# METABOLIC_BETA2_FEMALE <- -4.310906

MetabolicPathway <- function(data,lhsSample,sex){
  data$gpd <- ifelse(data$microsim.init.alc.gpd>200, 200, data$microsim.init.alc.gpd)
  # create new variables for gpd+2/1000 etc. 
  data$newgpd <- ((data$gpd+2)/1000)^-.5
  data$newgpdsqrt <- 1.0/sqrt((data$gpd+2)/1000)
  data$newgpd2 <- ((data$gpd+2)/100)
  data$newgpd3 <- data$newgpd2*data$newgpd2*data$newgpd2
  if(sex=="b"){
    METABOLIC_BETA1_MALE <- as.numeric(lhsSample["METABOLIC_BETA1_MALE"])
    METABOLIC_BETA2_MALE <- as.numeric(lhsSample["METABOLIC_BETA2_MALE"])
    METABOLIC_BETA1_FEMALE <- as.numeric(lhsSample["METABOLIC_BETA1_FEMALE"])
    METABOLIC_BETA2_FEMALE <- as.numeric(lhsSample["METABOLIC_BETA2_FEMALE"])
  }else if(sex=="m"){
  METABOLIC_BETA1_MALE <- lhsSample["METABOLIC_BETA1_MALE"]
  METABOLIC_BETA2_MALE <- lhsSample["METABOLIC_BETA2_MALE"]
  METABOLIC_BETA1_FEMALE <- 3.0397
  METABOLIC_BETA2_FEMALE <- -4.3109
  }else if(sex=="f"){
    METABOLIC_BETA1_MALE <- -1.020
    METABOLIC_BETA2_MALE <- -0.1275
    METABOLIC_BETA1_FEMALE <- lhsSample["METABOLIC_BETA1_FEMALE"]
    METABOLIC_BETA2_FEMALE <- lhsSample["METABOLIC_BETA2_FEMALE"]
  }
  test <- 1
  if(test==1){
  data$RRMetabolic <- ifelse(data$microsim.init.BMI>=30 & data$microsim.init.sex=="m" & 
                               data$Cirrhosis_risk==1,
                             (METABOLIC_BETA1_MALE*((((data$gpd+2)/1000)^-.5)-9.537024026) +
                                   METABOLIC_BETA2_MALE*((((data$gpd+2)/1000)^-.5)*
                                                           log((data$gpd+2)/1000)+43.0154401)),
                             # ifelse(data$microsim.init.BMI>=30 & data$microsim.init.sex=="f",
                             # (METABOLIC_BETA1_FEMALE*((((data$gpd+2)/1000)^-.5)-9.537024026) +
                             #    METABOLIC_BETA2_FEMALE*((((data$gpd+2)/1000)^-.5)*
                             #                            log((data$gpd+2)/1000)+43.0154401)),0))
                             ifelse(data$microsim.init.BMI>=30 & data$microsim.init.sex=="f" & 
                                      data$Cirrhosis_risk==1,
                                    (METABOLIC_BETA1_FEMALE*((data$gpd+2)/100)^3-.0000696286 +
                                          METABOLIC_BETA2_FEMALE*((data$gpd+2)/100)^3*
                                          log((data$gpd+2)/100)+.0002221693), 0))
  }else{
    
    data$RRMetabolic <- ifelse(data$microsim.init.BMI>=30 & data$microsim.init.sex=="f",
                                      (METABOLIC_BETA1_FEMALE*((data$gpd+2)/100)^3-.0000696286 +
                                         METABOLIC_BETA2_FEMALE*((data$gpd+2)/100)^3*
                                         log((data$gpd+2)/100)+.0002221693), 
                               ifelse(data$microsim.init.BMI>=30 & data$microsim.init.sex=="m",
                                      (METABOLIC_BETA1_MALE*((data$gpd+2)/100)^3-.0000696286 +
                                         METABOLIC_BETA2_MALE*((data$gpd+2)/100)^3*
                                         log((data$gpd+2)/100)+.0002221693), 0))
  
  # data$RRMetabolic <- ifelse(data$microsim.init.BMI>=30 & data$microsim.init.sex=="m",
  #                            METABOLIC_BETA1_MALE*((((data$newgpdsqrt)-9.537024026))) + 
  #                                  METABOLIC_BETA2_MALE*((((data$newgpdsqrt)*
  #                                                      log((data$gpd+2)/1000)+43.0154401))),
  #                            ifelse(data$microsim.init.BMI>=30 & data$microsim.init.sex=="f",
  #                                  METABOLIC_BETA1_FEMALE*((((data$newgpd3-.0000696286)))) + 
  #                                        METABOLIC_BETA2_FEMALE*(data$newgpd3*
  #                              log(data$newgpd2)+.0002221693), 0))
  }
  
  data$gpd <- NULL
  data$newgpd <- NULL
  data$newgpdsqrt <- NULL
  data$newgpd2 <- NULL
  data$newgpd3 <- NULL
  data$RRMetabolic <- ifelse(data$RRMetabolic<0, 0,data$RRMetabolic)
  data$RRMetabolic <- exp(data$RRMetabolic)
  return(data)
}

# PATHWAY 3 - ASSIGN RISK BASED ON CHRONIC HEPATITIS

# assign individuals a RR for cirrhosis depending on HEP B / C 

# taken from Llamosas-Falcón PAHO report 

# log (OR progression of liver disease) = 0.009854 * daily ethanol intake (g/d) with SE = 0.000907

# calibrated parameter for progression of chronic hepatitis to liver cirrhosis
# BETA_HEPATITIS <- 0.009854
# 
CirrhosisHepatitis <- function(data,lhsSample){
data$gpd <- ifelse(data$microsim.init.alc.gpd>200, 200, data$microsim.init.alc.gpd)
BETA_HEPATITIS <- as.numeric(lhsSample["BETA_HEPATITIS"])
data$RRHep <- ifelse(data$chronicHep==1,
                       (data$gpd*BETA_HEPATITIS),
                       0)
data$gpd <- NULL
data$RRHep <- exp(data$RRHep)
return(data)
}
