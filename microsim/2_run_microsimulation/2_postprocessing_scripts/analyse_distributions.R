
toplhsagesp <- read.csv("SIMAH_workplace/microsim/2_output_data/lhsSamples_wave15_agesp.csv") %>% 
  pivot_longer(BETA_MALE_MORTALITY:DECAY_SPEED) %>% 
  group_by(name) %>% 
  summarise(min = round(min(value),digits=2),
            max = round(max(value),digits=2),
            mean = round(mean(value),digits=2),
            sd = round(sd(value),digits=2)) %>% mutate(type="age specific") %>% 
  mutate(name = factor(name, 
                       levels=c("BETA_MALE_MORTALITY","BETA_FEMALE_MORTALITY",
                                "BETA_FORMER_DRINKERS_MEN","BETA_FORMER_DRINKERS_WOMEN",
                                "METABOLIC_BETA1_MALE","METABOLIC_BETA2_MALE",
                                "METABOLIC_BETA1_FEMALE","METABOLIC_BETA2_FEMALE",
                                "BETA_HEPATITIS","THRESHOLD","THRESHOLD_MODIFIER",
                                "DECAY_SPEED","IRR_correlation"))) %>% 
  arrange(name) %>% 
  mutate(summarystat = ifelse(name=="THRESHOLD" | name=="THRESHOLD_MODIFIER" | name=="DECAY_SPEED",
                              paste0(min, "-", max), paste0(sd))) %>% 
  dplyr::select(name, mean, summarystat) %>% mutate(type="age specific")

toplhsagest <-  read.csv("SIMAH_workplace/microsim/2_output_data/lhsSamples_wave15_agest.csv") %>% 
  pivot_longer(BETA_MALE_MORTALITY:DECAY_SPEED) %>% 
  group_by(name) %>% 
  summarise(min = round(min(value),digits=2),
            max = round(max(value),digits=2),
            mean = round(mean(value),digits=2),
            sd = round(sd(value),digits=2)) %>% mutate(type="age standardized") %>% 
  mutate(name = factor(name, 
                       levels=c("BETA_MALE_MORTALITY","BETA_FEMALE_MORTALITY",
                                "BETA_FORMER_DRINKERS_MEN","BETA_FORMER_DRINKERS_WOMEN",
                                "METABOLIC_BETA1_MALE","METABOLIC_BETA2_MALE",
                                "METABOLIC_BETA1_FEMALE","METABOLIC_BETA2_FEMALE",
                                "BETA_HEPATITIS","THRESHOLD","THRESHOLD_MODIFIER",
                                "DECAY_SPEED","IRR_correlation"))) %>% 
  arrange(name) %>% 
  mutate(summarystat = ifelse(name=="THRESHOLD" | name=="THRESHOLD_MODIFIER" | name=="DECAY_SPEED",
                              paste0(min, "-", max), paste0(sd))) %>% 
  dplyr::select(name, mean, summarystat) %>% mutate(type="age standardized")

toplhs_forriskfunctions = rbind(toplhsagesp, toplhsagest) %>% 
  filter(name=="BETA_MALE_MORTALITY" | name =="BETA_FEMALE_MORTALITY" 
         | name=="METABOLIC_BETA1_MALE" | name=="METABOLIC_BETA2_MALE" |
           name=="METABOLIC_BETA1_FEMALE" | name=="METABOLIC_BETA2_FEMALE" |
           name=="BETA_HEPATITIS")
write.csv(toplhs_forriskfunctions, 
          "SIMAH_workplace/microsim/2_output_data/publication/riskfunctions.csv", row.names=F)


toplhs <- rbind(toplhsagesp, toplhsagest) %>% 
  group_by(name,type) %>% 
  pivot_wider(names_from=type, values_from=c(mean, summarystat)) %>% 
  dplyr::select(name, `mean_age specific`, `summarystat_age specific`,
                `mean_age standardized`, `summarystat_age standardized`)
write.csv(toplhs, "SIMAH_workplace/microsim/2_output_data/publication/CalParameterTable.csv", row.names=F)

toplhsagesp <- read.csv("SIMAH_workplace/microsim/2_output_data/lhsSamples_wave15_agesp.csv") %>% 
  pivot_longer(BETA_MALE_MORTALITY:DECAY_SPEED) %>% 
  group_by(name) %>% 
  summarise(min = round(min(value),digits=5),
            max = round(max(value),digits=5),
            mean = round(mean(value),digits=5),
            sd = round(sd(value),digits=5)) %>% mutate(type="age specific") %>% 
  mutate(name = factor(name, 
                       levels=c("BETA_MALE_MORTALITY","BETA_FEMALE_MORTALITY",
                                "BETA_FORMER_DRINKERS_MEN","BETA_FORMER_DRINKERS_WOMEN",
                                "METABOLIC_BETA1_MALE","METABOLIC_BETA2_MALE",
                                "METABOLIC_BETA1_FEMALE","METABOLIC_BETA2_FEMALE",
                                "BETA_HEPATITIS","THRESHOLD","THRESHOLD_MODIFIER",
                                "DECAY_SPEED","IRR_correlation"))) %>% 
  arrange(name) %>% 
  mutate(summarystat = ifelse(name=="THRESHOLD" | name=="THRESHOLD_MODIFIER" | name=="DECAY_SPEED",
                              paste0(min, "-", max), paste0(sd))) %>% 
  dplyr::select(name, mean, summarystat) %>% mutate(type="age specific")

toplhsagest <-  read.csv("SIMAH_workplace/microsim/2_output_data/lhsSamples_wave15_agest.csv") %>% 
  pivot_longer(BETA_MALE_MORTALITY:DECAY_SPEED) %>% 
  group_by(name) %>% 
  summarise(min = round(min(value),digits=5),
            max = round(max(value),digits=5),
            mean = round(mean(value),digits=5),
            sd = round(sd(value),digits=5)) %>% mutate(type="age standardized") %>% 
  mutate(name = factor(name, 
                       levels=c("BETA_MALE_MORTALITY","BETA_FEMALE_MORTALITY",
                                "BETA_FORMER_DRINKERS_MEN","BETA_FORMER_DRINKERS_WOMEN",
                                "METABOLIC_BETA1_MALE","METABOLIC_BETA2_MALE",
                                "METABOLIC_BETA1_FEMALE","METABOLIC_BETA2_FEMALE",
                                "BETA_HEPATITIS","THRESHOLD","THRESHOLD_MODIFIER",
                                "DECAY_SPEED","IRR_correlation"))) %>% 
  arrange(name) %>% 
  mutate(summarystat = ifelse(name=="THRESHOLD" | name=="THRESHOLD_MODIFIER" | name=="DECAY_SPEED",
                              paste0(min, "-", max), paste0(sd))) %>% 
  dplyr::select(name, mean, summarystat) %>% mutate(type="age standardized")

toplhs_forriskfunctions = rbind(toplhsagesp, toplhsagest) %>% 
  filter(name=="BETA_MALE_MORTALITY" | name =="BETA_FEMALE_MORTALITY" 
         | name=="METABOLIC_BETA1_MALE" | name=="METABOLIC_BETA2_MALE" |
           name=="METABOLIC_BETA1_FEMALE" | name=="METABOLIC_BETA2_FEMALE" |
           name=="BETA_HEPATITIS")
write.csv(toplhs_forriskfunctions, 
          "SIMAH_workplace/microsim/2_output_data/publication/riskfunctions.csv", row.names=F)





if(PE==0){
normalise <- function(data,parameter){
  data <- data[,parameter]
  data <- data.frame(min=min(data), max=max(data), raw=data,
                     scaled = ((data - min(data)) + 10e-10) / ((max(data)-min(data)) + 10e-9))
  data$unscaled = ((data$max - data$min + 10e-10)*data$scaled) + (data$min - 10e-9)
  
  beta <- fitdist(data$scaled, "beta", method="mge")
  data$shape1 <- as.numeric(beta$estimate[1])
  data$shape2 <- as.numeric(beta$estimate[2])
  names(data) <- paste(names(data),parameter,sep="_")
  return(data)
}

toplhs <- cbind(toplhs, normalise(toplhs, "BETA_MALE_MORTALITY"), normalise(toplhs, "BETA_FEMALE_MORTALITY"),
                normalise(toplhs, "BETA_FORMER_DRINKERS_MEN"), normalise(toplhs, "BETA_FORMER_DRINKERS_WOMEN"),
                normalise(toplhs, "METABOLIC_BETA1_MALE"),
                normalise(toplhs, "METABOLIC_BETA2_MALE"), normalise(toplhs, "METABOLIC_BETA1_FEMALE"),
                normalise(toplhs, "METABOLIC_BETA2_FEMALE"), normalise(toplhs, "BETA_HEPATITIS"),
                normalise(toplhs, "THRESHOLD"), normalise(toplhs, "THRESHOLD_MODIFIER"),
                normalise(toplhs, "IRR_correlation"), normalise(toplhs, "DECAY_SPEED"))

prior <- list(c("qbeta", toplhs$shape1_BETA_MALE_MORTALITY, toplhs$shape2_BETA_MALE_MORTALITY), #BETA_MALE_MORTALITY
              c("qbeta", toplhs$shape1_BETA_FEMALE_MORTALITY, toplhs$shape2_BETA_FEMALE_MORTALITY), #BETA_FEMALE_MORTALITY
              c("qbeta", toplhs$shape1_BETA_FORMER_DRINKERS_MEN, toplhs$shape2_BETA_FORMER_DRINKERS_MEN), #BETA_FORMER_DRINKERS_MEN
              c("qbeta", toplhs$shape1_BETA_FORMER_DRINKERS_WOMEN, toplhs$shape2_BETA_FORMER_DRINKERS_WOMEN), #BETA_FORMER_DRINKERS_WOMEN
              c("qbeta", toplhs$shape1_METABOLIC_BETA1_MALE, toplhs$shape2_METABOLIC_BETA1_MALE), #METABOLIC_BETA1_MALE
              c("qbeta", toplhs$shape1_METABOLIC_BETA2_MALE, toplhs$shape2_METABOLIC_BETA2_MALE), #METABOLIC_BETA2_MALE
              c("qbeta", toplhs$shape1_METABOLIC_BETA1_FEMALE, toplhs$shape2_METABOLIC_BETA1_FEMALE), #METABOLIC_BETA1_FEMALE
              c("qbeta", toplhs$shape1_METABOLIC_BETA2_FEMALE, toplhs$shape2_METABOLIC_BETA2_FEMALE), #METABOLIC_BETA2_FEMALE
              c("qbeta", toplhs$shape1_BETA_HEPATITIS, toplhs$shape2_BETA_HEPATITIS), #BETA_HEPATITIS
              c("qbeta", toplhs$shape1_THRESHOLD, toplhs$shape2_THRESHOLD), #THRESHOLD
              c("qbeta", toplhs$shape1_THRESHOLD_MODIFIER, toplhs$shape2_THRESHOLD_MODIFIER), #THRESHOLD MODIFIER
              c("qbeta", toplhs$shape1_IRR_correlation, toplhs$shape2_IRR_correlation), #IRR CORRELATION
              c("qbeta", toplhs$shape1_DECAY_SPEED, toplhs$shape2_DECAY_SPEED)) #DECAY SPEED

N_PRIORS <- length(prior)
set.seed(as.numeric(Sys.time()))
lhsSampleUniforms <- maximinLHS(N_SAMPLES, N_PRIORS)
lhsSample <- matrix(nrow = N_SAMPLES, ncol = N_PRIORS)

for(i in 1:N_PRIORS) {
  lhsSample[, i] <- eval(call(prior[[i]][1], lhsSampleUniforms[, i], 
                              as.numeric(prior[[i]][2]), as.numeric(prior[[i]][3])))
}

lhsSample <- as.data.frame(lhsSample)
SampleNum <- c(1:N_SAMPLES)
lhsSample <- cbind(SampleNum, lhsSample)

names(lhsSample) <- names(toplhs)[1:ncol(lhsSample)]

# now convert all of the beta distributions back to the original scale
denormalise <- function(scaledvalue, min, max){
  unscaled = ((max - min + 10e-10)*scaledvalue) + (min - 10e-9)
  return(unscaled)
}

priors <- names(lhsSample[2:ncol(lhsSample)])
new <- list()
for(i in priors){
  scaled <- lhsSample[,i]
  min <- unique(toplhs[,paste("min",i,sep="_")])
  max <- unique(toplhs[,paste("max",i,sep="_")])
  new[[paste(i)]] <- denormalise(scaled,min,max)
}

priors <- data.frame(do.call(cbind,new))
lhsSample <- cbind(SampleNum, priors)

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
