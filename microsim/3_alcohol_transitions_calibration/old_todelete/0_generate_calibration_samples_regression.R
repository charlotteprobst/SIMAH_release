# read in the NESARC regression model 
alcohol_transitions <- read_csv("SIMAH_workplace/nesarc/Models/multinom_model.csv")
ses <- read_csv("SIMAH_workplace/nesarc/Models/multinom_model_ses.csv")



mean_zero <- alcohol_transitions %>% filter(type=="abstainermod") %>% 
  dplyr::select(parameter, Estimate) %>% pivot_wider(names_from=parameter, values_from=Estimate) %>% as.numeric(.)

se_zero <- alcohol_transitions %>% filter(type=="abstainermod") %>% 
  dplyr::select(parameter, Std..Error) %>% pivot_wider(names_from=parameter, values_from=Std..Error) %>% as.numeric(.)

mean_count <- alcohol_transitions %>% filter(type=="drinkermod") %>% 
  dplyr::select(parameter, Estimate) %>% pivot_wider(names_from=parameter, values_from=Estimate) %>% as.numeric(.)

se_count <- alcohol_transitions %>% filter(type=="drinkermod") %>% 
  dplyr::select(parameter, Std..Error) %>% pivot_wider(names_from=parameter, values_from=Std..Error) %>% as.numeric(.)

# Combine means and standard errors
means <- c(mean_zero, mean_count)
ses <- c(se_zero, se_count)
ses <- ses*10

# Generate Latin Hypercube Sampling
generate_lhs <- function(means, ses, nsamples) {
  num_parameters <- length(means)
  lhs_sample <- randomLHS(nsamples, num_parameters)
  scaled_sample <- matrix(nrow = nsamples, ncol = num_parameters)
  
  for (i in 1:num_parameters) {
    scaled_sample[, i] <- lhs_sample[, i] * ses[i] + means[i]
  }
  
  return(scaled_sample)
}

# Generate samples
samples <- generate_lhs(means, ses, nsamples)

# Output samples
samples <- data.frame(samples)

colnames(samples) <- c(subset(alcohol_transitions, type=="abstainermod")$parameter,
                       subset(alcohol_transitions, type=="drinkermod")$parameter)
samples$sample <- 1:nrow(samples)

first <- subset(alcohol_transitions, type=="abstainermod")$parameter[1]
last <- tail(subset(alcohol_transitions, type=="drinkermod")$parameter, n=1)

lhs <- samples %>% 
  pivot_longer(cols=first:last)

lengthabstainer <- length(subset(alcohol_transitions, type=="abstainermod")$parameter)
lengthdrinker <- length(subset(alcohol_transitions, type=="drinkermod")$parameter)


lhs <- lhs %>% group_by(sample) %>% 
  mutate(type=c(rep("abstainermod",lengthabstainer), 
                rep("drinkermod", lengthdrinker))) %>% 
  rename(parameter=name, Estimate=value)

lhs <- lhs %>% 
  mutate(parameter = ifelse(parameter=="X.Intercept.", "(Intercept)",
                            ifelse(parameter=="race.factor_2Black..non.Hispanic","race.factor_2Black, non-Hispanic",
                                   ifelse(parameter=="race.factor_2Other..non.Hispanic","race.factor_2Other, non-Hispanic", 
                                          ifelse(parameter=="age3_225.64", "age3_225-64", 
                                                 ifelse(parameter=="age3_265.", "age3_265+", 
                                                        ifelse(parameter=="alc_daily_g_1.cat2_lag", "alc_daily_g_1:cat2_lag", 
                                                               ifelse(parameter=="alc_daily_g_1.cat3_lag", "alc_daily_g_1:cat3_lag",
                                                                      parameter))))))))

# save samples - for wave 1 in Output Directory
write.csv(lhs, paste0(OutputDirectory, "/lhs_regression-1",".csv"))


transitionsList <- list()
for(i in unique(lhs$sample)){
  transitionsList[[paste(i)]] <- lhs %>% filter(sample==i) %>% 
    ungroup() %>% 
    dplyr::select(-sample)
}

