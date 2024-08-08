# read in the NESARC regression model 
library(stringr)
alcohol_transitions <- read_csv("SIMAH_workplace/nesarc/Models/multinom_model_race_cont.csv")
ses <- read_csv("SIMAH_workplace/nesarc/Models/multinom_model_ses_race_cont.csv")

means <- alcohol_transitions %>% pivot_longer(X.Intercept.:female.factor_2Women.race.factor_2Other..non.Hispanic) %>% 
  mutate(name=paste(name, cat, sep="_")) %>% dplyr::select(-cat) %>% 
  pivot_wider(names_from=name, values_from=value)

ses <- ses %>% pivot_longer(X.Intercept.:female.factor_2Women.race.factor_2Other..non.Hispanic) %>% 
  mutate(name=paste(name, cat, sep="_")) %>% dplyr::select(-cat) %>% 
  pivot_wider(names_from=name, values_from=value)

# check order is the same 
colnames(means)==colnames(ses)

# save a copy of the names 
names <- colnames(means)

means <- as.numeric(means)
ses <- as.numeric(ses)

# Generate Latin Hypercube Sampling
# Generate Latin Hypercube Sampling
generate_lhs <- function(means, ses, nsamples) {
  num_parameters <- length(means)
  lhs_sample <- randomLHS(nsamples, num_parameters)
  scaled_sample <- matrix(nrow = nsamples, ncol = num_parameters)
  
  for (i in 1:num_parameters) {
    scaled_sample[, i] <- qnorm(lhs_sample[, i], mean = means[i], sd = ses[i])
  }
  
  return(scaled_sample)
}

# Generate samples
samples <- generate_lhs(means, ses, nsamples)

# Output samples
samples <- data.frame(samples)

colnames(samples) <- names

samples$sample <- 1:nrow(samples)

first <- names[1]
last <- tail(names, n=1)

lhs <- samples %>% 
  pivot_longer(first:last) %>% 
  mutate(cat = case_when(
    str_detect(name, "High risk") ~ "High risk",
    str_detect(name, "Medium risk") ~ "Medium risk",
    str_detect(name, "Low risk") ~ "Low risk"),
    name = gsub("_High risk", "", name),
    name = gsub("_Medium risk", "", name),
    name = gsub("_Low risk", "", name)) %>% pivot_wider(names_from=name, values_from=value)

# save samples - for wave 1 in Output Directory
write.csv(lhs, paste0(OutputDirectory, "/lhs_regression-1",".csv"))

transitionsList <- list()
for(i in unique(lhs$sample)){
  transitionsList[[paste(i)]] <- lhs %>% filter(sample==i) %>% 
    ungroup() %>% 
    dplyr::select(-sample)
}

