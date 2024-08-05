library(stringr)
distribution <- read.csv("SIMAH_workplace/microsim/1_input_data/CatContDistr_beta.csv") %>%
  dplyr::select(group, shape1, shape2, min, max)

# # groups to calibrate 
groups <- read.csv(paste0(OutputDirectory, "/groupstocalibrate.csv")) %>%
  mutate(sex = ifelse(microsim.init.sex=="f","Female","Male"),
         group = paste(microsim.init.education, agecat, sex, sep="_"))
groups <- unique(groups$group)

# filter distribution based on matching
distributiontosample <- distribution %>% filter(str_detect(group, paste(groups, collapse="|")))

means <- distributiontosample %>% dplyr::select(group, shape1, shape2) %>% 
  pivot_longer(shape1:shape2) %>% 
  mutate(groupname = paste(group, name, sep="_")) %>% 
  dplyr::select(groupname, value)

ses <- means %>% 
  mutate(se=value*0.1) %>% 
  dplyr::select(groupname, se) %>% 
  pivot_wider(names_from=groupname, values_from=se)

means <- means %>% pivot_wider(names_from=groupname, values_from=value)

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
  rename(Value = value) %>% 
  mutate(parameter = case_when(
    str_detect(name, "shape1") ~ "shape1",
    str_detect(name, "shape2") ~ "shape2"
  ),
  name = gsub("_shape1", "", name),
  name = gsub("_shape2", "", name)) %>% 
  pivot_wider(names_from=parameter, values_from=Value) %>% 
  rename(group=name)

distributiontojoin <- distribution %>% filter(!str_detect(group, paste(groups, collapse="|")))

replicated_distribution <- bind_rows(replicate(nsamples, distributiontojoin, simplify = FALSE))

replicated_distribution <- replicated_distribution %>%
  mutate(sample = rep(1:nsamples, each = nrow(distributiontojoin))) %>%
  dplyr::select(sample, group, shape1, shape2)

lhs <- rbind(lhs, replicated_distribution)

distribution <- distribution %>% dplyr::select(group, min, max)

lhs <- left_join(lhs, distribution, by=c("group"))

# save samples - for wave 1 in Output Directory
write.csv(lhs, paste0(OutputDirectory, "/lhs_regression-1",".csv"))

transitionsList <- list()
for(i in unique(lhs$sample)){
  transitionsList[[paste(i)]] <- lhs %>% filter(sample==i) %>% 
    ungroup() %>% 
    dplyr::select(-sample)
}

