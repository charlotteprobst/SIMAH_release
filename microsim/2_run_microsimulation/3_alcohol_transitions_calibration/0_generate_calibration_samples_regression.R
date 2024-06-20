# read in the NESARC regression model 
alcohol_transitions <- read_csv("SIMAH_workplace/nesarc/Models/regression_NESARC_GLMM.csv")

mean_zero <- alcohol_transitions %>% filter(estimate=="PE" & type=="zero") %>% 
  dplyr::select(-c(type,estimate)) %>% as.numeric(.)

se_zero <- alcohol_transitions %>% filter(estimate=="SE" & type=="zero") %>% 
  dplyr::select(-c(type,estimate)) %>% as.numeric(.)

mean_count <- alcohol_transitions %>% filter(estimate=="PE" & type=="count") %>% 
  dplyr::select(-c(type,estimate)) %>% as.numeric(.)

se_count <- alcohol_transitions %>% filter(estimate=="SE" & type=="count") %>% 
  dplyr::select(-c(type,estimate)) %>% as.numeric(.)


generate_lhs <- function(means, ses, num_samples) {
  num_parameters <- length(means)
  lhs_sample <- randomLHS(num_samples, num_parameters)
  scaled_sample <- sweep(lhs_sample, 2, ses, `*`) + sweep(lhs_sample, 2, means, `+`)
  return(scaled_sample)
}

lhs_zero <- generate_lhs(mean_zero, se_zero, nsamples)
colnames(lhs_zero) <- colnames(alcohol_transitions)[-c(1:2)]
lhs_zero <- data.frame(type="zero",sample=1:nrow(lhs_zero), lhs_zero)

lhs_count <- generate_lhs(mean_count, se_count, nsamples)
colnames(lhs_count) <- colnames(alcohol_transitions)[-c(1:2)]
lhs_count <- data.frame(type="count",sample=1:nrow(lhs_count), lhs_count)

lhs <- rbind(lhs_zero, lhs_count)

# save samples - for wave 1 in Output Directory
write.csv(lhs, paste0(OutputDirectory, "/lhs_regression-1",".csv"))
